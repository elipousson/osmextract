# Implementation of oe_export(engine = "osmium"). The .osm.pbf file is read
# directly by DuckDB using the (community) osmium extension, which builds the
# geometries with libosmium, so there is no intermediate .gpkg file. The layers
# are defined with SQL rules that reproduce the ones in osmconf.ini.

oe_export_osmium = function(
  file_path,
  layer,
  format,
  extra_tags,
  boundary,
  boundary_type,
  force_export,
  quiet
) {
  if (!tools::file_ext(file_path) %in% c("pbf", "osm")) {
    oe_stop(
      .subclass = "oe_export-osmiumRequiresPbf",
      message = "engine = 'osmium' reads .osm.pbf files directly, so file_path must be a .pbf or .osm.pbf file"
    )
  }
  if (any(layer %!in% c("points", "lines", "multipolygons"))) {
    oe_stop(
      .subclass = "oe_export-osmiumLayerNotSupported",
      message = paste0(
        "engine = 'osmium' supports only the layers points, lines, and ",
        "multipolygons (it does not build geometries for relations other ",
        "than multipolygons and boundaries)."
      )
    )
  }
  if (!is.null(extra_tags)) {
    warning(
      "The argument extra_tags is ignored when engine = 'osmium': all tags are ",
      "stored in the 'tags' column.",
      call. = FALSE
    )
  }

  # Process the boundary into a WKT (rectangle for "spat", otherwise the
  # polygon itself).
  boundary_wkt = NULL
  clip = FALSE
  if (!is.null(boundary) && !anyNA(boundary)) {
    boundary_type = match.arg(boundary_type, c("spat", "clipsrc", "clipdst"))
    # The output is always in EPSG:4326, so "clipdst" does not need to keep the
    # CRS of the boundary and it is handled as "clipsrc".
    boundary = prepare_boundary(boundary, "clipsrc")
    clip = boundary_type != "spat"
    boundary_wkt = sf::st_as_text(
      if (clip) boundary else sf::st_as_sfc(sf::st_bbox(boundary))
    )
  }
  if (!is.null(boundary)) {
    force_export = TRUE
  }

  base_path = if (tools::file_ext(tools::file_path_sans_ext(file_path)) == "osm") {
    tools::file_path_sans_ext(tools::file_path_sans_ext(file_path))
  } else {
    tools::file_path_sans_ext(file_path)
  }
  # The engine is part of the name so that the outputs cannot be mixed up with
  # the ones created using engine = "gdal" (which have a different schema).
  output_paths = switch(
    format,
    parquet = stats::setNames(
      paste0(base_path, "_osmium_", layer, ".parquet"),
      layer
    ),
    duckdb = paste0(base_path, "_osmium.duckdb")
  )

  if (
    format == "parquet" &&
    all(file.exists(output_paths)) &&
    isFALSE(force_export)
  ) {
    oe_message(
      "All the parquet files were already detected. Skip export operations.",
      quiet = quiet,
      .subclass = "oe_export_skipOperations"
    )
    return(output_paths)
  }

  con = connect_osmium(
    dbdir = if (format == "duckdb") output_paths else ":memory:"
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  create_osmium_macros(con)

  pbf_literal = DBI::dbQuoteString(con, normalizePath(file_path))
  boundary_sql = if (!is.null(boundary_wkt)) {
    paste0(
      "ST_SetCRS(ST_GeomFromText(", DBI::dbQuoteString(con, boundary_wkt),
      "), 'EPSG:4326')"
    )
  }

  for (lyr in layer) {
    if (format == "parquet") {
      out = output_paths[[lyr]]
      if (file.exists(out) && isFALSE(force_export)) {
        oe_message(
          "The corresponding parquet file for the layer ", lyr,
          " was already detected. Skip export operations.",
          quiet = quiet,
          .subclass = "oe_export_skipOperations"
        )
        next
      }
    } else if (DBI::dbExistsTable(con, lyr) && isFALSE(force_export)) {
      oe_message(
        "The table for the layer ", lyr,
        " already exists in the duckdb file. Skip export operations.",
        quiet = quiet,
        .subclass = "oe_export_skipOperations"
      )
      next
    }

    oe_message(
      "Reading the layer ", lyr, " from the .osm.pbf file.",
      quiet = quiet,
      .subclass = "oe_export_startOsmium"
    )

    # The layer is staged in a temporary table so that features whose geometry
    # could not be built (NULL) can be counted before they are dropped. The
    # geometry gets the same CRS label used by the GDAL engine (the extension
    # uses OGC:CRS84, which DuckDB considers different from EPSG:4326).
    where = osmium_layer_rule(lyr)
    if (!is.null(boundary_sql)) {
      where = paste0(
        "(", where, ") AND ST_Intersects(ST_SetCRS(geometry, 'EPSG:4326'), ",
        boundary_sql, ")"
      )
    }
    DBI::dbExecute(
      con,
      paste0(
        "CREATE OR REPLACE TEMPORARY TABLE staging AS SELECT type, id, tags, ",
        "ST_SetCRS(geometry, 'EPSG:4326') AS geometry FROM osmium_read(",
        pbf_literal, ") WHERE ", where
      )
    )

    counts = DBI::dbGetQuery(
      con,
      "SELECT count(*) AS n, count(geometry) AS n_geometry FROM staging"
    )
    n_null = as.numeric(counts[["n"]]) - as.numeric(counts[["n_geometry"]])
    if (n_null > 0) {
      oe_message(
        "Dropped ", n_null, " feature(s) of the layer ", lyr,
        " because their geometry could not be built.",
        quiet = quiet,
        .subclass = "oe_export_nullGeometryDropped"
      )
    }

    final_sql = if (clip) {
      paste0(
        "SELECT * FROM (SELECT type, id, tags, ST_Intersection(geometry, ",
        boundary_sql, ") AS geometry FROM staging WHERE geometry IS NOT NULL) ",
        "WHERE NOT ST_IsEmpty(geometry)"
      )
    } else {
      "SELECT * FROM staging WHERE geometry IS NOT NULL"
    }

    if (format == "parquet") {
      tmp = paste0(out, ".tmp")
      on.exit(unlink(tmp), add = TRUE)
      DBI::dbExecute(
        con,
        paste0(
          "COPY (", final_sql, ") TO ", DBI::dbQuoteString(con, tmp),
          " (FORMAT parquet, COMPRESSION zstd)"
        )
      )
      file.rename(tmp, out)
    } else {
      DBI::dbExecute(
        con,
        paste0(
          "CREATE OR REPLACE TABLE ", DBI::dbQuoteIdentifier(con, lyr),
          " AS ", final_sql
        )
      )
    }
    DBI::dbExecute(con, "DROP TABLE staging")

    oe_message(
      "Exported the layer ", lyr, " to ", format, " format.",
      quiet = quiet,
      .subclass = "oe_export_finishedExport"
    )
  }

  output_paths
}

# SQL rules reproducing how GDAL (see osmconf.ini) assigns OSM elements to
# layers. The osmium extension returns the kinds node, line, area (closed ways
# and multipolygon/boundary relations) and relation. Closed ways without an
# area=yes tag are returned twice, as line and as area.
osmium_layer_rule = function(layer) {
  switch(
    layer,
    # Nodes with at least one significant tag
    points = "kind = 'node' AND osm_significant(tags)",
    # Ways with at least one tag, except closed ways that GDAL considers areas
    lines = "kind = 'line' AND cardinality(tags) > 0 AND NOT (ST_IsClosed(geometry) AND osm_is_area(tags))",
    # Areas with at least one significant tag: multipolygon/boundary relations
    # and closed ways with an "area" key.
    multipolygons = "kind = 'area' AND osm_significant(tags) AND (type = 'relation' OR osm_is_area(tags))"
  )
}

create_osmium_macros = function(con) {
  # Keys listed in the "unsignificant" setting of osmconf.ini. Note that the
  # "ignore" setting only hides tags from the output and it is not used to
  # decide which features are reported.
  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE TEMPORARY MACRO osm_significant(t) AS ",
      "len(list_filter(map_keys(t), lambda k: NOT (k IN ",
      "('created_by', 'converted_by', 'source', 'time', 'ele', 'attribution')))) > 0"
    )
  )
  # Closed ways are polygons if they have one of the keys listed in the
  # "closed_ways_are_polygons" setting of osmconf.ini (or area=yes), unless
  # they are tagged area=no.
  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE TEMPORARY MACRO osm_is_area(t) AS ",
      "coalesce((list_has_any(map_keys(t), ['aeroway', 'amenity', 'boundary', ",
      "'building', 'craft', 'geological', 'historic', 'landuse', 'leisure', ",
      "'military', 'natural', 'office', 'place', 'shop', 'sport', 'tourism']) ",
      "OR t['highway'] = 'platform' OR t['public_transport'] = 'platform' ",
      "OR t['area'] = 'yes') AND coalesce(t['area'], '') <> 'no', false)"
    )
  )
  invisible(TRUE)
}

# Open a DuckDB connection with the spatial and osmium extensions loaded. The
# osmium extension is first loaded as a regular (signed) extension. If that
# fails, we try to load an unsigned build (e.g. one compiled locally for the
# DuckDB version used by the duckdb R package) and, at last, to install it from
# the community repository.
connect_osmium = function(dbdir = ":memory:") {
  attempt = function(unsigned, install = FALSE) {
    con = DBI::dbConnect(
      duckdb::duckdb(
        dbdir = dbdir,
        config = if (unsigned) list(allow_unsigned_extensions = "true") else list()
      )
    )
    ok = tryCatch(
      {
        DBI::dbExecute(con, "INSTALL spatial")
        DBI::dbExecute(con, "LOAD spatial")
        if (install) {
          DBI::dbExecute(con, "INSTALL osmium FROM community")
        }
        DBI::dbExecute(con, "LOAD osmium")
        TRUE
      },
      error = function(cnd) FALSE
    )
    if (!ok) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      return(NULL)
    }
    con
  }

  con = attempt(unsigned = FALSE)
  if (is.null(con)) con = attempt(unsigned = TRUE)
  if (is.null(con)) con = attempt(unsigned = FALSE, install = TRUE)
  if (is.null(con)) {
    oe_stop(
      .subclass = "oe_export-osmiumUnavailable",
      message = paste0(
        "Cannot load the DuckDB 'osmium' extension. Install it with ",
        "INSTALL osmium FROM community (it must be available for the DuckDB ",
        "version used by the duckdb R package) or load a local build."
      )
    )
  }
  con
}
