#' Export an OSM extract to Parquet or DuckDB format
#'
#' This function exports the layers of an OSM extract to
#' [Parquet](https://parquet.apache.org/) files or to a
#' [DuckDB](https://duckdb.org/) database file. The conversion is performed by
#' DuckDB using its
#' [spatial extension](https://duckdb.org/docs/stable/core_extensions/spatial/overview).
#' Use [oe_import()] to read the exported data back into R.
#'
#' @details The input can be either a `.gpkg` file created by
#'   [oe_vectortranslate()] or a `.osm.pbf` file. In the second case, the
#'   function runs [oe_vectortranslate()] first (using the arguments
#'   `extra_tags`, `boundary`, and `boundary_type`) and exports the resulting
#'   `.gpkg` file. DuckDB does not read `.osm.pbf` files directly because its
#'   GDAL OSM driver can silently drop features when it reads a single layer
#'   from a large file, so GDAL is always used for the `.osm.pbf` to `.gpkg`
#'   step.
#'
#'   The output files are created in the same directory as the input file:
#'   * `format = "parquet"` creates one file for each layer, named
#'   `<input name>_<layer>.parquet`;
#'   * `format = "duckdb"` creates a single `<input name>.duckdb` file with one
#'   table for each layer.
#'
#'   The geometry column keeps its name and CRS. Parquet files are compressed
#'   with zstd. The export is skipped if the output already exists, unless
#'   `force_export = TRUE`. It is never skipped when `extra_tags` or `boundary`
#'   are not `NULL`.
#'
#'   This function requires the suggested packages `duckdb` and `DBI`. The
#'   first time it runs, DuckDB downloads its spatial extension, so an internet
#'   connection is needed.
#'
#'   ## Experimental: `engine = "osmium"`
#'
#'   With `engine = "osmium"` the `.osm.pbf` file is read directly by DuckDB
#'   using the [osmium](https://duckdb.org/community_extensions/extensions/osmium)
#'   community extension (which builds geometries with libosmium), so no
#'   `.gpkg` file is created. On a state-sized extract (Maryland, 216 MB) it was
#'   about 3 times faster than GDAL, but it needed much more memory: about 1.1
#'   to 1.5 GB for one layer and up to 2.4 GB for three layers in a single
#'   call, against less than 0.5 GB for GDAL. Exporting one layer at a time
#'   reduces the peak memory. The extension must be available for the DuckDB
#'   version used by the `duckdb` package (a local, unsigned build is also
#'   accepted).
#'
#'   The layers are defined with SQL rules that reproduce those in
#'   `osmconf.ini`. In our tests the `points` and `lines` layers contained the
#'   same features as the ones created by GDAL, while `multipolygons` differed
#'   in about 0.06% of the features. Other differences are:
#'   * only the layers `points`, `lines` and `multipolygons` are available;
#'   * the output has the columns `type` (`node`, `way` or `relation`), `id`,
#'   `geometry`, and `tags`, a `MAP` column with *all* the tags (instead of
#'   one column for each tag and `other_tags`). The argument `extra_tags` is
#'   ignored;
#'   * simple polygons are `POLYGON`s instead of `MULTIPOLYGON`s;
#'   * features whose geometry could not be built are dropped, and the
#'   function reports how many;
#'   * `boundary` is applied after reading the file (so it does not speed up the
#'   process), keeping the features that intersect the boundary (or its
#'   bounding box if `boundary_type = "spat"`). `"clipdst"` is treated as
#'   `"clipsrc"` since the output always uses EPSG:4326;
#'   * the output files are named `<input name>_osmium_<layer>.parquet` or
#'   `<input name>_osmium.duckdb`.
#'
#' @inheritParams oe_vectortranslate
#' @param file_path Character string representing the path of the input
#'   `.gpkg` or `.osm.pbf` file. With `engine = "osmium"` it must be a
#'   `.osm.pbf` file.
#' @param layer Character vector of layers to export. Must be some of `points`,
#'   `lines`, `multipolygons`, `multilinestrings`, and `other_relations`
#'   (only the first three with `engine = "osmium"`).
#' @param format Output format. Either `"parquet"` or `"duckdb"`.
#' @param engine Either `"gdal"` (the default, via an intermediate `.gpkg`
#'   file) or `"osmium"` (experimental, see Details).
#' @param force_export Logical. If `TRUE`, existing outputs are overwritten.
#'
#' @return Character vector with the path of the exported files. For
#'   `format = "parquet"` the vector has one element for each `layer` (named
#'   after the layers); for `format = "duckdb"` it is a single path.
#' @export
#'
#' @seealso [oe_import()], [oe_vectortranslate()]
#'
#' @examples
#' \dontrun{
#' file.copy(
#'   from = system.file("its-example.osm.pbf", package = "osmextract"),
#'   to = file.path(tempdir(), "test_its-example.osm.pbf"),
#'   overwrite = TRUE
#' )
#' its_pbf = file.path(tempdir(), "test_its-example.osm.pbf")
#'
#' its_parquet = oe_export(its_pbf, layer = c("lines", "points"), quiet = TRUE)
#' its_parquet
#' oe_import(its_parquet[["lines"]])
#'
#' its_duckdb = oe_export(its_pbf, format = "duckdb", quiet = TRUE)
#' oe_import(its_duckdb, layer = "lines")
#'
#' oe_clean(tempdir())
#' }
oe_export = function(
  file_path,
  layer = "lines",
  format = c("parquet", "duckdb"),
  engine = c("gdal", "osmium"),
  extra_tags = NULL,
  boundary = NULL,
  boundary_type = c("spat", "clipsrc", "clipdst"),
  force_export = FALSE,
  quiet = FALSE
) {
  check_duckdb_installed()
  format = match.arg(format)
  engine = match.arg(engine)

  if (!is.character(file_path) || length(file_path) != 1L || !file.exists(file_path)) {
    oe_stop(
      .subclass = "oe_export-filePathMissing",
      message = "The parameter file_path must correspond to an existing .gpkg or .pbf file"
    )
  }
  layer = unique(tolower(layer))
  if (
    !is.character(layer) ||
    !length(layer) ||
    anyNA(layer) ||
    any(layer %!in% oe_layer_names())
  ) {
    oe_stop(
      .subclass = "oe_export-layerNotProperlySpecified",
      message = paste0(
        "The layer parameter must be one or more of ",
        paste(oe_layer_names(), collapse = ", "), "."
      )
    )
  }

  if (engine == "osmium") {
    return(oe_export_osmium(
      file_path = file_path,
      layer = layer,
      format = format,
      extra_tags = extra_tags,
      boundary = boundary,
      boundary_type = boundary_type,
      force_export = force_export,
      quiet = quiet
    ))
  }

  # Extra tags or a boundary change the content of the .gpkg file, so any
  # existing output could be out of date.
  if (!is.null(extra_tags) || !is.null(boundary)) {
    force_export = TRUE
  }

  gpkg_file_path = switch(
    tools::file_ext(file_path),
    gpkg = file_path,
    pbf = ,
    osm = {
      for (lyr in layer) {
        gpkg_file_path = oe_vectortranslate(
          file_path,
          layer = lyr,
          extra_tags = extra_tags,
          boundary = boundary,
          boundary_type = boundary_type,
          quiet = quiet
        )
      }
      gpkg_file_path
    },
    oe_stop(
      .subclass = "oe_export-fileExtensionNotSupported",
      message = "The parameter file_path must be a .gpkg, .osm.pbf, or .pbf file"
    )
  )

  missing_layers = setdiff(layer, sf::st_layers(gpkg_file_path)[["name"]])
  if (length(missing_layers)) {
    oe_stop(
      .subclass = "oe_export-layerNotInGpkg",
      message = paste0(
        "The .gpkg file does not contain the layer(s): ",
        paste(missing_layers, collapse = ", "), "."
      )
    )
  }

  base_path = tools::file_path_sans_ext(gpkg_file_path)
  output_paths = switch(
    format,
    parquet = stats::setNames(paste0(base_path, "_", layer, ".parquet"), layer),
    duckdb = paste0(base_path, ".duckdb")
  )

  # Connect to the output database (duckdb) or to an in-memory one (parquet)
  con = DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = if (format == "duckdb") output_paths else ":memory:"
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  load_spatial(con)

  gpkg_literal = DBI::dbQuoteString(con, normalizePath(gpkg_file_path))

  for (lyr in layer) {
    st_read_sql = paste0(
      "SELECT * FROM ST_Read(", gpkg_literal, ", layer = ",
      DBI::dbQuoteString(con, lyr), ")"
    )

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
      # Write to a temporary file so that a failed export never leaves a
      # partial file behind.
      tmp = paste0(out, ".tmp")
      on.exit(unlink(tmp), add = TRUE)
      DBI::dbExecute(
        con,
        paste0(
          "COPY (", st_read_sql, ") TO ", DBI::dbQuoteString(con, tmp),
          " (FORMAT parquet, COMPRESSION zstd)"
        )
      )
      file.rename(tmp, out)
    } else {
      if (
        DBI::dbExistsTable(con, lyr) &&
        isFALSE(force_export)
      ) {
        oe_message(
          "The table for the layer ", lyr,
          " already exists in the duckdb file. Skip export operations.",
          quiet = quiet,
          .subclass = "oe_export_skipOperations"
        )
        next
      }
      DBI::dbExecute(
        con,
        paste0(
          "CREATE OR REPLACE TABLE ", DBI::dbQuoteIdentifier(con, lyr),
          " AS ", st_read_sql
        )
      )
    }
    oe_message(
      "Exported the layer ", lyr, " to ", format, " format.",
      quiet = quiet,
      .subclass = "oe_export_finishedExport"
    )
  }

  output_paths
}

#' Read an exported Parquet or DuckDB file
#'
#' This function reads a file created by [oe_export()] into R as an `sf`
#' object. Optionally, the data can be filtered with a DuckDB SQL query before
#' it is loaded in memory, which is useful for large extracts.
#'
#' @details Parquet files (and tables in `.duckdb` files) are exposed to the
#'   SQL `query` as a table named `data`. For example, `query = "SELECT * FROM
#'   data WHERE highway = 'primary'"`. The query can use the functions of the
#'   [DuckDB spatial extension](https://duckdb.org/docs/stable/core_extensions/spatial/overview).
#'   The result is converted to an `sf` object using the first column with
#'   `GEOMETRY` type, otherwise a plain `data.frame` is returned. As in
#'   [oe_read()], the precision of the output is set equal to 1e7 when the CRS
#'   is `EPSG:4326` (see `options(osmextract.precision)`).
#'
#'   This function requires the suggested packages `duckdb` and `DBI`.
#'
#' @param file_path Character string representing the path of a `.parquet` or
#'   `.duckdb` file.
#' @param layer Name of the table to read from a `.duckdb` file. It can be
#'   `NULL` if the file contains a single table. Ignored for `.parquet` files.
#' @param query DuckDB SQL query. The default value, `NULL`, selects all rows.
#'
#' @return An `sf` object.
#' @export
#'
#' @seealso [oe_export()]
#'
#' @examples
#' \dontrun{
#' # See ?oe_export
#' }
oe_import = function(file_path, layer = NULL, query = NULL) {
  check_duckdb_installed()

  ext = tools::file_ext(file_path)
  if (
    !is.character(file_path) ||
    length(file_path) != 1L ||
    !file.exists(file_path) ||
    ext %!in% c("parquet", "duckdb")
  ) {
    oe_stop(
      .subclass = "oe_import-filePathMissingOrNotSupported",
      message = "The parameter file_path must correspond to an existing .parquet or .duckdb file"
    )
  }
  file_path = normalizePath(file_path)

  con = DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = if (ext == "duckdb") file_path else ":memory:",
    read_only = ext == "duckdb"
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  load_spatial(con)

  if (ext == "parquet") {
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TEMP VIEW data AS SELECT * FROM read_parquet(",
        DBI::dbQuoteString(con, file_path), ")"
      )
    )
  } else {
    tables = DBI::dbListTables(con)
    if (is.null(layer)) {
      if (length(tables) != 1L) {
        oe_stop(
          .subclass = "oe_import-layerRequired",
          message = paste0(
            "The duckdb file contains several tables (",
            paste(tables, collapse = ", "), "): set the layer argument."
          )
        )
      }
      layer = tables
    }
    if (layer %!in% tables) {
      oe_stop(
        .subclass = "oe_import-layerNotFound",
        message = paste0(
          "The table ", layer, " was not found in the duckdb file. Available tables: ",
          paste(tables, collapse = ", "), "."
        )
      )
    }
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TEMP VIEW data AS SELECT * FROM ",
        DBI::dbQuoteIdentifier(con, layer)
      )
    )
  }

  if (is.null(query)) {
    query = "SELECT * FROM data"
  }

  # Find the geometry column(s) of the query result
  col_info = DBI::dbGetQuery(con, paste0("DESCRIBE ", query))
  geom_idx = grep("^GEOMETRY", col_info[["column_type"]])
  if (!length(geom_idx)) {
    return(DBI::dbGetQuery(con, query))
  }
  geom_col = col_info[["column_name"]][geom_idx[[1L]]]
  geom_crs = regmatches(
    col_info[["column_type"]][geom_idx[[1L]]],
    regexpr("(?<=epsg:)[0-9]+", col_info[["column_type"]][geom_idx[[1L]]], perl = TRUE, ignore.case = TRUE)
  )
  geom_crs = if (length(geom_crs)) as.integer(geom_crs) else 4326L

  geom_quoted = DBI::dbQuoteIdentifier(con, geom_col)
  out = DBI::dbGetQuery(
    con,
    paste0(
      "SELECT * EXCLUDE (", geom_quoted, "), ST_AsWKB(", geom_quoted, ") AS ",
      geom_quoted, " FROM (", query, ")"
    )
  )

  geom = sf::st_as_sfc(
    structure(unclass(out[[geom_col]]), class = "WKB"),
    crs = geom_crs
  )
  out[[geom_col]] = NULL
  out = sf::st_sf(out, geometry = geom)

  # Same precision applied by oe_read() (see ?oe_read and #323)
  if (sf::st_crs(out) == sf::st_crs("EPSG:4326")) {
    sf::st_precision(out) = getOption("osmextract.precision", 1e7)
  }
  out
}

oe_layer_names = function() {
  c("points", "lines", "multipolygons", "multilinestrings", "other_relations")
}

check_duckdb_installed = function() {
  if (!all(vapply(c("duckdb", "DBI"), requireNamespace, logical(1), quietly = TRUE))) {
    oe_stop(
      .subclass = "oe_duckdb-packagesMissing",
      message = "This function requires the 'duckdb' and 'DBI' packages. Install them with install.packages(c('duckdb', 'DBI'))."
    )
  }
  invisible(TRUE)
}

load_spatial = function(con) {
  tryCatch(
    {
      DBI::dbExecute(con, "INSTALL spatial")
      DBI::dbExecute(con, "LOAD spatial")
    },
    error = function(cnd) {
      oe_stop(
        .subclass = "oe_duckdb-spatialUnavailable",
        message = paste0(
          "Cannot install or load the DuckDB spatial extension (an internet ",
          "connection is required the first time): ", conditionMessage(cnd)
        )
      )
    }
  )
  invisible(TRUE)
}
