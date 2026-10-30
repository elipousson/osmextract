skip_if_no_duckdb_spatial = function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("DBI")
  con = DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ok = tryCatch(
    {
      DBI::dbExecute(con, "INSTALL spatial")
      DBI::dbExecute(con, "LOAD spatial")
      TRUE
    },
    error = function(cnd) FALSE
  )
  if (!ok) testthat::skip("DuckDB spatial extension is not available")
}

test_that("oe_export: parameter checks", {
  skip_if_no_duckdb_spatial()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  expect_error(
    oe_export("not-a-file.pbf"),
    class = "oe_export-filePathMissing"
  )
  expect_error(
    oe_export(its_pbf, layer = "roads"),
    class = "oe_export-layerNotProperlySpecified"
  )
  expect_error(
    oe_export(its_pbf, layer = NA_character_),
    class = "oe_export-layerNotProperlySpecified"
  )

  txt = file.path(tempdir(), "file.txt")
  writeLines("a", txt)
  withr::defer(unlink(txt))
  expect_error(oe_export(txt), class = "oe_export-fileExtensionNotSupported")

  expect_error(
    oe_import(txt),
    class = "oe_import-filePathMissingOrNotSupported"
  )
})

test_that("oe_export to parquet works and round trips with oe_import", {
  skip_if_no_duckdb_spatial()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  out = oe_export(its_pbf, layer = c("lines", "points"), quiet = TRUE)
  withr::defer(unlink(out))
  expect_named(out, c("lines", "points"))
  expect_true(all(file.exists(out)))
  expect_equal(unname(tools::file_ext(out)), c("parquet", "parquet"))

  its_lines = oe_import(out[["lines"]])
  expected = sf::st_read(
    sub("_lines\\.parquet$", ".gpkg", out[["lines"]]),
    layer = "lines",
    quiet = TRUE
  )
  expect_s3_class(its_lines, "sf")
  expect_equal(nrow(its_lines), nrow(expected))
  expect_true(sf::st_crs(its_lines) == sf::st_crs(expected))
  expect_true(all(sf::st_geometry_type(its_lines) == "LINESTRING"))
  expect_true(all(c("osm_id", "highway", "other_tags") %in% names(its_lines)))

  # Compare the geometries
  expect_equal(
    unname(sf::st_length(its_lines)),
    unname(sf::st_length(expected)),
    tolerance = 1e-6
  )

  # query argument
  q = "SELECT * FROM data WHERE highway = 'primary'"
  its_primary = oe_import(out[["lines"]], query = q)
  expect_equal(
    nrow(its_primary),
    sum(expected[["highway"]] == "primary", na.rm = TRUE)
  )

  # A query with no geometry returns a data.frame
  its_n = oe_import(out[["lines"]], query = "SELECT count(*) AS n FROM data")
  expect_false(inherits(its_n, "sf"))
  expect_equal(its_n[["n"]], nrow(expected))
})

test_that("oe_export skips existing files unless force_export = TRUE", {
  skip_if_no_duckdb_spatial()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  out = oe_export(its_pbf, quiet = TRUE)
  withr::defer(unlink(out))
  expect_message(
    oe_export(its_pbf),
    class = "oe_export_skipOperations"
  )
  expect_message(
    oe_export(its_pbf, force_export = TRUE),
    class = "oe_export_finishedExport"
  )
  expect_false(file.exists(paste0(out[["lines"]], ".tmp")))
})

test_that("oe_export works with an existing .gpkg and to duckdb format", {
  skip_if_no_duckdb_spatial()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()
  its_gpkg = oe_vectortranslate(its_pbf, quiet = TRUE)
  oe_vectortranslate(its_pbf, layer = "points", quiet = TRUE)

  out = oe_export(its_gpkg, layer = c("lines", "points"), format = "duckdb", quiet = TRUE)
  withr::defer(unlink(out))
  expect_length(out, 1L)
  expect_equal(tools::file_ext(out), "duckdb")

  expect_error(oe_import(out), class = "oe_import-layerRequired")
  expect_error(oe_import(out, layer = "multipolygons"), class = "oe_import-layerNotFound")
  its_points = oe_import(out, layer = "points")
  expect_s3_class(its_points, "sf")
  expect_equal(
    nrow(its_points),
    nrow(sf::st_read(its_gpkg, layer = "points", quiet = TRUE))
  )
  expect_message(
    oe_export(its_gpkg, layer = "lines", format = "duckdb"),
    class = "oe_export_skipOperations"
  )

  # A layer that is not in the .gpkg file
  expect_error(
    oe_export(its_gpkg, layer = "multipolygons"),
    class = "oe_export-layerNotInGpkg"
  )
})

test_that("oe_export with extra_tags adds the new columns", {
  skip_if_no_duckdb_spatial()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  out = oe_export(its_pbf, quiet = TRUE)
  withr::defer(unlink(out))
  expect_false("oneway" %in% names(oe_import(out[["lines"]])))

  out = oe_export(its_pbf, extra_tags = "oneway", quiet = TRUE)
  expect_true("oneway" %in% names(oe_import(out[["lines"]])))
})

# engine = "osmium" ------------------------------------------------------

skip_if_no_osmium = function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("DBI")
  con = tryCatch(connect_osmium(), error = function(cnd) NULL)
  if (is.null(con)) testthat::skip("DuckDB osmium extension is not available")
  DBI::dbDisconnect(con, shutdown = TRUE)
}

test_that("oe_export(engine = 'osmium'): parameter checks", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("DBI")
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()
  its_gpkg = oe_vectortranslate(its_pbf, quiet = TRUE)

  expect_error(
    oe_export(its_gpkg, engine = "osmium"),
    class = "oe_export-osmiumRequiresPbf"
  )
  expect_error(
    oe_export(its_pbf, layer = "multilinestrings", engine = "osmium"),
    class = "oe_export-osmiumLayerNotSupported"
  )
  expect_error(
    oe_export(its_pbf, layer = c("lines", "other_relations"), engine = "osmium"),
    class = "oe_export-osmiumLayerNotSupported"
  )
})

test_that("oe_export(engine = 'osmium') matches the GDAL layers on the ITS example", {
  skip_if_no_osmium()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()
  layers = c("points", "lines", "multipolygons")

  expect_warning(
    out <- oe_export(its_pbf, layer = layers, engine = "osmium", extra_tags = "oneway", quiet = TRUE),
    "extra_tags is ignored"
  )
  withr::defer(unlink(out))
  expect_named(out, layers)
  expect_true(all(file.exists(out)))
  expect_match(basename(out), "_osmium_")
  # No gpkg file is created
  expect_false(any(grepl("\\.gpkg$", list.files(tempdir()))))

  for (lyr in layers) {
    its_gdal = oe_vectortranslate(its_pbf, layer = lyr, quiet = TRUE)
    expected = sf::st_read(its_gdal, layer = lyr, quiet = TRUE)
    expected_id = switch(
      lyr,
      points = paste("node", expected[["osm_id"]]),
      lines = paste("way", expected[["osm_id"]]),
      multipolygons = ifelse(
        is.na(expected[["osm_id"]]),
        paste("way", expected[["osm_way_id"]]),
        paste("relation", expected[["osm_id"]])
      )
    )
    got = oe_import(out[[lyr]])
    expect_s3_class(got, "sf")
    expect_named(got, c("type", "id", "tags", "geometry"))
    expect_setequal(paste(got[["type"]], got[["id"]]), expected_id)
    expect_true(sf::st_crs(got) == sf::st_crs(4326))
    expect_false(anyNA(sf::st_dimension(got)))
  }
})

test_that("oe_export(engine = 'osmium') skips existing outputs unless force_export = TRUE", {
  skip_if_no_osmium()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  out = oe_export(its_pbf, engine = "osmium", quiet = TRUE)
  withr::defer(unlink(out))
  expect_message(
    oe_export(its_pbf, engine = "osmium"),
    class = "oe_export_skipOperations"
  )
  expect_message(
    oe_export(its_pbf, engine = "osmium", force_export = TRUE),
    class = "oe_export_finishedExport"
  )
  expect_false(file.exists(paste0(out[["lines"]], ".tmp")))
})

test_that("oe_export(engine = 'osmium') works with the duckdb format", {
  skip_if_no_osmium()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  out = oe_export(
    its_pbf,
    layer = c("lines", "points"),
    format = "duckdb",
    engine = "osmium",
    quiet = TRUE
  )
  withr::defer(unlink(out))
  expect_length(out, 1L)
  expect_match(basename(out), "_osmium\\.duckdb$")
  expect_error(oe_import(out), class = "oe_import-layerRequired")
  expect_s3_class(oe_import(out, layer = "points"), "sf")
  expect_message(
    oe_export(its_pbf, layer = "lines", format = "duckdb", engine = "osmium"),
    class = "oe_export_skipOperations"
  )
})

test_that("oe_export(engine = 'osmium') applies the boundary argument", {
  skip_if_no_osmium()
  withr::local_envvar(
    .new = list(
      "OSMEXT_DOWNLOAD_DIRECTORY" = tempdir(),
      "TESTTHAT" = "true"
    )
  )
  its_pbf = setup_pbf()

  its_poly = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-1.55577, 53.80850),
      c(-1.55787, 53.80926),
      c(-1.56096, 53.80891),
      c(-1.56096, 53.80736),
      c(-1.55675, 53.80658),
      c(-1.55495, 53.80749),
      c(-1.55577, 53.80850)
    ))),
    crs = 4326
  )

  all_lines = oe_export(its_pbf, engine = "osmium", quiet = TRUE)
  n_all = nrow(oe_import(all_lines[["lines"]]))

  spat = oe_export(its_pbf, engine = "osmium", boundary = its_poly, quiet = TRUE)
  lines_spat = oe_import(spat[["lines"]])
  expect_gt(nrow(lines_spat), 0L)
  expect_lt(nrow(lines_spat), n_all)
  # The same features selected by GDAL (-spat)
  gdal_spat = sf::st_read(
    oe_vectortranslate(its_pbf, boundary = its_poly, quiet = TRUE),
    layer = "lines",
    quiet = TRUE
  )
  expect_setequal(lines_spat[["id"]], as.numeric(gdal_spat[["osm_id"]]))

  # clipsrc keeps only the parts inside the boundary
  clip = oe_export(
    its_pbf,
    engine = "osmium",
    boundary = its_poly,
    boundary_type = "clipsrc",
    quiet = TRUE
  )
  lines_clip = oe_import(clip[["lines"]])
  expect_gt(nrow(lines_clip), 0L)
  expect_true(all(sf::st_within(lines_clip, sf::st_buffer(its_poly, 1e-9), sparse = FALSE)))
  expect_lt(
    sum(as.numeric(sf::st_length(lines_clip))),
    sum(as.numeric(sf::st_length(lines_spat)))
  )

  # boundary = NA imports everything
  na_boundary = oe_export(its_pbf, engine = "osmium", boundary = NA, quiet = TRUE)
  expect_equal(nrow(oe_import(na_boundary[["lines"]])), n_all)
})
