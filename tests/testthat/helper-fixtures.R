fixture_path <- function(name) {
  file.path(testthat::test_path("fixtures"), name)
}

read_fixture_json <- function(name) {
  jsonlite::fromJSON(fixture_path(name), simplifyVector = FALSE)
}
