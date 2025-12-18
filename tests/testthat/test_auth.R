library(testthat)

test_that("eg_set_api_key updates options", {
  withr::local_options(list(evidenceguide.api_key = NULL))
  expect_invisible(eg_set_api_key("ck_test"))
  expect_equal(getOption("evidenceguide.api_key"), "ck_test")
})

test_that("eg_get_api_key respects override and env var", {
  withr::local_options(list(evidenceguide.api_key = NULL))
  withr::local_envvar(EVIDENCE_GUIDE_API_KEY = "ck_env")
  expect_equal(eg_get_api_key(), "ck_env")
  expect_equal(eg_get_api_key("ck_direct"), "ck_direct")
})
