library(testthat)

sample_result <- read_fixture_json("result_success.json")

complete_payload <- list(job123 = sample_result$result)

empty_payload <- list(job999 = list(metadata = list(title = "Empty", doi = NULL, year = 2024, journal = NULL), studies = list()))

test_that("as_studies_df flattens studies", {
  df <- as_studies_df(complete_payload)
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 2L)
  expect_true(all(c("job_id", "doi", "title", "study_number", "test", "stat_value") %in% names(df)))
  expect_equal(unique(df$job_id), "job123")
  expect_equal(df$title[[1]], "Intervention Improves Outcomes")
  expect_equal(df$stat_value[[1]], 2.31)
  expect_equal(df$effect_size_type[[2]], "d")
})

test_that("as_studies_df handles empty study lists", {
  df <- as_studies_df(empty_payload)
  expect_equal(nrow(df), 0L)
})

