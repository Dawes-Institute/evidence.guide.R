# Tidy helpers -------------------------------------------------------------

as_studies_df <- function(results) {
  if (is.null(results)) {
    return(tibble::tibble())
  }

  if (!is.list(results) || inherits(results, "data.frame")) {
    results <- list(results)
  }

  if (is.null(names(results))) {
    names(results) <- rep("", length(results))
  }

  rows <- purrr::imap(results, function(result, job_id) {
    if (is.null(result)) {
      return(tibble::tibble())
    }

    payload <- result
    if (!is.null(payload$result)) {
      payload <- payload$result
    }

    metadata <- eg_coalesce(payload$paper, list())
    studies <- eg_coalesce(payload$studies, list())

    if (!length(studies)) {
      return(tibble::tibble())
    }

    purrr::imap_dfr(studies, function(study, idx) {
      resolved_job_id <- if (is.na(job_id) || identical(job_id, "")) NA_character_ else as.character(job_id)

      df_field <- eg_as_numeric(study$degrees_of_freedom)
      
      tibble::tibble(
        job_id = resolved_job_id,
        doi = eg_coalesce(metadata$doi, NA_character_),
        title = eg_coalesce(metadata$title, NA_character_),
        year = eg_as_integer(metadata$publication_year),
        journal = eg_coalesce(metadata$journal, NA_character_),
        study_number = eg_coalesce(study$study_number, idx),
        design = eg_coalesce(study$study_design, NA_character_),
        hypothesis = eg_coalesce(study$hypothesis, NA_character_),
        arms = paste0(eg_coalesce(study$arms, character()), collapse = "|"),
        test = eg_coalesce(study$test_statistic, NA_character_),
        measure = NA_character_,
        stat_value = eg_as_numeric(study$test_statistic_value),
        df = df_field,
        n = eg_as_integer(study$sample_size),
        p = eg_as_numeric(study$p_value),
        effect_size_type = NA_character_,
        effect_size = NA_real_,
        effect_size_ci_lower = NA_real_,
        effect_size_ci_upper = NA_real_,
        snippet = eg_coalesce(study$snippet, NA_character_),
        snippet_page = NA_integer_,
        confidence = eg_as_numeric(study$confidence)
      )
    })
  })

  rows <- Filter(function(x) nrow(x) > 0, rows)
  if (!length(rows)) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(rows)
}

eg_as_numeric <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  parsed <- suppressWarnings(as.numeric(x))
  if (all(is.na(parsed))) {
    return(NA_real_)
  }
  parsed
}

eg_as_integer <- function(x) {
  if (is.null(x)) {
    return(NA_integer_)
  }
  if (is.numeric(x)) {
    return(as.integer(round(x)))
  }
  parsed <- suppressWarnings(as.integer(x))
  if (is.na(parsed)) {
    return(NA_integer_)
  }
  parsed
}

eg_ci_bound <- function(ci, index) {
  if (is.null(ci) || length(ci) < index) {
    return(NA_real_)
  }
  eg_as_numeric(ci[[index]])
}
