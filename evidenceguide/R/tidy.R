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

    metadata <- eg_coalesce(payload$metadata, list())
    studies <- eg_coalesce(payload$studies, list())

    if (!length(studies)) {
      return(tibble::tibble())
    }

    purrr::imap_dfr(studies, function(study, idx) {
      resolved_job_id <- if (is.na(job_id) || identical(job_id, "")) NA_character_ else as.character(job_id)

      primary_test <- if (!is.null(study$tests) && length(study$tests)) study$tests[[1]] else list()
      primary_excerpt <- if (!is.null(study$excerpts) && length(study$excerpts)) study$excerpts[[1]] else list()

      df_value <- primary_test$df
      df_field <- if (is.null(df_value)) {
        NA_character_
      } else if (length(df_value) == 1L) {
        parsed <- suppressWarnings(as.numeric(df_value))
        if (!is.na(parsed)) parsed else as.character(df_value)
      } else {
        paste(df_value, collapse = ", ")
      }

      tibble::tibble(
        job_id = resolved_job_id,
        doi = eg_coalesce(metadata$doi, NA_character_),
        title = eg_coalesce(metadata$title, NA_character_),
        year = eg_as_integer(metadata$year),
        journal = eg_coalesce(metadata$journal, NA_character_),
        study_number = eg_coalesce(study$study_number, idx),
        design = eg_coalesce(study$design, NA_character_),
        hypothesis = eg_first_or_na(study$hypotheses),
        arms = paste0(eg_coalesce(study$arms, character()), collapse = "|"),
        test = eg_coalesce(primary_test$stat, NA_character_),
        measure = eg_coalesce(primary_test$measure, NA_character_),
        stat_value = eg_as_numeric(primary_test$value),
        df = df_field,
        n = eg_as_integer(study$n),
        p = eg_as_numeric(primary_test$p),
        effect_size_type = eg_coalesce(primary_test$effect_size$type, NA_character_),
        effect_size = eg_as_numeric(primary_test$effect_size$value),
        effect_size_ci_lower = eg_ci_bound(primary_test$effect_size$ci, 1L),
        effect_size_ci_upper = eg_ci_bound(primary_test$effect_size$ci, 2L),
        snippet = eg_coalesce(primary_excerpt$text, NA_character_),
        snippet_page = eg_coalesce(primary_excerpt$page, NA_integer_),
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

eg_first_or_na <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NA_character_)
  }
  as.character(x[[1]])
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
