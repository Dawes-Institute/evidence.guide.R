# End-to-end pipeline ------------------------------------------------------

eg_process <- function(files = NULL,
                       model = NULL,
                       mode = c("text", "vision"),
                       recompute = FALSE,
                       interval = 1.5,
                       timeout = 1800,
                       progress = TRUE,
                       api_key = eg_get_api_key(),
                       base_url = eg_get_base_url()) {
  mode <- match.arg(mode)

  chosen_files <- files
  if (is.null(chosen_files)) {
    chosen_files <- eg_select_files()
  }

  if (!length(chosen_files)) {
    cli::cli_inform(c("i" = "No files selected; returning empty result."))
    empty <- tibble::tibble(job_id = character(), status = character(), result = list(), error = list())
    return(list(
      jobs = empty,
      json = list(),
      studies = tibble::tibble()
    ))
  }

  uploads <- eg_upload(
    files = chosen_files,
    model = model,
    mode = mode,
    recompute = recompute,
    api_key = api_key,
    base_url = base_url
  )

  waits <- eg_wait(
    uploads$job_id,
    interval = interval,
    timeout = timeout,
    progress = progress,
    api_key = api_key,
    base_url = base_url
  )

  order_idx <- match(uploads$job_id, waits$job_id)
  ordered <- waits[order_idx, ]

  jobs <- tibble::tibble(
    file = uploads$file,
    job_id = uploads$job_id,
    status = ordered$status,
    result = ordered$result,
    error = ordered$error
  )

  success_idx <- which(jobs$status == "succeeded")
  json <- if (!length(success_idx)) {
    list()
  } else {
    purrr::set_names(jobs$result[success_idx], jobs$job_id[success_idx])
  }

  studies <- as_studies_df(json)

  list(
    jobs = jobs,
    json = json,
    studies = studies
  )
}
