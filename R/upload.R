# Upload workflow ----------------------------------------------------------

eg_upload <- function(files,
                      model = NULL,
                      mode = c("text", "vision"),
                      recompute = FALSE,
                      api_key = eg_get_api_key(),
                      base_url = eg_get_base_url()) {
  if (rlang::is_missing(files) || is.null(files)) {
    rlang::abort("`files` must be provided.", class = c("eg_error", "eg_error_argument"))
  }

  paths <- unique(normalizePath(as.character(files), mustWork = FALSE))
  if (!length(paths)) {
    rlang::abort("No files supplied.", class = c("eg_error", "eg_error_argument"))
  }

  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    rlang::abort(
      paste("The following files do not exist:", paste(missing, collapse = ", ")),
      class = c("eg_error", "eg_error_argument")
    )
  }

  exts <- tolower(tools::file_ext(paths))
  if (any(exts != "pdf")) {
    cli::cli_inform(c("!" = "Non-PDF files detected and ignored: {paste(basename(paths[exts != 'pdf']), collapse = ', ')}"))
    paths <- paths[exts == "pdf"]
  }

  if (!length(paths)) {
    rlang::abort("No PDF files remain after filtering.", class = c("eg_error", "eg_error_argument"))
  }

  mode <- match.arg(mode)

  query <- list(mode = mode)
  if (!is.null(model)) {
    query$model <- model
  }
  if (isTRUE(recompute)) {
    query$recompute <- "true"
  }

  results <- purrr::map(paths, function(path) {
    req <- eg_request(c("v1", "parse"), api_key = api_key, base_url = base_url)
    req <- do.call(httr2::req_url_query, c(list(req), query))
    req <- httr2::req_body_multipart(req, file = curl::form_file(path))
    resp <- eg_req_perform(req)
    eg_resp_check(resp)
    body <- eg_resp_json(resp)

    # Handle both fresh uploads (job_id at top level) and cached results (job_id in paper)
    job_id <- if (!is.null(body$job_id)) body$job_id else body$paper$job_id
    if (is.null(job_id)) {
      rlang::abort("Evidence Guide response did not include a `job_id`.", class = c("eg_error", "eg_error_protocol"))
    }

    tibble::tibble(
      file = path,
      job_id = as.character(job_id),
      status = "queued"
    )
  })

  dplyr::bind_rows(results)
}
