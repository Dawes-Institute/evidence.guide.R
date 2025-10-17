# HTTP helpers -------------------------------------------------------------

eg_get_base_url <- function() {
  base <- .eg_get_option("evidenceguide.base_url", .eg_option_defaults[["evidenceguide.base_url"]])
  if (!rlang::is_string(base) || !nzchar(base)) {
    rlang::abort("Invalid Evidence Guide base URL; call `eg_set_base_url()` first.")
  }
  base
}

eg_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("evidenceguide", lib.loc = .libPaths())),
    error = function(...) "0.0.0.9000"
  )
  sprintf("evidenceguide/%s", version)
}

eg_request <- function(path = character(), api_key = eg_get_api_key(), base_url = eg_get_base_url()) {
  if (!rlang::is_string(api_key) || !nzchar(api_key)) {
    rlang::abort("Evidence Guide API key is missing.", class = c("eg_error", "eg_error_auth"))
  }

  req <- httr2::request(base_url)
  if (length(path)) {
    req <- httr2::req_url_path_append(req, path)
  }

  timeout <- .eg_get_option("evidenceguide.http_timeout", .eg_option_defaults[["evidenceguide.http_timeout"]])

  req |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `User-Agent` = eg_user_agent(),
      Accept = "application/json"
    ) |>
    httr2::req_options(timeout = timeout)
}

eg_req_perform <- function(req) {
  performer <- .eg_get_option("evidenceguide.http_perform")
  if (is.null(performer)) {
    httr2::req_perform(req)
  } else {
    performer(req)
  }
}

eg_resp_check <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    eg_abort_http(resp)
  }
  resp
}

eg_resp_json <- function(resp) {
  body <- httr2::resp_body_string(resp)
  if (!nzchar(body)) {
    return(list())
  }
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

eg_abort_http <- function(resp) {
  status <- httr2::resp_status(resp)
  body <- tryCatch(eg_resp_json(resp), error = function(...) list())

  message <- eg_format_http_message(status, body)
  details <- list(
    status = status,
    body = body,
    response = resp
  )

  rlang::abort(
    message,
    class = c(sprintf("eg_http_error_%s", status), "eg_http_error", "eg_error"),
    !!!details
  )
}

eg_format_http_message <- function(status, body) {
  code_msg <- switch(
    as.character(status),
    "400" = "Bad request. Check parameters and retry.",
    "401" = "Unauthorized. Confirm your Evidence Guide API key.",
    "402" = eg_format_http_402(body),
    "404" = "Job not found. Confirm the supplied job identifier.",
    "409" = "Duplicate upload detected. Use `recompute = TRUE` to force reprocessing.",
    "415" = "Only PDF files are accepted by the Evidence Guide API.",
    "429" = "Rate limit exceeded while calling Evidence Guide. Backing off and retrying is recommended.",
    "500" = "Server error from Evidence Guide. Please retry shortly.",
    "502" = "Bad gateway received from Evidence Guide. Retry later.",
    "503" = "Evidence Guide is temporarily unavailable. Retry later.",
    "504" = "Gateway timeout from Evidence Guide. Retry later.",
    paste("HTTP", status, "error returned by Evidence Guide.")
  )

  extra <- if (!is.null(body$message)) body$message else body$error
  if (!is.null(extra) && nzchar(extra)) {
    paste(code_msg, extra, sep = " ")
  } else {
    code_msg
  }
}

eg_format_http_402 <- function(body) {
  balance <- if (!is.null(body$balance)) body$balance else body$available
  required <- body$required
  url <- body$purchase_url

  parts <- c("Insufficient Evidence Guide credits.")
  if (!is.null(balance)) {
    parts <- c(parts, sprintf("Available: %s.", balance))
  }
  if (!is.null(required)) {
    parts <- c(parts, sprintf("Required: %s.", required))
  }
  if (!is.null(url)) {
    parts <- c(parts, sprintf("Purchase more credits at %s.", url))
  }
  paste(parts, collapse = " ")
}

eg_backoff_delay <- function(attempt) {
  delay <- 1.5^pmin(attempt, 10)
  delay <- min(60, delay + stats::runif(1, 0, 1))
  delay
}
