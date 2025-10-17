# Authentication helpers ---------------------------------------------------

#' Set the Evidence Guide API key for the current session.
#'
#' @param key Character API key beginning with `ck_`.
#' @param persist Logical; when `TRUE`, the key is written to `~/.Renviron`.
#' @return Invisibly returns the key.
eg_set_api_key <- function(key, persist = FALSE) {
  if (!rlang::is_string(key) || !nzchar(trimws(key))) {
    rlang::abort("API key must be a non-empty string.", class = c("eg_error", "eg_error_argument"))
  }

  key <- trimws(key)
  options("evidenceguide.api_key" = key)

  if (isTRUE(persist)) {
    eg_write_renviron(key)
  }

  invisible(key)
}

eg_write_renviron <- function(key) {
  path <- normalizePath(file.path("~", ".Renviron"), mustWork = FALSE)
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()

  pattern <- "^EVIDENCE_GUIDE_API_KEY="
  keep <- !grepl(pattern, lines)
  lines <- c(lines[keep], sprintf("EVIDENCE_GUIDE_API_KEY=%s", key))

  writeLines(lines, path, useBytes = TRUE)
}

#' Retrieve the Evidence Guide API key.
#'
#' @param key Optional key to prioritise over stored values.
#' @return Character scalar API key.
eg_get_api_key <- function(key = NULL) {
  if (!is.null(key)) {
    if (!rlang::is_string(key) || !nzchar(trimws(key))) {
      rlang::abort("API key must be a non-empty string.", class = c("eg_error", "eg_error_argument"))
    }
    return(trimws(key))
  }

  opt_key <- .eg_get_option("evidenceguide.api_key")
  if (is.character(opt_key) && nzchar(opt_key)) {
    return(opt_key)
  }

  env_key <- Sys.getenv("EVIDENCE_GUIDE_API_KEY", unset = "")
  if (nzchar(env_key)) {
    return(env_key)
  }

  rlang::abort(
    "Call `eg_set_api_key()` or set `EVIDENCE_GUIDE_API_KEY` in your environment first.",
    class = c("eg_error", "eg_error_auth")
  )
}

#' Override the Evidence Guide API base URL (useful for staging).
#'
#' @param url Character scalar.
#' @return Invisibly returns the URL.
eg_set_base_url <- function(url) {
  if (!rlang::is_string(url) || !nzchar(trimws(url))) {
    rlang::abort("Base URL must be a non-empty string.", class = c("eg_error", "eg_error_argument"))
  }
  options("evidenceguide.base_url" = trimws(url))
  invisible(url)
}
