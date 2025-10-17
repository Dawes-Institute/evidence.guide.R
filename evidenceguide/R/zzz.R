# Package initialization ---------------------------------------------------

.eg_option_defaults <- list(
  "evidenceguide.base_url" = "https://api.evidence.guide",
  "evidenceguide.http_timeout" = 60,
  "evidenceguide.http_perform" = NULL
)

.onLoad <- function(libname, pkgname) {
  missing <- setdiff(names(.eg_option_defaults), names(options()))
  if (length(missing)) {
    to_set <- .eg_option_defaults[missing]
    options(to_set)
  }
  invisible()
}

.eg_get_option <- function(name, default = NULL) {
  opt <- getOption(name, default = NULL)
  if (is.null(opt)) {
    return(default)
  }
  opt
}

eg_coalesce <- function(x, fallback) {
  if (is.null(x) || length(x) == 0) {
    fallback
  } else {
    x
  }
}
