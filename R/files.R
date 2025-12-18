# File selection -----------------------------------------------------------

eg_select_files <- function(multi = TRUE) {
  multi <- isTRUE(multi)
  paths <- character()

  if (multi && eg_has_tcltk()) {
    paths <- tcltk::tk_choose.files(multi = TRUE, caption = "Select PDF files for Evidence Guide")
  } else if (.Platform$OS.type == "windows") {
    paths <- utils::choose.files(multi = multi, caption = "Select PDF files for Evidence Guide")
  } else if (eg_can_use_rstudioapi()) {
    selected <- rstudioapi::selectFile(caption = "Select PDF file for Evidence Guide")
    paths <- if (!is.null(selected)) selected else character()
  } else {
    cli::cli_inform(c("i" = "Select PDF files. Cancel the dialog when finished."))
    repeat {
      choice <- tryCatch(utils::file.choose(new = length(paths) > 0), error = function(e) NA_character_)
      if (is.na(choice) || !nzchar(choice)) {
        break
      }
      paths <- c(paths, choice)
      if (!multi) {
        break
      }
    }
  }

  paths <- unique(normalizePath(paths, mustWork = FALSE))
  paths <- paths[file.exists(paths)]

  is_pdf <- tolower(tools::file_ext(paths)) == "pdf"
  dropped <- paths[!is_pdf]
  if (length(dropped)) {
    cli::cli_inform(c("!" = "Ignoring non-PDF files: {paste(basename(dropped), collapse = ', ')}"))
  }

  paths[is_pdf]
}

eg_has_tcltk <- function() {
  requireNamespace("tcltk", quietly = TRUE)
}

eg_can_use_rstudioapi <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(FALSE)
  }
  isTRUE(rstudioapi::isAvailable())
}
