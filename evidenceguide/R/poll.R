# Polling helpers ----------------------------------------------------------

eg_fetch <- function(job_ids,
                     api_key = eg_get_api_key(),
                     base_url = eg_get_base_url()) {
  ids <- unique(as.character(job_ids))
  if (!length(ids)) {
    return(tibble::tibble(job_id = character(), status = character(), result = list(), error = list()))
  }

  rows <- purrr::map(ids, function(id) {
    out <- tryCatch(
      eg_poll_once(id, api_key = api_key, base_url = base_url),
      error = function(err) err
    )

    if (inherits(out, "condition")) {
      tibble::tibble(
        job_id = id,
        status = "error",
        result = list(NULL),
        error = list(out)
      )
    } else {
      tibble::tibble(
        job_id = id,
        status = eg_coalesce(out$status, "unknown"),
        result = list(out$result),
        error = list(out$error)
      )
    }
  })

  dplyr::bind_rows(rows)
}

eg_wait <- function(job_ids,
                    interval = 1.5,
                    timeout = 1800,
                    progress = TRUE,
                    api_key = eg_get_api_key(),
                    base_url = eg_get_base_url()) {
  ids <- unique(as.character(job_ids))
  n <- length(ids)

  if (!n) {
    return(tibble::tibble(job_id = character(), status = character(), result = list(), error = list()))
  }

  if (interval <= 0) {
    rlang::abort("`interval` must be positive.", class = c("eg_error", "eg_error_argument"))
  }

  statuses <- rep("queued", n)
  last_known <- rep("queued", n)
  results <- vector("list", n)
  errors <- vector("list", n)
  attempts <- integer(n)
  done <- rep(FALSE, n)
  start <- Sys.time()
  deadline <- if (is.finite(timeout)) start + timeout else NA
  timeout_triggered <- FALSE

  pb <- NULL
  if (isTRUE(progress)) {
    pb <- progress::progress_bar$new(
      total = n,
      format = "Polling [:bar] :percent (:current/:total)",
      clear = FALSE
    )
  }

  while (any(!done)) {
    for (i in seq_len(n)) {
      if (done[i]) next

      if (!is.na(deadline) && Sys.time() > deadline) {
        statuses[i] <- "timeout"
        errors[[i]] <- list(list(message = "Polling timed out before completion.", last_status = last_known[i]))
        done[i] <- TRUE
        timeout_triggered <- TRUE
        if (!is.null(pb)) pb$tick()
        next
      }

      poll <- tryCatch(
        eg_poll_once(ids[i], api_key = api_key, base_url = base_url),
        eg_http_error_429 = function(err) structure(list(err = err), class = "eg_rate_limited"),
        error = function(err) structure(list(err = err), class = "eg_poll_failure")
      )

      if (inherits(poll, "eg_rate_limited")) {
        attempts[i] <- attempts[i] + 1L
        delay <- eg_backoff_delay(attempts[i])
        cli::cli_inform(c("!" = "Rate limit hit for job {ids[i]}; retrying in {sprintf('%.1f', delay)}s."))
        Sys.sleep(delay)
        next
      }

      if (inherits(poll, "eg_poll_failure")) {
        attempts[i] <- attempts[i] + 1L
        delay <- eg_backoff_delay(attempts[i])
        cli::cli_inform(c("!" = "Transient error while polling job {ids[i]}: {conditionMessage(poll$err)}. Retrying in {sprintf('%.1f', delay)}s."))
        Sys.sleep(delay)
        next
      }

      attempts[i] <- 0L
      statuses[i] <- eg_coalesce(poll$status, "unknown")
      last_known[i] <- statuses[i]

      if (identical(statuses[i], "succeeded") || identical(statuses[i], "completed")) {
        results[[i]] <- list(poll$result)
        errors[[i]] <- list(NULL)
        done[i] <- TRUE
        if (!is.null(pb)) pb$tick()
      } else if (identical(statuses[i], "failed")) {
        results[[i]] <- list(NULL)
        errors[[i]] <- list(eg_coalesce(poll$error, list(message = "Evidence Guide reported failure.")))
        done[i] <- TRUE
        if (!is.null(pb)) pb$tick()
      } else {
        results[[i]] <- list(poll$result)
        errors[[i]] <- list(poll$error)
      }
    }

    if (any(!done)) {
      Sys.sleep(interval)
    }
  }

  if (timeout_triggered) {
    warning("At least one job timed out before completion.", call. = FALSE)
  }

  tibble::tibble(
    job_id = ids,
    status = statuses,
    result = results,
    error = errors
  )
}

eg_poll_once <- function(job_id,
                         api_key = eg_get_api_key(),
                         base_url = eg_get_base_url()) {
  req <- eg_request(c("v1", "result", job_id), api_key = api_key, base_url = base_url)
  resp <- eg_req_perform(req)
  eg_resp_check(resp)
  body <- eg_resp_json(resp)

  status <- body$status
  payload <- body$result

  list(
    status = status,
    result = payload,
    error = body$error
  )
}
