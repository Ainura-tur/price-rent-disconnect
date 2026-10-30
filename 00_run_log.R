# =============================================================================
# 00_run_log.R  --  a log file for every run of every script
#
# Each script sources this file before its library calls, calls
# start_run_log() with its own name, and calls end_run_log() as its last
# line. Everything the script prints still reaches the console, and a copy
# goes to a new file:
#   run_logs/<script>_<date>_<time>.log
# The log holds
#   - a header: script, start time, folder, R version, git commit, run mode
#   - all printed output, as on the console
#   - every message, and the first three of each distinct warning, with the
#     time it occurred
#   - a footer: finish time, run time, status, every warning counted by
#     message, and sessionInfo(), which lists the package versions
# While a log is open, readr's and readxl's progress bars are off: they
# redraw themselves with carriage returns, which a file keeps as long runs of
# bars and spaces.
# If the script stops with an error, the footer gives the error and the calls
# that led to it, and Rscript exits with status 1, as it does without the log.
# run_logs/run_history.csv gets one row per run: script, start, finish,
# seconds, status, warnings, git commit and log file.
#
# Run scripts from the repository root, which holds this file and run_logs/.
# To run a script without a log, for instance line by line, set
# options(run_log = FALSE) first.
#
# A script sourced by another one, as run_all.R does, gets its own log, and
# its output also reaches the caller's log. If the caller catches an error in
# the sourced script, the next start_run_log() or end_run_log() closes that
# log with the status "stopped early".
#
# Printed output reaches the log through sink(split = TRUE). Messages and
# warnings bypass sinks, so the first log of an R session installs global
# calling handlers that copy them (globalCallingHandlers(), R 4.0 or later).
# R refuses these inside tryCatch() and similar wrappers. Under Rscript or
# source() there is no wrapper. Under one, such as a run_all.R that sources
# each script inside tryCatch(), the log holds the printed output only and
# says so in its header; a log started at the top level of the calling
# script, before the wrapper, installs the handlers for every script it runs.
# The handlers stay installed for the rest of the session and do nothing
# while no log is open.
#
# The functions live in their own environment, whose parent is base R, so
# the objects a script creates (02c defines get() and show(), for example)
# cannot change how they work. Sourcing this file again keeps that
# environment, so a sourced script cannot reset its caller's open log.
# =============================================================================

if (!exists(".run_log", inherits = FALSE) || !is.environment(.run_log)) {
  .run_log <- local({

    dir          <- "run_logs"
    stack        <- list()  # open logs, innermost last; one environment each
    handlers_on  <- FALSE   # message, warning and interrupt handlers installed?
    old_error    <- NULL    # options("error") before the first log opened
    old_progress <- list()  # progress-bar options before the first log opened
    max_repeats  <- 3L      # lines written per distinct warning; the footer counts all

    stamp <- function(x = Sys.time(), fmt = "%Y-%m-%d %H:%M:%S %Z") format(x, fmt)
    rule  <- strrep("-", 72)

    span <- function(secs) {
      if (secs < 120) sprintf("%.0f seconds", secs)
      else if (secs < 7200) sprintf("%.1f minutes", secs / 60)
      else sprintf("%.1f hours", secs / 3600)
    }

    one_line <- function(cl, width = 160L)
      substr(paste(deparse(cl, nlines = 1L), collapse = ""), 1L, width)

    # Name of the function a call invokes: "source" for source(...) and
    # base::source(...).
    call_name <- function(cl) {
      f <- cl[[1L]]
      if (is.call(f) && length(f) == 3L && is.name(f[[1L]]) &&
          as.character(f[[1L]]) %in% c("::", ":::")) f <- f[[3L]]
      if (is.name(f)) as.character(f) else ""
    }

    # The outermost tryCatch() or similar wrapper around the caller, or "" if
    # none. R refuses globalCallingHandlers() inside one.
    wrapper <- function() {
      hit <- intersect(vapply(sys.calls(), call_name, ""),
                       c("tryCatch", "withCallingHandlers", "try", "suppressWarnings",
                         "suppressMessages", "suppressPackageStartupMessages"))
      if (length(hit)) hit[[1L]] else ""
    }

    # The frame of the innermost source() call, or NULL under Rscript. A log
    # whose frame has left the call stack belongs to a run that was cut short.
    source_frame <- function() {
      i <- which(vapply(sys.calls(), call_name, "") == "source")
      if (length(i)) sys.frames()[[max(i)]] else NULL
    }

    git <- function() {
      run <- function(args)
        tryCatch(suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
                 error = function(e) character())
      hash <- run(c("rev-parse", "--short", "HEAD"))
      if (!length(hash) || !is.null(attr(hash, "status")))
        return(c(hash = "", text = "no commit found (not a git repository, or git not installed)"))
      dirty <- run(c("status", "--porcelain", "--untracked-files=no"))
      c(hash = hash[[1L]],
        text = paste0(hash[[1L]], if (length(dirty)) ", with uncommitted changes" else ""))
    }

    # Append text to one log. cat() appends by file name, through a new
    # connection each time: cat() to the sink's own connection would echo the
    # text to the console and to the logs below it. The sink's connection
    # appends too, so neither overwrites the other. A failed write is skipped.
    append_to <- function(e, text)
      tryCatch(cat(text, file = e$file, append = TRUE, sep = ""),
               error = function(err) NULL, warning = function(w) NULL)

    write_all <- function(text) for (e in stack) append_to(e, text)

    on_message <- function(cnd) {
      if (!length(stack)) return(invisible())
      tryCatch({
        txt <- conditionMessage(cnd)
        if (!grepl("\n$", txt)) txt <- paste0(txt, "\n")
        write_all(paste0("[", stamp(fmt = "%H:%M:%S"), "] ", txt))
      }, error = function(err) NULL)
      invisible()
    }

    on_warning <- function(cnd) {
      if (!length(stack)) return(invisible())
      tryCatch({
        msg   <- conditionMessage(cnd)
        cl    <- conditionCall(cnd)
        where <- if (is.null(cl)) "" else paste0(" in ", one_line(cl, 120L))
        time  <- paste0("[", stamp(fmt = "%H:%M:%S"), "] ")
        key   <- substr(msg, 1L, 200L)
        # Each log shows the first few repeats of a warning; its footer counts all.
        for (e in stack) {
          n <- e$warnings[key]
          n <- if (is.na(n)) 1L else n + 1L
          e$warnings[key] <- n
          if (n <= max_repeats)
            append_to(e, paste0(time, "Warning", where, ": ", msg, "\n"))
          else if (n == max_repeats + 1L)
            append_to(e, paste0(time, "Further \"", substr(msg, 1L, 80L),
                                "\" warnings are counted in the footer only.\n"))
        }
      }, error = function(err) NULL)
      invisible()
    }

    on_interrupt <- function(cnd) {
      if (!length(stack)) return(invisible())
      tryCatch(while (length(stack)) close_top("interrupted", c("The run was interrupted.", "")),
               error = function(err) NULL)
      invisible()
    }

    # Close the innermost log: end its sink and any sink opened after it,
    # write the footer, close the file and add a row to run_history.csv.
    close_top <- function(status, detail = character()) {
      e <- stack[[length(stack)]]
      stack <<- stack[-length(stack)]
      while (sink.number() > e$sink_level) sink()
      end  <- Sys.time()
      secs <- as.numeric(difftime(end, e$start, units = "secs"))
      w    <- sort(e$warnings, decreasing = TRUE)
      nw   <- sum(w)
      label <- if (status == "completed" && nw > 0)
        sprintf("completed, with %d warning%s", nw, if (nw == 1) "" else "s") else status
      shown <- utils::head(w, 20L)
      footer <- c(
        "", rule, detail,
        paste("Finished:", stamp(end), "after", span(secs)),
        paste("Status:  ", label),
        if (nw > 0) c("Warnings by message:",
                      sprintf("  %3d x %s", shown, names(shown)),
                      if (length(w) > 20L) sprintf("  ... and %d more messages", length(w) - 20L)),
        rule, "Session info:",
        tryCatch(utils::capture.output(utils::sessionInfo()),
                 error = function(err) "sessionInfo() failed"))
      tryCatch(close(e$con), error = function(err) NULL)
      tryCatch(cat(footer, file = e$file, append = TRUE, sep = "\n"),
               error = function(err) NULL)
      tryCatch({
        f   <- file.path(e$dir, "run_history.csv")
        new <- !file.exists(f)
        row <- data.frame(script = e$script, started = stamp(e$start, "%Y-%m-%d %H:%M:%S"),
                          finished = stamp(end, "%Y-%m-%d %H:%M:%S"), seconds = round(secs),
                          status = status, warnings = nw, git = e$git_hash,
                          log = basename(e$path))
        utils::write.table(row, f, sep = ",", row.names = FALSE, col.names = new,
                           append = !new, qmethod = "double")
      }, error = function(err) NULL)
      if (!length(stack)) {
        options(error = old_error)
        old_error <<- NULL
        options(old_progress)
        old_progress <<- list()
        # readr sets readr.show_progress = TRUE when it loads, unless the option
        # is already set; restore that default if readr loaded during the run.
        if (is.null(getOption("readr.show_progress")) && isNamespaceLoaded("readr"))
          options(readr.show_progress = TRUE)
      }
      e$path
    }

    # Close logs whose script ended without end_run_log(): their source()
    # call is no longer on the stack, so a caller caught an error.
    close_stale <- function() {
      frames <- sys.frames()
      while (length(stack)) {
        e <- stack[[length(stack)]]
        if (is.null(e$frame) || any(vapply(frames, identical, NA, e$frame))) break
        close_top("stopped early",
                  c("The script stopped before end_run_log(); its caller probably caught an",
                    "error. Any lines after that point came from the caller.", ""))
      }
    }

    # options("error") while a log is open. It runs before R unwinds, so the
    # calls that led to the error are still visible.
    on_error <- function() {
      old <- old_error
      if (length(stack)) {
        msg   <- sub("\n$", "", geterrmessage())
        calls <- sys.calls()
        calls <- calls[-length(calls)]
        tb    <- vapply(calls, one_line, "")
        detail <- c(msg, "",
                    if (length(tb)) c("Calls that led to the error, outermost first:",
                                      sprintf("  %2d. %s", seq_along(tb), tb), ""))
        while (length(stack)) close_top("failed", detail)
      }
      if (!is.null(old)) {
        if (is.function(old)) old()
        else if (is.expression(old)) for (x in old) eval(x, globalenv())
        else eval(old, globalenv())
      } else if (!interactive()) {
        # With options("error") set, Rscript would carry on after an error.
        cat("Execution halted\n", file = stderr())
        quit(save = "no", status = 1, runLast = FALSE)
      }
    }

    begin_log <- function(script) {
      if (isFALSE(getOption("run_log"))) return(invisible(NULL))
      close_stale()
      dir.create(dir, showWarnings = FALSE, recursive = TRUE)
      t0   <- Sys.time()
      base <- file.path(dir, paste0(script, "_", stamp(t0, "%Y-%m-%d_%H%M%S")))
      path <- paste0(base, ".log")
      k    <- 1L
      while (file.exists(path)) { k <- k + 1L; path <- paste0(base, "_", k, ".log") }
      g <- git()

      note <- character()
      if (!handlers_on) {
        w <- wrapper()
        if (nzchar(w)) {
          note <- c("Note:    messages and warnings are not copied here: the script runs inside",
                    sprintf("         %s(). To copy them, start a log at the top level of the calling", w),
                    "         script: source(\"00_run_log.R\"); start_run_log(\"run_all\"), for example.")
        } else {
          globalCallingHandlers(message = on_message, warning = on_warning,
                                interrupt = on_interrupt)
          handlers_on <<- TRUE
        }
      }

      e <- new.env(parent = emptyenv())
      e$script     <- script
      e$path       <- path
      e$dir        <- normalizePath(dir)
      e$file       <- file.path(e$dir, basename(path))  # absolute: scripts may change folder
      e$start      <- t0
      e$git_hash   <- g[["hash"]]
      e$sink_level <- sink.number()
      e$frame      <- source_frame()
      e$warnings   <- integer()

      field <- function(label, value) sprintf("%-9s%s", label, value)
      cat(c(field("Script:", script),
            field("Started:", stamp(t0)),
            field("Folder:", normalizePath(getwd())),
            field("R:", paste(R.version.string, "on", R.version$platform)),
            field("Git:", g[["text"]]),
            field("Run by:", if (interactive()) "an interactive R session" else "Rscript (batch)"),
            note, rule),
          file = e$file, sep = "\n")
      e$con <- file(e$file, open = "at")   # the sink appends after the header
      message("Run log: ", path)

      if (!length(stack)) {
        old_error <<- getOption("error")
        options(error = on_error)
        # Progress bars redraw themselves with carriage returns; a log file keeps
        # every frame, as long lines of bars and spaces. Switch them off meanwhile.
        old_progress <<- options(readr.show_progress = FALSE, readxl.show_progress = FALSE)
      }
      stack <<- c(stack, list(e))
      sink(e$con, split = TRUE)
      invisible(path)
    }

    finish_log <- function() {
      close_stale()
      if (!length(stack)) return(invisible(NULL))
      path <- close_top("completed")
      message("Run log saved: ", path)
      invisible(path)
    }

    environment()
  }, envir = new.env(parent = baseenv()))
}

RUN_LOG_DIR   <- .run_log$dir
start_run_log <- .run_log$begin_log
end_run_log   <- .run_log$finish_log
