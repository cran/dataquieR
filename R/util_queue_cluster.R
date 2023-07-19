util_queue_cluster_setup <- function(
                                n_nodes,
                                progress,
                                debug_parallel) {
  util_ensure_suggested(c("R6", "processx"))
  self <- NULL # https://github.com/r-lib/R6/issues/230#issuecomment-862462217
  private <- NULL # https://github.com/r-lib/R6/issues/230#issuecomment-862462217
  task_q <- R6::R6Class( # from the callr package by Csárdi and Chang
    "task_q",
    public = list(
      initialize = function(concurrency = n_nodes) {
        private$start_workers(concurrency)
        invisible(self)
      },
      list_tasks = function() private$tasks,
      get_num_waiting = function()
        sum(!private$tasks$idle & private$tasks$state == "waiting"),
      get_num_running = function()
        sum(!private$tasks$idle & private$tasks$state == "running"),
      get_num_done = function() sum(private$tasks$state == "done"),
      is_idle = function() sum(!private$tasks$idle) == 0,

      push = function(fun, args = list(), id = NULL) {
        if (is.null(id)) id <- private$get_next_id()
        if (id %in% private$tasks$id) stop("Duplicate task id")
        before <- which(private$tasks$idle)[1]
        private$tasks <- tibble::add_row(private$tasks, .before = before,
                                         id = id, idle = FALSE, state = "waiting", fun = list(fun),
                                         args = list(args), worker = list(NULL), result = list(NULL))
        private$schedule()
        invisible(id)
      },

      push2 = function(expr, id = NULL, work_fkt) {
        self$push(id = id,
          function(work_fkt, expr, id) {
            r <- list(
              try(work_fkt(expr, environment()))
            )
            names(r) <- id
            r
            },
          list(id = id,
               expr = expr,
               work_fkt = work_fkt))
      },

      compute_report = function(all_calls, worker, step) {
        for (.rno in seq_len(1 + (length(all_calls) %/% step))) { # at least 1
          rno <- (step * (.rno - 1)) + seq_len(step)
          rno <- rno[rno <= length(all_calls)]
          calls <- all_calls[rno]
          nms <- names(calls)
          id <- paste0(nms, collapse = "; ")
          self$push(id = id,
                    function(work_fkt, expr, id, nms) {
                      r <- lapply(setNames(expr, nm = nms),
                        function(e) {
                          try(work_fkt(e, environment()))
                      })
                      r
                    },
                    list(id = id,
                         expr = calls,
                         work_fkt = worker,
                         nms = nms))
        }
        r <- self$results()
        r <- r[names(all_calls)]
        return(r)
      },

      compute_report_old = function(all_calls, worker) {
        for (rno in seq_len(length(all_calls))) {
          call <- all_calls[[rno]]
          nm <- names(all_calls)[[rno]]
          self$push2(call, id = nm, work_fkt = worker)
        }
        self$results()[names(all_calls)]
      },

      poll = function(timeout = 0) {
        limit <- Sys.time() + timeout
        as_ms <- function(x) if (x == Inf) -1L else as.integer(x)
        repeat{
          topoll <- which(private$tasks$state == "running")
          conns <- lapply(
            private$tasks$worker[topoll],
            function(x) x$get_poll_connection())
          pr <- processx::poll(conns, as_ms(timeout))
          private$tasks$state[topoll][pr == "ready"] <- "ready"
          private$schedule()
          ret <- private$tasks$id[private$tasks$state == "done"]
          if (is.finite(timeout)) timeout <- limit - Sys.time()
          if (length(ret) || timeout < 0) break;
        }
        ret
      },

      pop = function(timeout = 0) {
        if (is.na(done <- self$poll(timeout)[1])) return(NULL)
        row <- match(done, private$tasks$id)
        result <- private$tasks$result[[row]]
        private$tasks <- private$tasks[-row, ]
        c(result, list(task_id = done))
      },

      workerEval = function(fkt, args) {
        private$wEval(fkt, args)
      },

      export = function(...) {
        for (nm in c(...)) {
          # cat("exporting", nm, "\n")
          private$wEval(function(x, y) {
            e <- rlang::global_env()
            assign(x = x, value = y, envir = e) }, list(
              nm,
              get(nm, parent.frame())
            ))
        }
      },

      results = function() {
        r <- list()
        n <- sum(!private$tasks$idle)
        while (!self$is_idle()) {
          task_result <- self$pop(Inf)
          progress(100 * (n-sum(!private$tasks$idle)) / n)
          # if (5 > length(task_result$result)) browser()
          r <- c(r, task_result$result)
        #   if (!self$is_idle())
        #     Sys.sleep(10)
        }
        r
      }

    ),

    private = list(
      tasks = NULL,
      next_id = 1L,
      get_next_id = function() {
        id <- private$next_id
        private$next_id <- id + 1L
        paste0(".", id)
      },

      wEval = function(fkt, args = list()) {
        stopifnot(all(private$tasks$state == "ready"))
        lapply(private$tasks$worker, function(w) {
          while (w$get_state() == "starting") {
            Sys.sleep(1)
            w$read()
          }
          w$run(fkt, args)
        })
      },

      start_workers = function(concurrency) {
        private$tasks <- tibble::tibble(
          id = character(), idle = logical(),
          state = c("waiting", "running", "ready", "done")[NULL],
          fun = list(), args = list(), worker = list(), result = list())
        for (i in seq_len(concurrency)) {
          rs <- callr::r_session$new(wait = FALSE)
          private$tasks <- tibble::add_row(private$tasks,
                                           id = paste0(".idle-", i), idle = TRUE, state = "ready", # state = "running",
                                           fun = list(NULL), args = list(NULL), worker = list(rs),
                                           result = list(NULL))
        }
      },

      schedule = function() {
        ready <- which(private$tasks$state == "ready")
        if (!length(ready)) return()
        rss <- private$tasks$worker[ready]

        private$tasks$result[ready] <- lapply(rss, function(x) x$read())
        private$tasks$worker[ready] <- replicate(length(ready), NULL)
        private$tasks$state[ready] <-
          ifelse(private$tasks$idle[ready], "waiting", "done")

        waiting <- which(private$tasks$state == "waiting")[1:length(ready)]
        private$tasks$worker[waiting] <- rss
        private$tasks$state[waiting] <-
          ifelse(private$tasks$idle[waiting], "ready", "running")
        lapply(waiting, function(i) {
          if (! private$tasks$idle[i]) {
            private$tasks$worker[[i]]$call(private$tasks$fun[[i]],
                                           private$tasks$args[[i]])
          }
        })
      }
    )
  )
  task_q$new()
}
