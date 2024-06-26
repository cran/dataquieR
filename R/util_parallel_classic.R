
#' @keywords internal
util_parallel_classic <- function(all_calls,
                                  worker,
                                  n_nodes,
                                  progress,
                                  debug_parallel) {
  outer_env <- parent.frame()
  # was length(parallel::getDefaultCluster()), but parallelMap doesn't use
  # defaultcluster any more!!
  tasks_per_node <- ceiling(length(all_calls) / n_nodes)
  indices <- 1:length(all_calls)
  length(indices) <- n_nodes * tasks_per_node # make equal length
  task_matrix <- matrix(indices, ncol = n_nodes, nrow = tasks_per_node,
                        byrow = TRUE)

  if (getOption("parallelMap.mode") %in% c("BatchJobs", "batchtools")) {
    task_matrix <- matrix(seq_along(all_calls), nrow = 1) # for batch jobs, don't split
  }

  r <- unlist(lapply(
    1:nrow(task_matrix),
    function(row) {
      slices <- task_matrix[row, ]
      slices <- slices[!is.na(slices)]
      if (length(all_calls))
        progress(100 * row / nrow(task_matrix))
      util_message(
        sprintf("%s [%s] %d of %d, %s", Sys.time(), "INFO",
                row, nrow(task_matrix), "DQ")
      ) # TODO: Use RStudio job if available
      if (debug_parallel) {
        # TODO: log something about the current chunk
      }
      R.devices::suppressGraphics(
        # don't use any auto graphics device (needed for certain
        # parallelization methods)
        parallelMap::parallelLapply(impute.error = identity,
                                    all_calls[slices],
                                    worker,
                                    env = outer_env)
      )
    }
  ), recursive = FALSE)

  r
}
