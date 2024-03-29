set_state_inspector(function() {
  # normal purpose of this function is to check for changes of the global state
  # here, we abuse this function to ensure, that before any test runs,
  # the caches are emptied. Maybe, we need to be more careful and edit
  # each and any test to explicitly run the functions below, if needed.
  suppressMessages(prep_purge_data_frame_cache())
  # Sys.unsetenv("NOT_CRAN")
  prep_list_dataframes()
  util_reset_cache()
  invisible(NULL) # returned objects are considered global state
})
