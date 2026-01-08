cat1 <- as.character(util_as_cat(1))
cat2 <- as.character(util_as_cat(2))
cat3 <- as.character(util_as_cat(3))
cat4 <- as.character(util_as_cat(4))
cat5 <- as.character(util_as_cat(5))


util_funins <- function(f, expr, after = 1) { # https://stackoverflow.com/a/38733185
  expr <- substitute(expr)
  rlang::fn_body(f)<-as.call(append(as.list(rlang::fn_body(f)), expr, after=after))
  f
}

util_funwrap <- function(f, fn) {
  rlang::fn_body(f)<-
    call("{",
      call(fn, rlang::fn_body(f))
    )
  f
}

util_first_arg_study_data_or_resp_vars <- function() {
  cl <- rlang::caller_call()
  clan <- rlang::call_args_names(cl)
  cf <- rlang::caller_fn()
  formals_names <- names(formals(cf))
  if (length(formals_names) > 1 &&
      formals_names[[1]] %in% c("resp_vars", "variable_group", "label_col") &&
      "study_data" %in% formals_names  &&
      length(clan) >= 1
  ) {
    study_data_formal_pos <- which(formals_names == "study_data")
    first_arg_nm <- formals_names[[1]]
    if (clan[[1]] == "" &&
        !first_arg_nm %in% clan &&
        !"study_data" %in% clan) {
      missing_first_arg <- eval(call("missing",
                                     as.symbol(first_arg_nm)),
                                envir = rlang::caller_env())
      if (!missing_first_arg) {
        first_arg_val <- get(first_arg_nm, rlang::caller_env())
        if (is.data.frame(first_arg_val) ||
            isTRUE(try(first_arg_val %in% prep_list_dataframes(),
                       silent = TRUE)) ||
            is.data.frame(try(prep_get_data_frame(first_arg_val,
                                                  column_names_only = TRUE,
                                                  keep_types = TRUE),
                              silent = TRUE))) {
          args <- rlang::call_args(cl)[-1]

          # args <- c(setNames(list(formals(cf)[[1]]),
          #                    nm = first_arg_nm),
          #           rlang::call_args(cl)[-1])
          args[["study_data"]] <- first_arg_val
          repl_call <-
            do.call(call, c(list(rlang::call_name(cl)), args), quote = TRUE)
          return(repl_call)
        }
      }
    }
  }
  return(NULL)
}

util_maybe_eval_to_dataquieR_result <- function(my_call) {
  # detect recursion
  recursive_call <-
    sum(as.character(rlang::call_name(sys.call())) == vapply(sys.calls(),
                                                             function(x) {
                                                               r <-
                                                              rlang::call_name(
                                                                x
                                                              )
                                                               if (is.null(r))
                                                                 r <-
                                                                   NA_character_
                                                               return(r)
                                                              },
                                                             FUN.VALUE =
                                                              character(1))) > 1
  if (is.na(recursive_call)) {
    recursive_call <- FALSE
  }
  my_call <- substitute(my_call)
  if (.called_in_pipeline || # TODO: in pipielines, the conditions are already collected, this may be change, later:
      getOption("dataquieR.testdebug", dataquieR.testdebug_default) ||
      getOption("dataquieR.dontwrapresults", dataquieR.dontwrapresults_default) ||
      identical(Sys.getenv("TESTTHAT"), "true") ||
      recursive_call) {  # don't do, if in testthat runs, so that expect_condition still works.
    eval.parent(my_call)
  } else {
    call_to_attach <- as.symbol("<call not found>")
    if (util_is_try_error(try(call_to_attach <- rlang::call_match(rlang::caller_call(),
                                        rlang::caller_fn()), silent = TRUE))) {
      util_warning(c("Internal Warning, sorry. Please report: This will",
                     "show a deprecation warning, but it should anyway",
                     "not happen."))
      try(call_to_attach <- rlang::call_match(my_call, rlang::call_fn(my_call)),
          silent = TRUE)
    }
    fn <- rlang::call_name(rlang::caller_call())
    .v <- try(eval(as.symbol("resp_vars"), envir = rlang::caller_env()),
              silent = TRUE)
    if (!util_is_try_error(.v) && !is.null(.v)) {
      nm <- paste0(.v, collapse = SPLIT_CHAR)
    } else {
      nm <- "[ALL]"
    }
    nm <- paste0(fn, ".", nm)
    r <- util_eval_to_dataquieR_result(my_call,
                                       nm = nm,
                                       function_name = fn,
                                  filter_result_slots = c(),
                                  env = parent.frame(),
                                  checkpoint_resumed = FALSE,
                                  called_in_pipeline = FALSE) # dont handle stuff twice
    if (!util_is_try_error(.v))
      attr(call_to_attach, "entity_name") <- .v
    .v <- try(eval(as.symbol("label_col"), envir = rlang::caller_env()),
              silent = TRUE)
    if (!util_is_try_error(.v))
      attr(call_to_attach, "label_col") <- .v
    attr(r, "call") <- call_to_attach
    attr(r, "function_name") <- fn
    r
  }
}

preps <- quote({
  # maybe fix order ####
  ....alt_call_res <- NULL
  ..cl <- util_first_arg_study_data_or_resp_vars()
  if (is.call(..cl)) {
    # util_warning("%s", util_pretty_vector_string(ls(rlang::caller_env(3))))
    # util_message("Not calling %s, calling %s, instead...",
    #              sQuote(util_deparse1(rlang::caller_call(0))),
    #              sQuote(util_deparse1(..cl)))
    ....alt_call_res <- force(eval(..cl, rlang::caller_env(3)))
    return(....alt_call_res)
  } else {
    rm(..cl)
  }
  #######
  if (!.called_in_pipeline && # TODO: in pipielines, the conditions are already collected, this may be change, later:
      !getOption("dataquieR.testdebug", dataquieR.testdebug_default) &&
      !identical(Sys.getenv("TESTTHAT"), "true")) {  # don't do, if in testthat runs, so that expect_condition still works.
    my_call <- rlang::caller_call(2)
    if ("meta_data_v2" %in% names(formals()) &&
        !is.null(my_call$meta_data_v2)) {
      util_maybe_load_meta_data_v2()
      meta_data_v2 <- rlang::missing_arg()
      my_call$meta_data_v2 <- NULL
    }
    # IDEA: Called recursively?
    # search for study-data:
    env_args <- formals(util_meta_data_env)
    ..what_i_have <- intersect(names(env_args), ls())
    missing_env_args <- vapply(..what_i_have, FUN.VALUE = logical(1),
                               function(object) {
      .m <- call("missing", object)
      r <- try(eval(.m, envir = rlang::caller_env(2)), silent = TRUE)
      return(identical(r, TRUE))
    })
    ..what_i_have <- ..what_i_have[!missing_env_args]
    env_args[..what_i_have] <- mget(..what_i_have,
                                    inherits = FALSE)
    my_call <- rlang::call_match(my_call, rlang::current_fn())
    if (is.null(my_call$study_data) && "study_data" %in% names(formals())) {
      if (FALSE) { # FIXME ES: check the dataframe level metadata
        util_message("Using %s from your dataframe level metadata",
                     dQuote("study_data"))
        env_args <- list(study_data = .sd)
      } else if ("study_data" %in% prep_list_dataframes()) {
        util_message("Using %s from the dataframe cache.", dQuote("study_data"))
        .sd <- prep_get_data_frame("study_data")
        env_args <- list(study_data = .sd)
      } else if (exists("study_data", envir = rlang::caller_env(3))) {
        util_message("Using %s from your calling environment",
                     sQuote("study_data"))
        .sd <- get("study_data", envir = rlang::caller_env(3))
        env_args <- list(study_data = .sd)
      } else if (exists("study_data", envir = rlang::global_env())) {
        util_message("Using %s from your glabal environment",
                     sQuote("study_data"))
        .sd <- get("study_data", envir = rlang::global_env())
        env_args <- list(study_data = .sd)
      } else {
        util_warning(
          c("likely, the call will fail, because I could not find",
            "some %s, anywhere. Please pass them in your call or",
            "put them in your environment or data frame cache with",
            "the name %s"),
          sQuote("study_data"),
          dQuote("study_data"))
      }
    }
    .mde <- do.call(util_meta_data_env, env_args, quote = FALSE)
    #
    my_call_orig <- my_call
    my_call <- try(.mde$provisionize_call(my_call, internal = TRUE, # TODO: also handle other condtions
                                      env = environment()), silent = TRUE)
    if (util_is_try_error(my_call)) {
      if (.called_in_pipeline || # TODO: in pipielines, the conditions are already collected, this may be change, later:
          getOption("dataquieR.testdebug", dataquieR.testdebug_default) ||
          getOption("dataquieR.dontwrapresults", dataquieR.dontwrapresults_default) ||
          identical(Sys.getenv("TESTTHAT"), "true")) {  # don't do, if in testthat runs, so that expect_condition still works.
        util_error(my_call)
      } else {
        function_name <- "dataquieR"
        nm <- paste0(function_name, ".?????")
        try({
            function_name <- rlang::call_name(attr(my_call, "condition")$trace[[1]][[1]])
            fkt <- get(function_name,
                       mode = "function")
            rvs <- rlang::call_match(attr(my_call, "condition")$trace[[1]][[1]],
                                     fkt)[["resp_vars"]]
            if (length(rvs) > 1) rvs <- "[ALL]"
            if (length(rvs) < 1) rvs <- "?"
            gvs <- rlang::call_match(attr(my_call, "condition")$trace[[1]][[1]],
                                     fkt)[["group_vars"]] # TODO: this is not the name of the meta col but the content, but better than nothing
            if (length(gvs) == 1)
              nm <- paste0(function_name, "_", tolower(gvs), ".", rvs)
            else
              nm <- paste0(function_name, ".", rvs)

          },
          silent = TRUE
        )
        .r <- util_eval_to_dataquieR_result(
          util_error(my_call),
          function_name = function_name,
          nm = nm
        )
        class(.r) <- unique(c(class(.r), "master_result"))
        ....alt_call_res <- .r
        return(....alt_call_res)
      }
    }

    for(n in names(my_call)) {
      if (n %in% names(formals())) {
        if (!is.language(my_call[[n]])) {
          .v <- try(my_call[[n]], silent = TRUE)
        } else {
          .v <- try(eval(as.symbol(n)), silent = TRUE)
        }
        if (util_is_try_error(.v) || is.null(.v)) {
          .v2 <- eval.parent(my_call[[n]], n = 2)
          if (!util_is_try_error(.v2)) {
            .v <- .v2
          }
        }
        assign(n, .v)
      }
    }
  }
})

# TODO: Write a test for the following
util_decorator <- function(x,
                           f_nm = as.character(substitute(x))) {
  f <- util_funwrap(x, "util_maybe_eval_to_dataquieR_result")

  # reverse order (appends code)
  f <- util_funins(f,
    if (!is.null(....alt_call_res)) {
        return(....alt_call_res)
    }
  )
  # f <- util_funins(f,
  # )
  f <- util_funins(f, eval(preps))
  # /reverse order (appends code)
  f
}

for (fkt in ls(pattern = "^(int|com|con|acc|des)_")) {
  assign(fkt, util_decorator(get(fkt), fkt))
}

try({
  if (suppressWarnings(util_ensure_suggested("pkgload", err = FALSE,
                                             goal =
                                             "provide help on report sections during development"))) {
    dev_package <- pkgload::is_dev_package(utils::packageName())
  } else {
    dev_package <- FALSE
  }

  if (dev_package) {
    roxygenise_call <- 0
    called_from_rogygen <- FALSE
    while (!is.null(cl <- rlang::caller_call(roxygenise_call)) &&
           !called_from_rogygen) {
      ns <- rlang::call_ns(cl)
      nm <- rlang::call_name(cl)
      if (is.null(ns)) ns <- "" else ns <- paste0(ns, "::")
      if (paste0(ns, nm) == "roxygen2::roxygenise") {
        called_from_rogygen <- TRUE
      } else {
        roxygenise_call <- roxygenise_call + 1
      }
    }
    if (called_from_rogygen) withr::defer(local({
      use_cli_format <-
        suppressWarnings(util_ensure_suggested("cli", err = FALSE,
                                             goal =
                                               "nicer messages"))
      if (use_cli_format) {
        cmd <- sprintf("{.run [%s](%s)}", "dataquieR:::util_load_manual(TRUE)",
                       "dataquieR:::util_load_manual(TRUE)")
      } else {
        cmd <- "dataquieR:::util_load_manual(TRUE)"
      }
      need_rebuild <- FALSE
      max_man <- max(vapply(list.files("man/", full.names = TRUE, all.files = TRUE,
                                       pattern = "*.Rd", ignore.case = TRUE), file.mtime,
                            FUN.VALUE = file.mtime("/")), na.rm = TRUE)
      man_hash <- rlang::hash(rlang::hash_file(list.files("man/", full.names =
                                                            TRUE,
                                                          all.files = TRUE,
                                                          pattern = "*.Rd",
                                                          ignore.case = TRUE)))
      old_man_hash <- ""
      try(
        old_man_hash <- rio::import("inst/manual.RData",
                                   which = ".man_hash",
                                   trust = TRUE),
        silent = TRUE
      )
      if (old_man_hash == "" || old_man_hash != man_hash) {
        .update <- file.mtime("inst/manual.RData") < max_man
        .update <- .update || is.na(.update)
        if (.update) {
          rlang::inform(
            use_cli_format = use_cli_format,
            c(i = cli::format_inline(sprintf(paste("inst/manual.RData may be out of date, as a dataquieR developer,",
                                                   "please, consider updating it with %s -- doing this, now"),
                                             cmd))))
          need_rebuild <- TRUE
        }
      }
      old_man_hash <- ""
      try(
        old_man_hash <- rio::import("inst/indicator_or_descriptor.RData",
                                    which = ".man_hash",
                                    trust = TRUE),
        silent = TRUE
      )
      if (old_man_hash == "" || old_man_hash != man_hash) {
        .update <- file.mtime("inst/indicator_or_descriptor.RData") < max_man
        .update <- .update || is.na(.update)
        if (.update) {
          rlang::inform(
            use_cli_format = use_cli_format,
            c(i = cli::format_inline(sprintf(paste("inst/indicator_or_descriptor.RData may be out of date, as a",
                  " dataquieR developer, please, consider updating it with",
                  "%s -- doing this, now"), cmd))))
          need_rebuild <- TRUE
        }
      }
      if (need_rebuild) {
        if (suppressWarnings(util_ensure_suggested("devtools", err = FALSE,
                                                   goal =
                                                   "provide help on report sections during development")) &&
            suppressWarnings(util_ensure_suggested("callr", err = FALSE,
                                                   goal =
                                                   "provide help on report sections during development"))) {
          recursive_call <- Sys.getenv("dataquieR_child", "") == "TRUE"
          if (!recursive_call) callr::r(env =
                                          c(dataquieR_child = "TRUE"),
                                        function(man_hash) {
            devtools::document();
            devtools::load_all()
            util_load_manual(TRUE, man_hash = man_hash)
          }, args = list(man_hash = man_hash))
        }
      }
    }), envir = rlang::caller_env(roxygenise_call), priority = "last")
  }
}, silent = !TRUE)

.git_hash <- try(suppressWarnings(suppressMessages(
  system("git rev-parse HEAD",
         intern = TRUE,ignore.stderr = TRUE
         )
)), silent = TRUE)

if (util_is_try_error(.git_hash) ||
    !is.character(.git_hash) ||
    length(.git_hash) != 1 ||
    !grepl("^[a-z0-9]{40}$", .git_hash)) {
  .git_hash <- ""
}

util_dataquieR_version <- function() {
  gh <- .git_hash
  if (nzchar(gh)) {
    gh <- paste0(" (", gh, ")")
  }
  sprintf("%s%s",
          as.character(packageVersion(
            utils::packageName())),
          gh)
}

# Hint: Debug with warpper turned off: withr::with_options(list(warn = 2, dataquieR.dontwrapresults = TRUE), com_item_missingness(study_data, resp_vars = "SBP_0", item_level = md0, suppressWarnings = TRUE))

# IDEA: util_vectorize for a more tailored version of:
# print(lapply(c("SBP_0", "DBP_0"), acc_margins))
# print(lapply(c("SBP_0", "DBP_0"), com_item_missingness))
# print(Vectorize(com_item_missingness, SIMPLIFY = FALSE)(c("SBP_0", "DBP_0")))
# print(Vectorize(acc_varcomp, SIMPLIFY = FALSE)(c("SBP_0", "DBP_0")))
# print(Vectorize(acc_margins, SIMPLIFY = FALSE)(c("SBP_0", "DBP_0")))
# print(lapply(c("SBP_0", "DBP_0"), \(x) acc_margins(resp_vars = x)))
