#' Remove a specified element from the data frame cache
#'
#' @param object_to_remove [character] name of the object to be removed
#' as character string (quoted), or character vector containing the names of the
#' objects to remove from the cache
#'
#' @return nothing
#'
#' @family data-frame-cache
#' @export
#'
#' @examples
#' \dontrun{
#' prep_load_workbook_like_file("meta_data_v2") #load metadata in the cache
#' ls(.dataframe_environment()) #get the list of dataframes in the cache
#'
#' #remove cross-item_level from the cache
#' prep_remove_from_cache("cross-item_level")
#'
#' #remove dataframe_level and expected_id from the cache
#' prep_remove_from_cache(c("dataframe_level", "expected_id"))
#'
#' #remove missing_table and segment_level from the cache
#' x<- c("missing_table", "segment_level")
#' prep_remove_from_cache(x)
#' }
#'
prep_remove_from_cache <- function(object_to_remove) {
  #if the argument is one object, e.g. "meta_data"
  if(length(object_to_remove) == 1) {
    # check if the object is present in the cache and remove it
    if(object_to_remove %in% ls(.dataframe_environment())){
    rm(list=capture.output(cat(object_to_remove)),
       envir = .dataframe_environment())
    util_message(paste0(object_to_remove, " have been removed from cache"))
    } else { #provide a message is the object is not present in the cache
    util_message(paste0(object_to_remove, " not present in the cache"))
    }
  } else if (length(object_to_remove) > 1) {  #if the argument is a vector
    # check if all objects in the vector are present in the cache and remove them
    if(all(object_to_remove %in% ls(.dataframe_environment()))){
      rm(list=paste(object_to_remove), envir = .dataframe_environment())
      util_message(sprintf("%s have been removed from cache",
                           paste0(object_to_remove, collapse = " & ")))
    } else { # provide a message if not all objects in the vector are present in
      # the cache
      util_message(sprintf("Not all the objects (%s) are present in the cache",
                           paste0(object_to_remove, collapse = ", ")))
    }
  }
}
