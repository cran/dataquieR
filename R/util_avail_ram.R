# Title
#
# @returns
# @seealso https://stackoverflow.com/a/76664004
# @author Alexander Christensen
# util_get_avail_ram <- function() {
#   # Get operating system
#   OS <- tolower(Sys.info()["sysname"])
#
#   # Branch based on OS
#   if (OS == "windows") {
#     # Windows
#
#     # System information
#     system_info <- system("systeminfo", intern = TRUE)
#
#     # Get available memory
#     value <- system_info[grep("Available Physical Memory", system_info)]
#
#     # Remove extraneous information
#     value <- gsub("Available Physical Memory: ", "", value)
#     value <- gsub("\\,", "", value)
#
#     # Convert to bytes
#     value_split <- unlist(strsplit(value, split = " "))
#
#     # Check for second value
#     bytes <- as.numeric(value_split[1]) * switch(value_split[2],
#                                                  "KB" = 1e03,
#                                                  "MB" = 1e06,
#                                                  "GB" = 1e09)
#
#   } else if (OS == "linux") {
#     # Linux
#
#     # Split system information
#     info_split <- strsplit(system("free", intern = TRUE), split = " ")
#
#     # Remove "Mem:" and "Swap:"
#     info_split <- lapply(info_split, function(x) {
#       gsub("Mem:", "", x)
#     })
#     info_split <- lapply(info_split, function(x) {
#       gsub("Swap:", "", x)
#     })
#
#     # Get actual values
#     info_split <- lapply(info_split, function(x) {
#       x[x != ""]
#     })
#
#     # Bind values
#     info_split <- do.call(rbind, info_split[1:2])
#
#     # Get free values
#     bytes <- as.numeric(info_split[2, info_split[1, ] == "free"])
#
#   } else {
#     # Mac
#
#     # System information
#     system_info <- system("top -l 1 -s 0 | grep PhysMem", intern = TRUE)
#
#     # Get everything after comma
#     unused <- gsub(" .*,", "", system_info)
#
#     # Get values only
#     value <- gsub("PhysMem: ", "", unused)
#     value <- gsub(" unused.", "", value)
#
#     # Check for bytes
#     if (grepl("M", value)) {
#       bytes <- as.numeric(gsub("M", "", value)) * 1e06
#     } else if (grepl("G", value)) {
#       bytes <- as.numeric(gsub("G", "", value)) * 1e09
#     } else if (grepl("K", value)) {
#       bytes <- as.numeric(gsub("K", "", value)) * 1e03
#     }
#
#   }
#
#   class(bytes) <- "object_size"
#
#   # Return bytes
#   return(bytes)
#
# }
