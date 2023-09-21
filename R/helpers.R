#' Create a uid based on system time
#' 
#' @param ... additional uids to avoid time confrontations. For example, a user 
#' uid in addition to the system time to ensure that simultaneous edits by two 
#' users are still unique records
#' 
#' Note that the hash is created by crc32, and is not recommended to be used to 
#' hide sensitive information like an email or other identifying records
#' 
#' @export
uid <- function(...) {
  x = digest::digest(Sys.time(), algo = "crc32")
  if (!missing(...)) {
    z = list(...)
    x = paste(x, paste(lapply(z, \(y) digest::digest(y, algo = "crc32")), collapse = "-"), sep = "-")
  }
  x
}
