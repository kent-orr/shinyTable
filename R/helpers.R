#' Create a uid based on system time
#' 
#' @export
uid <- function() {
  digest::digest(Sys.time(), algo = "crc32")
}