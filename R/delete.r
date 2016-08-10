#' Move a file to trash
#' @export
#' @param file_id the ID of the file
#' @param token a valid OAuth2.0 token
#' @return ID of the trashed file
trash_file <- function(file_id, token = get_access_cred()) {
  url <- paste(base_url, file_id, "trash", sep = "/")
  req <- POST(url, config = config(token = token))
  stop_for_status(req)
  resp <- content(req)
  resp$id
}