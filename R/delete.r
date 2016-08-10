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

#' Move file(s) to trash
#' @export
#' @param title file title
#' @param all whether to move all file(s) having the same title to trash
#' @param token a valid OAuth2.0 token
#' @return ID(s) of file(s) moved to trash
trash_files_from_title <- function(title, all = FALSE,
                                   token = get_access_cred()) {
  files <- list_files_from_title(title, token)
  
  if (!all) {
    file_id <- files$items[[1]]$id
    return(trash_file(file_id, token))
  } else {
    file_ids <- sapply(files$items, function(x) x$id)
    return(sapply(file_ids, trash_file, token = token))
  }
}