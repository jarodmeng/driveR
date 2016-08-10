base_url <- "https://www.googleapis.com/drive/v2/files"

#' List files using file title
#' @param title file title
#' @param token a valid OAuth2.0 token
#' @return req
#' @export
list_files_from_title <- function(title, token = get_access_cred()) {
  req <- GET(base_url, config = config(token = token),
      query = list(q = sprintf("trashed = false and fullText contains '%s'",
                               title)))
  
  stop_for_status(req)
  content(req)
}

#' Get a file from drive
#' @param file_id the ID for the file
#' @param token a valid OAuth2.0 token
#' @return file resource
#' @export
get_file <- function(file_id, token = get_access_cred()) {
  url <- paste(base_url, file_id, sep = "/")
  req <- GET(url, config = config(token = token))
  stop_for_status(req)
  content(req)
}

#' Get a file using file title
#' @param title file title
#' @param token a valid OAuth2.0 token
#' @return file resource
#' @export
get_file_from_title <- function(title, token = get_access_cred()) {
  file_list <- list_files_from_title(title)
  file_id <- file_list$items[[1]]$id
  get_file(file_id)
}

#' Download a file from drive
#' @param file_id the ID for the file
#' @param file_name the file name
#' @param token a valid OAuth2.0 token
#' @return the downloaded file name
#' @export
download_file <- function(file_id, file_name, token = get_access_cred()) {
  url <- paste(base_url, file_id, sep = "/")
  req <- GET(url, config = config(token = token),
             query = list(alt = "media"))
  stop_for_status(req)
  conn <- file(file_name, "wb")
  on.exit(close(conn))
  writeBin(content(req), conn)
  return(file_name)
}