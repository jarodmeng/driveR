upload_url <- "https://www.googleapis.com/upload/drive/v2/files"

#' upload a file to Google Drive
#' @param file_name file name
#' @param title file title
#' @param type mime type of the file
#' @param token a valid OAuth2.0 token
#' @return ID of the uploaded file
#' @export
upload_file <- function(file_name, title = file_name, type = NULL,
                        token = get_access_cred()) {
  url <- upload_url

  metadata <- tempfile()
  writeLines(toJSON(list(title = title)), metadata)

  req <- POST(url,
    body = list(
      metadata = httr::upload_file(metadata,
                                   type = "application/json; charset=UTF-8"),
      media = httr::upload_file(file_name, type = type)),
    config = config(token = token),
    add_headers("Content-Type" = "multipart/related"),
    encode = "multipart")

  stop_for_status(req)
  resp <- content(req)
  return(resp$id)
}