#' Save an object as Rds file to drive
#' @export
#' @param obj object to be uploaded
#' @param title file title
#' @param email email address to add to permission list
#' @param token a valid OAuth2.0 token
#' @return file id
save_rds <- function(obj, title = NULL, email = NULL,
                     token = get_access_cred()) {
  tmp <- tempfile(fileext = ".Rds")
  on.exit(unlink(tmp))
  saveRDS(obj, tmp)
  if (is.null(title)) {
    title <- paste0(deparse(substitute(obj)), ".Rds")
  }
  file_id <- upload_file(tmp, title, email = email, token = token)
  print(paste0("File name: ", title))
  print(paste0("File id: ", file_id))
  return(file_id)
}

#' Load an object from Rds file from drive
#' @export
#' @param title file title
#' @param token a valid OAuth2.0 token
load_rds <- function(title, token = get_access_cred()) {
  file <- list_files_from_title(title)
  file_id <- file$items[[1]]$id
  tmp <- tempfile(fileext = ".Rds")
  on.exit(unlink(tmp))
  rds_file <- download_file(file_id, tmp, token = token)
  readRDS(rds_file)
}