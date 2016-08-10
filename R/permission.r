#' Add permission for a file
#' @export
#' @importFrom httr content_type_json
#' @param file_id the ID of a file
#' @param email email address to be added
#' @param role the primary role of the user
#' @param type the account type
#' @param token a valid OAuth2.0 token
#' @param ... any other argument to be passed to the API call
#' @return permission id
add_permission <- function(file_id, email, role = "reader", type = "user",
                           token = get_access_cred(), ...) {
  url <- paste(base_url, file_id, "permissions", sep = "/")
  body <- list(role = role, type = type, value = email, ...)
  req <- POST(url, body = body, config = config(token = token),
              encode = "json", content_type_json())
  stop_for_status(req)
  resp <- content(req)
  resp$id
}