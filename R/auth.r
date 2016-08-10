driveR <- httr::oauth_app("google",
  "46564717964-nu77q8pc1b5k8vngcggr8nih3ougnhne.apps.googleusercontent.com",
  "t0322nEGQjoBNMdZlW-n30rE")

google <- httr::oauth_endpoints("google")

drive_env <- new.env(parent = emptyenv())

#' Get and set access credentials
#'
#' @section API console:
#' To manage your google projects, use the API console:
#' \url{https://console.cloud.google.com/}
#'
#' @keywords internal
#' @export
#' @param value new access credentials, as returned by
#'  \code{\link[httr]{oauth2.0_token}}
get_access_cred <- function() {
  cred <- drive_env$access_cred
  if (is.null(cred)) {
    set_oauth2.0_cred()
  }
  
  drive_env$access_cred
}

#' @rdname get_access_cred
#' @importFrom httr oauth2.0_token
#' @export
set_oauth2.0_cred <- function(app = driveR) {
  cred <- oauth2.0_token(google, app,
                         scope = c("https://www.googleapis.com/auth/drive"))
  
  set_access_cred(cred)
}

#' @rdname get_access_cred
#' @export
set_access_cred <- function(value) {
  drive_env$access_cred <- value
}

#' @rdname get_access_cred
#' @export
reset_access_cred <- function() {
  set_access_cred(NULL)
}

#' @export
#' @importFrom httr oauth_service_token
#' @rdname get_access_cred
#' @param service_token A JSON string, URL or file, giving or pointing to
#'   the service token file.
set_service_token <- function(service_token) {
  
  service_token <- fromJSON(service_token)
  
  scope <- "https://www.googleapis.com/auth/drive"
  
  cred <- oauth_service_token(google, service_token, scope)
  
  set_access_cred(cred)
}