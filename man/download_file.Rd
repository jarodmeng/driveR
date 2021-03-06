% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.r
\name{download_file}
\alias{download_file}
\title{Download a file from drive}
\usage{
download_file(file_id, file_name = tempfile(), token = get_access_cred())
}
\arguments{
\item{file_id}{the ID for the file}

\item{file_name}{the file name}

\item{token}{a valid OAuth2.0 token}
}
\value{
the downloaded file name
}
\description{
Download a file from drive
}

