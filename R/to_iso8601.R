#' Convert POSIXct to an ISO 8601 string
#'
#' @param d data frame containing a POSIXct column to convert
#' @param col POSIXct column to convert
#' @param ...
#'
#' @return a data frame with the previous POSIXct column now an ISO 8601 string
#' @export
#'
to_iso8601 <- function(d,col,...) {
  d[,col] <- lapply(d[,col],
                    function(x) strftime(x,
                                         "%Y-%m-%dT%H:%M:%S%z",
                                         tz = "UTC"))
  return(d)
}