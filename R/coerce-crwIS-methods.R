#' Coerce \code{"crwIS"} to sp classes
#'
#' Package \pkg{crawlr} implements \code{coerce}-methods (i.e,
#' (\code{as(object, Class)})) to convert \code{"crwIS"} S3 objects from
#' the \code{crawl} package to
#' \code{"\linkS4class{SpatialPointsDataFrame}"} or
#' \code{"\linkS4class{SpatialLines}"}
#' or \code{"data.frame"}.
#'
#' @author Josh M. London
#' @name coerce-crwIS-methods
#' @rdname coerce-crwIS-methods
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp SpatialLines
#' @exportMethod coerce
NULL

#' "crwIS" class
#'
#' We will register the S3 'crwIS' class from crawl as a formally
#' defined class to we can create the
#' @name crwIS-class
#' @rdname coerce-crwIS-methods
#'
#' @exportClass crwIS
setOldClass("crwIS","crwIS")

#' @name coerce-crwIS
#' @param crwISobj an object of class \code{"crwIS"},
#' @rdname coerce-crwIS-methods
#' @export
as.SPDF <- function(crwISobj) {
  spdf <- data.frame(mu.x = crwISobj$alpha.sim[,'mu.x'],
                     mu.y = crwISobj$alpha.sim[,'mu.y'],
                     num_time = crwISobj$Time,
                     locType = crwISobj$locType
  )
  coordinates(spdf) <- ~mu.x+mu.y
  return(spdf)
}

#' @name coerce-crwIS
#' @rdname coerce-crwIS-methods
setAs(from = "crwIS", to = "SpatialPointsDataFrame",
      def = function(from) as.SPDF(from))

#' @param crwISobj an object of class \code{"crwIS"},
#' @rdname coerce-crwIS-methods
#' @export
as.SL <- function(crwISobj) {
  spdf <- as.SPDF(crwISobj)
  sl <- as(spdf, "SpatialLines")
  return(sl)
}

#' @name coerce-crwIS
#' @rdname coerce-crwIS-methods
setAs(from = "crwIS", to = "SpatialLines",
      def = function(from) as.SL(from))

#' @name coerce-crwIS
#' @param crwISobj an object of class \code{"crwIS"},
#' @rdname coerce-crwIS-methods
#' @export
as.data.frame.crwIS <- function(crwISobj) {
  df <- data.frame(mu.x = crwISobj$alpha.sim[,'mu.x'],
                   mu.y = crwISobj$alpha.sim[,'mu.y'],
                   num_time = crwISobj$Time,
                   locType = crwISobj$locType
  )
  df
}

#' @name coerce-crwIS
#' @rdname coerce-crwIS-methods
setAs(from = "crwIS", to = "data.frame",
      def = function(from) as.data.frame.crwIS(from))
