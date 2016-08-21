#' Subset \code{"crwIS"} objects
#'
#' Package \pkg{crawlr} implements \code{subset}-methods to
#' subset \code{"crwIS"} S3 objects from
#' the \code{crawl} package based on \code{locType}
#'
#' @author Josh M. London
#' @name subset-crwIS-methods
#' @rdname subset-crwIS-methods
#'

setMethod("subset", signature(x = "crwIS"),
          function(x, locType = c("o","p"), ...) {
            loctype_select <- which(x$locType %in% locType)
            x$alpha.sim <- x$alpha.sim[loctype_select,]
            x$locType <- x$locType[loctype_select]
            x$Time <- x$Time[loctype_select]
            return(x)
          }
)