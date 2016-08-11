#' Get optimal land mask raster resolution
#'
#' the \code{crawl} package includes a function \code{fix_path} that will
#' adjust a movement path to move around a restricted area (e.g. land if
#' the animal is a fish or marine mammal). The resolution of this land
#' mask is an important parameter and, often, is related to the
#' spatial scale of the movement. Higher resolution has a significant cost
#' in compute time and memory requirements. So, this function uses a simple
#' rule to determine an optimal resolution for this land mask. The highest
#' resolution is 100 m per side for any region with a max dimension less
#' than or equal to 25 km. Above a max dimension of 25 km, the resolution
#' is determined by dividing the max dimension by 1000 and multiplying by 4
#'
#' @param pts a SpatialPoints, or SpatialPointsDataFrame
#'
#' @return numeric value indicating the square cell dimension in meters
#' @export
#'
get_optimal_res <- function(pts) {
  x_len <- diff(pts@bbox['mu.x',])[[1]]
  y_len <- diff(pts@bbox['mu.y',])[[1]]

  max_len <- max(x_len, y_len)

  res <- ifelse(max_len >= 25000, (max_len/1000) * 4, 100)
  return(res)
}