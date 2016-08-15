#' SpatialPoints from a list of Simulated Tracks
#'
#' @param sim_tracks simulated tracks returned from get_sim_tracks()
#' @param locType which locTypes to filter
#'
#' @return a SpatialPointsDataFrame containing all points from sim_tracks
#' @export
#'
get_sim_points <- function(sim_tracks, locType = c('p','o')) {
  simPoints = lapply(sim_tracks, function(x)
                     as(x, "data.frame"))

  simPoints <- dplyr::bind_rows(simPoints) %>% filter(locType %in% locType)

  sp::coordinates(simPoints) <- ~mu.x+mu.y
  sp::proj4string(simPoints) <- sp::CRS("+init=epsg:3571")
  return(simPoints)
}