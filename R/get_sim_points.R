#' SpatialPoints from a list of Simulated Tracks
#'
#' @param sim_tracks simulated tracks returned from get_sim_tracks()
#' @param locType which locTypes to filter
#' @param crs a valid CRS object that will be associated with returned SPDF
#'
#' @return a SpatialPointsDataFrame containing all points from sim_tracks
#' @export
#'
get_sim_points <- function(sim_tracks, locType = c('p','o'), crs) {

  sim_tracks <- lapply(sim_tracks,
                      function(x) {
                        if (inherits(x,"crwIS")) {
                          crawlr::as.data.frame(x)
                          } else if (inherits(x,"data.frame")) {
                            x
                          }
                      }
                            )

  simPoints <- dplyr::bind_rows(sim_tracks) %>%
    dplyr::filter(locType %in% locType)

  sp::coordinates(simPoints) <- ~mu.x+mu.y
  if (!missing(crs)) {
    sp::proj4string(simPoints <- crs)
  }
  return(simPoints)
}