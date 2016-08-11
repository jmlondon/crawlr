#' Get All the Simulated Tracks You Want
#'
#' @param crw_obj a crwFit object
#' @param iter the number of simulated tracks to return
#'
#' @return a list of length iter containing crwIS objects
#' @export
#'
get_sim_tracks <- function(crw_obj, iter) {
  simObj <- crawl::crwSimulator(crw_obj,
                                predTime = predTimes,
                                method = "IS",
                                parIS = 100)
  sim_tracks = list()
  for (i in 1:iter) {
    sim_tracks[[i]] <- crawl::crwPostIS(simObj, fullPost = FALSE)
    return(sim_tracks)
  }
}