#' Create Hexagonal Utilization Distribution Polygons
#'
#' @param data_sp SpatialPointsDataFrame of regular locations
#' @param cellsize cellsize parameter passed to spsample
#' @param leaflet should the output be converted to EPSG:4326 for leaflet
#' @param cellsize.override whether to override cellsize specified value would result in poor performance
#'
#' @return a SpatialPolygonsDataFrame
#' @export
#'
#' @examples
#' # library(crawl)
# library(dplyr)
# library(leaflet)
# library(sp)
# library(rgdal)
# library(viridis)
#
# data(harborSeal)
#
# spPoints<- harborSeal %>% filter(!is.na(latitude)) %>%
#   arrange(Time)
#
# coordinates(spPoints) <- ~longitude+latitude
# proj4string(spPoints) <- CRS("+init=epsg:4326")
#
# spPoints <- spTransform(spPoints, CRS("+init=epsg:3338"))
#
# spPolys <- ud_hexpolys(spPoints, cellsize = 5000, leaflet = TRUE) %>%
#   subset(count > 0)
#
# pal <- colorNumeric(viridis(16), domain = NULL)
#
# leaflet(spPolys) %>% addProviderTiles("Esri.OceanBasemap") %>%
#   addPolygons(
#     fillOpacity = 0.75,
#     color = 'white',
#     fillColor = ~ pal(log(count)),
#     weight = 1
#   )
ud_hexpolys <- function(data_sp, study_area, cellsize, leaflet = FALSE,
                        density = TRUE,
                        cellsize.override = TRUE) {

  if (missing(study_area)) {
    bb <- bbox(data_sp)
    max_dim <- max(bb["mu.x","max"] - bb["mu.x","min"],
                   bb["mu.y","max"] - bb["mu.y","min"])

    if (max_dim/cellsize > 150) {
      if (cellsize.override) {
        cellsize = 1250
        warning (
          paste("cellsize of", cellsize, "will result in poor",
                 "performance. setting to 1250 meters. if you want",
                 "a higher resolution, specify cellsize.override = FALSE."
                )
        )
      }
    }

    set.seed(seed)

    hex_polys <- sp::spsample(data_sp,
                              cellsize = cellsize,
                              type = "hexagonal",
                              offset = c(0, 0))
    hex_polys <- sp::HexPoints2SpatialPolygons(hex_polys)
  }

  if (!missing(study_area)) {
    if (is.na(proj4string(study_area))) {
      stop("proj4string for study_area is NA")
    }

    hex_polys <- sp::spsample(study_area,
                              cellsize = cellsize,
                              type = "hexagonal",
                              offset = c(0, 0))
    hex_polys <- sp::HexPoints2SpatialPolygons(hex_polys)
  }

  hex_over <- over(data_sp,hex_polys)

  count_table <- data.frame(
    table(factor(hex_over, levels = 1:length(hex_polys@polygons))),
    row.names = sapply(hex_polys@polygons, function(x) x@ID)
    )
  count_table <- count_table[,"Freq", drop = FALSE]
  colnames(count_table) <- "counts"
  out_polys <- SpatialPolygonsDataFrame(hex_polys, count_table,
                                        match.ID = TRUE)

  # pts_poly_count <- GISTools::poly.counts(data_sp, hex_polys)

  if (density) {
    out_polys$density <- out_polys$counts/nrow(data_sp@data)
  }

  if (leaflet) {
    out_polys <- spTransform(out_polys, CRS("+init=epsg:4326"))
  }
  return(out_polys)
}