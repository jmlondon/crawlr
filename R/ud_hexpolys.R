#' Create Hexagonal Utilization Distribution Polygons
#'
#' @param data_sp SpatialPointsDataFrame of regular locations
#' @param cellsize cellsize parameter passed to spsample
#' @param leaflet should the output be converted to EPSG:4326 for leaflet
#' @param seed for consistency
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
ud_hexpolys <- function(data_sp, cellsize, leaflet = FALSE,
                        density = TRUE, seed = 123) {
  set.seed(seed)

  hex_polys <- spsample(data_sp,
                        cellsize = cellsize,
                        type = "hexagonal") %>%
    HexPoints2SpatialPolygons(.)

  pts_poly_count <- GISTools::poly.counts(data_sp, hex_polys)

  if (density) {
    out_polys <- SpatialPolygonsDataFrame(
      hex_polys,
      data.frame(density = pts_poly_count/nrow(data_sp@data))
    )
  } else {
  out_polys <- SpatialPolygonsDataFrame(
    hex_polys,
    data.frame(count = pts_poly_count), match.ID = TRUE)
  }

  if (leaflet) {
    out_polys <- spTransform(out_polys, CRS("+init=epsg:4326"))
  }
  return(out_polys)
}