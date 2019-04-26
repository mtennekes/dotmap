#' Dot Maps
#'
#' Dot Maps are maps where spatial data points are visualized as (colored) dots.
#' 
#' @name dotmap-package
#' @aliases dotmap
#' @docType package
#' @author Martijn Tennekes \email{mtennekes@@gmail.com}
#' @keywords GIS, dot map
NULL

#' Residential land use areas of the Netherlands.
#' 
#' Residential land use areas of the Netherlands in two maps: \code{NLD_area1} are areas where people live, \code{NLD_area2} are areas where people might live, e.g. above shops or in industrial areas. For the dot map, \code{NLD_area1} is used as the primary source. When the population density in those areas reaches a certain threshold, \code{NLD_area2} can be used to place the remaining dots.
#' 
#' \code{NLD_area1} Areas in the Netherlands where people live
#' 
#' \code{NLD_area2} Areas in the Netherlands where people might live, e.g. above shops or in industrial areas.
#' 
#' @usage data(NLD_area1)
#' @name NLD_area1
#' @rdname Shapes
#' @docType data
NULL

#' @usage data(NLD_area2)
#' @name NLD_area2
#' @rdname Shapes
#' @docType data
NULL

#' Population tables
#' 
#' Population tables for age, gender, and origin.
#' 
#' @usage data(NLD_age)
#' @name NLD_age
#' @rdname PopulationTables
#' @docType data
NULL

#' @usage data(NLD_gender)
#' @name NLD_gender
#' @rdname PopulationTables
#' @docType data
NULL

#' @usage data(NLD_origin)
#' @name NLD_origin
#' @rdname PopulationTables
#' @docType data
NULL
