% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_region_maps.R
\name{create_region_maps}
\alias{create_region_maps}
\title{Create region maps}
\usage{
create_region_maps(dm, i = NULL, j = NULL, logfile = NULL)
}
\arguments{
\item{dm}{dotmap_meta object}

\item{i}{rows. By default \code{NULL} which means all rows are taken}

\item{j}{colums. By default \code{NULL} which means all columns are taken}

\item{logfile}{logfile}
}
\description{
Create maps of the regions at the resolution of specific zoom level, divided in tiles according to the arrangement of a lower zoom level.
}
