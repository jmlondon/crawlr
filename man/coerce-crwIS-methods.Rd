% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coerce-crwIS-methods.R
\name{coerce-crwIS-methods}
\alias{as.SL}
\alias{as.SPDF}
\alias{as.data.frame.crwIS}
\alias{coerce-crwIS}
\alias{coerce-crwIS-methods}
\alias{crwIS-class}
\title{Coerce \code{"crwIS"} to sp classes}
\usage{
as.SPDF(crwISobj)

as.SL(crwISobj)

\method{as.data.frame}{crwIS}(crwISobj)
}
\arguments{
\item{crwISobj}{an object of class \code{"crwIS"},}

\item{crwISobj}{an object of class \code{"crwIS"},}

\item{crwISobj}{an object of class \code{"crwIS"},}
}
\description{
Package \pkg{crawlr} implements \code{coerce}-methods (i.e,
(\code{as(object, Class)})) to convert \code{"crwIS"} S3 objects from
the \code{crawl} package to
\code{"\linkS4class{SpatialPointsDataFrame}"} or
\code{"\linkS4class{SpatialLines}"}
or \code{"data.frame"}.

We will register the S3 'crwIS' class from crawl as a formally
defined class to we can create the
}
\author{
Josh M. London
}

