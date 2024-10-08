% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcf_data.R
\name{gcf_data}
\alias{gcf_data}
\title{Contour plot from data}
\usage{
gcf_data(
  x,
  y = NULL,
  z = NULL,
  xlim = NULL,
  ylim = NULL,
  xylim = NULL,
  fit = "",
  gg = TRUE,
  ...
)
}
\arguments{
\item{x}{either just x data, x and y data, or x, y and z data}

\item{y}{either y data, z data, or null}

\item{z}{either z data or null}

\item{xlim}{x limits for the contour plot, will be set to data limits +- 5\% if not specified}

\item{ylim}{y limits for the contour plot, will be set to data limits +- 5\% if not specified}

\item{xylim}{x and y limits for the contour plot}

\item{fit}{Method to fit a model with. Current options are laGP (default)
and mlegp. laGP is faster but might cause trouble.}

\item{gg}{If FALSE, will use base graphics by calling cf_func()}

\item{...}{passed to cf_func}
}
\description{
Makes filled contour plot from data without sidebar by interpolating 
with a Gaussian process model.
This is the same as `cf_data` except it will use ggplot2 to
make the plot.
}
\examples{
x <- runif(20)
y <- runif(20)
z <- exp(-(x-.5)^2-5*(y-.5)^2)
gcf_data(x,y,z)
}
