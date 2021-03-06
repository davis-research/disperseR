% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/getParentTrees.R
\name{getParentTrees}
\alias{getParentTrees}
\title{Get Parent Trees For Subplots}
\usage{
getParentTrees(subplotdf, fulldf, searchradius = 20.5)
}
\arguments{
\item{subplotdf}{This is a data.frame which contains 'x', 'y', and 'species'
columns. These are the places to search the full data.frame for parent
trees.}

\item{fulldf}{This is the full data.frame which catalogues each tree in a
plot.}

\item{searchradius}{This is a number, default 20.5, which represents the
  distance to search from the center of the subplot for adult trees. It
  should be the search distance plus half of the subplot width.}
}
\value{
This function returns individual records of trees that fall within
  the search radius, their x and y coordinates, dbh value, species, distance
  to subplot, and the seedling density within that subplot.
}
\description{
This function finds all possible parent trees in a radius around a subplot.
}

