#' ssdAllTrees Dataset
#'
#' This data.frame describes the trees found in plots in ssdPlotDesc. These data
#' (in both sets) are part of a larger dataset that was obfuscated for privacy.
#' You can view the transformation of ssdAllTrees into expandedTrees by looking
#' at the "PlotYearManipulation" vignette in this package.
#'
#' @format A data frame with 14 rows and 12 variables: \describe{
#'   \item{plot}{The plot's nickname.} \item{subplot}{The subplot number.}
#'   \item{tagnumber}{A tree's unique identifier.} \item{sppcode}{A tree's
#'   species. Follows USDA PLANTS database conventions except CADE should be
#'   CADE27, QUCH should be QUCH2, and PIMO should be PIMO3.}
#'   \item{ingrowthyear}{The first year this tree appeared, if it appeared after
#'   plot establishment.} \item{yearfirstrecorded}{The first year that this tree
#'   was measured.} \item{mortalityyear}{The year that this tree died.}
#'   \item{dbh1}{Diameter at breast height, in meters, during the first
#'   measurement year. See ssdPlotDesc for measurement years by plot.}
#'   \item{dbh2...7}{The numbered measurement year. See ssdPlotDesc for
#'   measurement years by plot.} \item{xcoord}{The x coordinate of the tree
#'   within its plot.} \item{ycoord}{The y coordinate of the tree within its
#'   plot.} }
"ssdAllTrees"
