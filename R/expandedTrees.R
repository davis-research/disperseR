#' expandedTrees Data Frame
#'
#' This data.frame is similar to the ssdAllTrees dataframe, but is organized by plot and year instead of tree; this means that a tree has a record for each time it was measured. There are 51,493 rows of 11 columns.
#'  @format A data frame with 51,493 rows and 11 variables: \describe{
#'   \item{plot}{The plot's nickname.}
#'   \item{treeid}{A unique identifier for an individual tree.}
#'   \item{species}{A tree's species. Follows USDA PLANTS database conventions except CADE should be CADE27, QUCH should be QUCH2, and PIMO should be PIMO3.}
#'   \item{ingrowth}{The first year this tree appeared, if it appeared after plot establishment.}
#'   \item{firstrec}{The first year that this tree was measured.}
#'   \item{deathyear}{The year that this tree died.}
#'   \item{x}{The tree's x coordinate.}
#'   \item{y}{The treee's y coordinate.}
#'   \item{measyear}{The year of measurement for this record. Trees may have multiple entries with different measuremeant years.}
#'   \item{stage}{Whether the tree is a seedling (first year recorded) or tree (more than one record, or large enough when the plot was established).}
#'   }
"expandedTrees"
