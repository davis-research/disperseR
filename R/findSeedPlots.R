#' Finding All Possible Seedling Plots In An Area
#'
#' This function searches a dataframe of seedlings and returns a list of (x,y)
#' coordinates where they are clumped together. The individuals entered into
#' this function must be of the same species, and the data.frame must contain an
#' "x" column, a "y" column, and a "treeid" column. The former two must be
#' numeric or integers.
#'
#' @param seedlings This is a data.frame with "x", "y", and "treeid" columns,
#'   representing individual seedlings' geographic location and a unique
#'   identifier.
#' @param m This is the search radius to search around each seedling to find its
#'   neighbors. The search parameter is a square, not a circle.
#'
#' @return This function returns a response data.frame with columns "x" and "y"
#'   to represent geographic location of the center of the box, and "n" with a
#'   number of seedlings found in that box.
#'
#' @note This function follows these steps: Starting with the first row, it
#'   determines an m-bounded box around the (x,y) coordinates given. All
#'   individuals in the box (via a subset of the seedlings data.frame) are
#'   counted, and then removed from future searches to prevent double-counts.
#'   This algorithm may find slightly different results depending on the order
#'   of the seedlings in your data.frame. Plan accordingly. This function does
#'   not minimize the number of boxes drawn.
#' @examples
#' ## make a sample seed data.frame by subsetting the included
#' ## expandedTrees data.frame..
#' sampleseeds <- expandedTrees[expandedTrees$plot=="trinity" &
#'                              expandedTrees$measyear==2001 &
#'                              expandedTrees$stage=="seedling",]
#' ## show the start of results
#' head(findSeedPlots(sampleseeds, 1))
#' @export

findSeedPlots <- function(seedlings, m){
  responsetable <- data.frame(x=NA, y=NA, n=NA)
  seedlings$used <- 0
  for(i in 1:nrow(seedlings)){
    ## variable declarations

    count <- 1
    x <- seedlings[i, "x"]
    y <- seedlings[i, "y"]


    closeSeeds <- seedlings[seedlings$x > (x-m) & seedlings$x < (x+m) & seedlings$y > (y-m) & seedlings$y < (y+m) & seedlings$used==0,]
    count <- nrow(closeSeeds)
    seedlings[seedlings$treeid %in% closeSeeds$treeid,"used"] <- 1

    responsetable <- rbind(responsetable, c(x, y, count))
  }

  responsetable <- responsetable[responsetable$n > 0,]
  return(cleanResponse(responsetable, 1))
}
