#' Get Parent Trees For Subplots
#'
#' This function finds all possible parent trees in a radius around a subplot.
#'
#' @param subplotdf This is a data.frame which contains 'x', 'y', and 'species'
#'   columns. These are the places to search the full data.frame for parent
#'   trees.
#' @param fulldf This is the full data.frame which catalogues each tree in a
#'   plot.
#' @param searchradius This is a number, default 20.5, which represents the
#'   distance to search from the center of the subplot for adult trees. It
#'   should be the search distance plus half of the subplot width.
#'
#' @return This function returns individual records of trees that fall within
#'   the search radius, their x and y coordinates, dbh value, species, distance
#'   to subplot, and the seedling density within that subplot.
#'
#' @export

getParentTrees <- function(subplotdf, fulldf, searchradius=20.5){
  ## initialize responsetable
    responsetable <- data.frame(subplotx=NA,
                             subploty=NA,
                             species=NA,
                             numseedlings=NA,
                             m=NA,
                             dbh=NA,
                             treex=NA,
                             treey=NA)

  ## start looping through each subplot
    for(i in 1:nrow(subplotdf)){

      boundaries <- getBoxBoundaries(subplotdf[i, "x"],
                                     subplotdf[i, "y"],
                                     searchradius)

      subsettedDf <- fulldf[which(fulldf$x > boundaries[1] &
                                    fulldf$x < boundaries[2] &
                                    fulldf$y > boundaries[3] &
                                    fulldf$y < boundaries[4] &
                                    fulldf$species == subplotdf[i, "species"] &
                                    !is.na(fulldf$dbh)
                                    ),]
      ## if there are records
      if(nrow(subsettedDf) > 0){
        ## loop through adult trees
        for(j in 1:nrow(subsettedDf)){
       xymat <- matrix(data=c(subplotdf[i, "x"],
                       subsettedDf[j, "x"],
                       subplotdf[i, "y"],
                       subsettedDf[j, "y"]), nrow=2)

       ## calculate euclidian distance between plot and tree
           m <- dist(xymat)
          responsetable <- rbind(responsetable,
                                 c(subplotdf[i, "x"],
                                   subplotdf[i, "y"],
                                   subplotdf[i, "species"],
                                   subplotdf[i, "numseedlings"],
                                   m,
                                   subsettedDf[j, "dbh"],
                                   subsettedDf[j, "x"],
                                   subsettedDf[j, "y"]
                                   )
                                 )
        }

      }
    }

    ## We did this the lazy way, which means some of our distances may be larger
    ## than our true search radius. This is because we made a box, not a circle,
    ## to guarantee minimum distance. What we CAN do is trim those values that
    ## fall outside of the box off of the final table.
    responsetable <- responsetable[which(responsetable$m <= searchradius),]


    ## clean up responsetable

    return(cleanResponse(responsetable))
}
