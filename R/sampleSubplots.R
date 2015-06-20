#' Sample Seedling Subplots For Seedlings
#'
#' This function takes a list of random x and y coordinates within a plot,
#' builds a box around each point, and subsets a full plot's dataframe to see if
#' there are any seedlings inside.
#'
#' @param subplotxy A data.frame of x and y coordinates for sampling subplots
#' @param fulldf The full plot dataframe, containing all trees and seedlings
#'   within your larger plot space.
#' @param subplotsize The size of the subplot in coordinate space, default is 1.
#'
#' @export

sampleSubplots <- function(subplotxy, fulldf, subplotsize=1){

  subplotbound <- subplotsize/2
  responsetable <- data.frame(x=NA,
                              y=NA,
                              species=NA,
                              numseedlings=NA)

  ## loop through each row of subplotxy
  for(i in 1:nrow(subplotxy)){
    ## build the bounds of the sampling box

    boundaries <- getBoxBoundaries(subplotxy[i, "x"],
                                   subplotxy[i, "y"],
                                   subplotbound)

    ## subset to any values that are in the subplot
    subsettedDf <- fulldf[which(fulldf$x > boundaries[1] & fulldf$x < boundaries[2] & fulldf$y > boundaries[3] & fulldf$y < boundaries[4] & is.na(fulldf$dbh)),]
    if(nrow(subsettedDf) > 0){
      ## get the number of species left inside
      species <- unique(subsettedDf$species)
      ## for each species...
      for(j in 1:length(species)){
        responsetable <- rbind(responsetable,
                          c(subplotxy[i, "x"],
                          subplotxy[i, "y"],
                          species[j],
                          sum(is.na(subsettedDf[subsettedDf$species==species[j],
                          "dbh"]))
                            )
                          )
      } ## end species for loop

    } ##end if there are successful rows

  } ## end subplot for loop

  ## clean up responsetable

  return(cleanResponse(responsetable))
}
