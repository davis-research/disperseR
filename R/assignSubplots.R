#' Assigns the Correct Subplot Identity to Subplots
#'
#' This is a helper function to clean up the result from buildBoxes. It'll be
#' wrapped up eventually.
#'
#' @param df This is the major trees data.frame. It holds individual tree
#'   records, like in expandedTrees.
#' @param subplotdf This is the data.frame generated by buildBoxes, it has
#'   POINT_X, POINT_Y, and Subplot; Subplot will be overwritten by this
#'   function.
#' @return This function returns a copy of subplotdf, with new values in
#'   Subplot.
#'
#' @export

assignSubplots <- function(df, subplotdf){
  subplots <- unique(subplotdf$Subplot)


  ## for each subplot ID currently (1:16)
  for(i in 1:length(subplots)){
    ## only get the subplots with that identity
    reducedSub <- subplotdf[subplotdf$Subplot==subplots[i],]
    ## get the corners
    minx <- min(reducedSub$POINT_X)
    maxx <- max(reducedSub$POINT_X)
    miny <- min(reducedSub$POINT_Y)
    maxy <- max(reducedSub$POINT_Y)

    ## get the reduced other Df
    reducedDf <- df[df$x > minx &
                      df$x < maxx &
                      df$y > miny &
                      df$y < maxy,]

    ## if there's more than one subplot in the designation...
    if(length(unique(reducedDf$subplot))>1){
      uniquesubs <- unique(reducedDf$subplot)
      #print(paste("Unique SubPlots:", uniquesubs))
      ## set the first record as a possible winner
      winner <- 0
      counts <- vector()
      for(j in 1:length(uniquesubs)){
        ##first time, winner=0, should eval to true
        counted <- nrow(reducedDf[reducedDf$subplot==uniquesubs[j],])
        if(counted > winner){
          winner <- counted
          majority <- uniquesubs[j]
        }
        counts <- c(counts, counted)
      }
      subplotdf[subplotdf$Subplot==subplots[i], "newsub"] <- majority
      #print(counts)
    } else{
      ## there's only one subplot, we're golden.
      if(length(unique(reducedDf$subplot))==1){
        subplotdf[subplotdf$Subplot==subplots[i],"newsub"] <- unique(
          reducedDf$subplot)
      } else{
        subplotdf[subplotdf$Subplot==subplots[i],"newsub"] <- "X"
      }

    }
  }
  subplotdf$Subplot <- subplotdf$newsub
  final <- subplotdf[, c("POINT_X", "POINT_Y", "Subplot")]
  return(final)
}
