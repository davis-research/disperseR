#' Find SeedTrap Locations
#'
#' This function is imported from other authors. It will be updated with function soon enough.
#'
#'
#' @export

trapUTM <- function(filename,subplots,site,bearing=0,plotcorns=list(F)) {
  # filename = the filename with directory path for the csv of the GIS attribute
  # table; subplots = the subplots that have the traps in them site = the site
  # name; bearing = the approx bearing of the site #### NOT WORKING FOR OTHERS
  # THAN NORTH!! ; plotcorns = a list of a logical toggle to print a map of the
  # seed trap locations and the path/filename of the figure

  ### STILL TO DO:
  #   -- fix for a non-northfacing plot
  #   -- make a function for find the directional corners based on the bearing
  #   -- do the geometry for adding the offset
  #   -- look through all the files in the "Seedling_reloc_details" folder to add
  #        irregularities to the each plot (note: SeedlingPlotInfo.xls matches up
  #        the old names to the new)

  # save the default options, then set stringsAsFactors to False
  holdopt <- options()["stringsAsFactors"]; options(stringsAsFactors=F)

  # import the polygon corners
  pcor <- read.csv(file=filename,header=T,as.is=T)

  # get the subplot length
  spl <- length(subplots)

  ## set up response data.frame
  outdat <- data.frame(
    ID=rep("none",9*spl),
    PLOT_NAME=rep(site,9*spl),
    SUBPLOT=rep(subplots,each=9),
    TRAP=rep(1:9,spl),
    XCoord=rep(-99.99,9*spl),
    YCoord=rep(-99.99,9*spl))

  ##make the ID column
  outdat$ID <- paste(outdat$PLOT_NAME,outdat$SUBPLOT,outdat$TRAP,sep="-")

  ##set up other output variables
  allcns <- data.frame()
  allposc <- c()


  ##for each subplot
  for (i in subplots) {
    ## extract the subplot corners from GIS output file
    cns <- pcor[which(pcor$Subplot==i),c("POINT_X","POINT_Y")]
    colnames(cns) <- c("x","y")

    # get rid of the duplicated point --
    # the GIS polygon repeats the first and
    # last point
    cns <- cns[!duplicated(cns),]
    allcns <- rbind(allcns,cns)

    # find the minimum and maximum corner points
    minx <- min(cns$x)
    miny <- min(cns$y)
    maxx <- max(cns$x)
    maxy <- max(cns$y)

    # make a vector that knows the corners -- CHANGE THIS IF bearing !=0
    posc <- rep(0,4)

    ##find each corner, sw=swc, etc.
    swc <- which(cns$x < minx+1 & cns$y < miny+1)
    posc[swc] <- "swc"

    sec <- which(cns$x > cns$x[swc] & cns$y < miny+1)
    posc[sec] <- "sec"

    nwc <- which(cns$x < minx+1 & cns$y > cns$y[swc])
    posc[nwc] <- "nwc"

    nec <- which(cns$x > cns$x[swc] & cns$y > cns$y[swc])
    posc[nec] <- "nec"

    allposc <- c(allposc,posc)

    ### OFFSETS need to be CHANGED if bearing != 0
    # trap 1: swc corner
    # ang <- # find angle of line and muliply cos/sin the offset
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==1)] <- cns$x[
        which(posc=="swc")]-0.25
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==1)] <- cns$y[
        which(posc=="swc")]-0.25

    # trap 2: side between swc & nwc
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==2)] <- mean(c(
        cns$x[which(posc=="swc")],
        cns$x[which(posc=="nwc")])
      )-0.25
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==2)] <- mean(c(
        cns$y[which(posc=="swc")],
        cns$y[which(posc=="nwc")])
      )

    # trap 3: nwc corner
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==3)] <- cns$x[
        which(posc=="nwc")]-0.25
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==3)] <- cns$y[
        which(posc=="nwc")]+0.25

    # trap 4: side between swc and sec
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==4)] <- mean(c(
        cns$x[which(posc=="swc")],
        cns$x[which(posc=="sec")])
      )
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==4)] <- mean(c(
        cns$y[which(posc=="swc")],
        cns$y[which(posc=="sec")])
      )-0.25

    # trap 5: center
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==5)] <- mean(c(
        cns$x[which(posc=="swc")],
        cns$x[which(posc=="nec")])
      )
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==5)] <- mean(c(
        cns$y[which(posc=="swc")],
        cns$y[which(posc=="nec")])
      )

    # trap 6: side between nwc and nec
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==6)] <- mean(c(
        cns$x[which(posc=="nwc")],
        cns$x[which(posc=="nec")])
      )
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==6)] <- mean(c(
        cns$y[which(posc=="nwc")],
        cns$y[which(posc=="nec")])
      )+0.25

    # trap 7: sec corner
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==7)] <- cns$x[
        which(posc=="sec")]
    +0.25
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==7)] <- cns$y[
        which(posc=="sec")]
    -switch(site,
            "PGABMA"=ifelse(i==6,1.8,0.25),0.25)

    # trap 8: side between sec and nec
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==8)] <- mean(c(
        cns$x[which(posc=="sec")],
        cns$x[which(posc=="nec")])
      )+0.25
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==8)] <- mean(c(
        cns$y[which(posc=="sec")],
        cns$y[which(posc=="nec")])
      )

    # trap 9: nec corner
    outdat$XCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==9)] <- cns$x[
        which(posc=="nec")]
    +0.25
    outdat$YCoord[
      which(outdat$SUBPLOT==i & outdat$TRAP==9)] <- cns$y[
        which(posc=="nec")]
    +0.25

  }  # end for loop thru subplots
  rm(i)

  if (plotcorns[[1]]) {   # make a figure of the seed corners if T
    png(filename=plotcorns[[2]],width=960,height=960)
    plot(allcns$x,
         allcns$y,
         pch=2,
         col="white",
         main=site)
    text(allcns$x,
         allcns$y,
         labels=allposc,
         cex=0.5)

    subtrap <- paste(outdat$SUBPLOT,outdat$TRAP,sep="-")
    text(outdat$XCoord,
         outdat$YCoord,
         labels=subtrap,
         cex=0.5)
    dev.off()
  }

  options(stringsAsFactors=holdopt)   # return the options to the original

  return(outdat)
}  # end function
