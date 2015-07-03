### R code from vignette source 'PlotYearManipulation.Rnw'

###################################################
### code chunk number 1: PlotYearManipulation.Rnw:26-54
###################################################
## load library
library(disperseR)


##subsetting by years sampled
sevenyears <- ssdAllTrees[ssdAllTrees$plot=="bellow" |
                            ssdAllTrees$plot=="palate" |
                            ssdAllTrees$plot=="reclusive" |
                            ssdAllTrees$plot=="octane" |
                            ssdAllTrees$plot=="artist",]

sixyears <- ssdAllTrees[ssdAllTrees$plot=="trigger" |
                            ssdAllTrees$plot=="rigid",]

fiveyears <- ssdAllTrees[ssdAllTrees$plot=="crackers" |
                            ssdAllTrees$plot=="trinity" |
                            ssdAllTrees$plot=="realtor" |
                            ssdAllTrees$plot=="sodium" |
                            ssdAllTrees$plot=="distress" |
                            ssdAllTrees$plot=="gravy" |
                            ssdAllTrees$plot=="chestnut",]

## check to make sure all trees are accounted for
(nrow(sevenyears) + nrow(sixyears) + nrow(fiveyears) == nrow(ssdAllTrees))

## delete the extraneous columns from sixyears and fiveyears
fiveyears <- fiveyears[,-c(15,16)]
sixyears <- sixyears[,-16]


###################################################
### code chunk number 2: PlotYearManipulation.Rnw:64-195
###################################################
## get values where ingrowthyear has a value, for testing
samplerows <- head(ssdAllTrees[!is.na(ssdAllTrees$ingrowthyear),])
## add in rows that have no ingrowth year
samplerows <- rbind(samplerows,
                    head(ssdAllTrees[is.na(ssdAllTrees$ingrowthyear),]))
##add in rows that have no ingrowthyear but DO have a mortality year
samplerows <- rbind(samplerows,
                    head(ssdAllTrees[is.na(ssdAllTrees$ingrowthyear) &
                                       !is.na(ssdAllTrees$mortalityyear),]))

## make the function

processTreeRows <- function(samplerows, ssdPlotDesc, ssdAllTrees){
i <- 1
j <- 1

    finalrows <- data.frame(plot="dummy",
                            subplot="dummy",
                            treeid="dummy",
                            species="dummy",
                            ingrowth="dummy",
                            firstrec="dummy",
                            deathyear="dummy",
                            x="dummy",
                            y="dummy",
                            measyear="dummy",
                            dbh="dummy",
                            stage="dummy",
                            stringsAsFactors=FALSE
                            )
for(i in 1:nrow(samplerows)){
  ##get plot measurement years
    plotYears <- (ssdPlotDesc[ssdPlotDesc$plot==samplerows[i, "plot"], 5:11])
  ## make a dataframe of dbh1-7 and year values to search later
    rPlotYears <- as.data.frame(cbind(paste("dbh", 1:7, sep=""),
                                      as.numeric(plotYears[1,])),
                                stringsAsFactors=FALSE,
                                na.string="NA")
  ## clean it up a bit
    colnames(rPlotYears) <- c("colname", "year")
    ##get only years where there are acutally values
      years <- plotYears[!is.na(plotYears)]

  ## if there is an ingrowth year in this particular row...
  if(!is.na(samplerows[i,"ingrowthyear"])){
    ## remove any years that are less than that.
      years <- years[years >= samplerows[i, "ingrowthyear"]]
    ## remember that this row is a seedling at least once
      class <- "seedling"
  } else{
    ## if it was NA, mark it as seedling
      if(is.na(samplerows[i, "dbh1"])){
        class <- "seedling"
      } else{
        class <- "tree"
      } ## end else
  } ##end else


  ## is there a mortalityyear
  if(!is.na(samplerows[i, "mortalityyear"])){
    ## remove any years greater than mortality year
      years <- years[years < samplerows[i, "mortalityyear"]]
    ## remember that this tree died
    ## this is currently unused, but we may need it later
      dead <- TRUE
  }
      ##if a seedling died before it could be officially measured
    if(length(years)==0){
          addedrow <- NULL
          addedrow <- data.frame(plot=samplerows[i, "plot"],
                        subplot=samplerows[i, "subplot"],
                        treeid=samplerows[i, "treeid"],
                        species=samplerows[i, "sppcode"],
                        ingrowth=samplerows[i, "ingrowthyear"],
                        firstrec=samplerows[i, "yearfirstrecorded"],
                        deathyear=samplerows[i, "mortalityyear"],
                        x=samplerows[i, "xcoord"],
                        y=samplerows[i, "ycoord"],
                        measyear=samplerows[i, "yearfirstrecorded"],
                        dbh=NA,
                        stage="seedling",
                        stringsAsFactors=FALSE
                        )

          finalrows <- rbind(finalrows, addedrow)

    } else{
      ## so, for each year that we have measurements...
      for(j in 1:length(years)){
        ##find which dbh column to use
          colname <- rPlotYears[rPlotYears$year==years[j],"colname"]
        ## clean it up because R sucks
          colname <- colname[!is.na(colname)]
        ## select that dbh value
          dbh <- as.numeric(samplerows[i, colname])
          ## make sure we don't label a tree as a seedling more than once
          if(j > 1 & class=="seedling"){
            class <- "tree"
          }
        ## build row for new table.
        addedrow <- NULL
          addedrow <- data.frame(plot=samplerows[i, "plot"],
                        subplot=samplerows[i, "subplot"],
                        treeid=samplerows[i, "treeid"],
                        species=samplerows[i, "sppcode"],
                        ingrowth=samplerows[i, "ingrowthyear"],
                        firstrec=samplerows[i, "yearfirstrecorded"],
                        deathyear=samplerows[i, "mortalityyear"],
                        x=samplerows[i, "xcoord"],
                        y=samplerows[i, "ycoord"],
                        measyear=years[j],
                        dbh=dbh,
                        stage=class,
                        stringsAsFactors=FALSE
                        )
          #print(addedrow)
          ## do the rbind
          finalrows <- rbind(finalrows, addedrow)
      } ## end j loop
    }##end seedling loop
}## end i loop
return(finalrows[-1,])
}## end function

## use the function
finalrows <- processTreeRows(samplerows, ssdPlotDesc, ssdAllTrees)

head(finalrows)
tail(finalrows)
nrow(finalrows)


###################################################
### code chunk number 3: PlotYearManipulation.Rnw:200-205
###################################################
palate <- processTreeRows(ssdAllTrees[ssdAllTrees$plot=="palate",], ssdPlotDesc, ssdAllTrees)
str(palate)
nrow(palate)
head(palate)
tail(palate)


###################################################
### code chunk number 4: PlotYearManipulation.Rnw:209-216
###################################################

## responses <- list()
## for(i in 1:length(plotnames)){ responses[[plotnames[i]]] <- processTreeRows(ssdAllTrees[ssdAllTrees$plot==plotnames[i],], ssdPlotDesc, ssdAllTrees)}
## expandedTrees <- rbind(responses[[1]], responses[[2]], responses[[3]], responses[[4]], responses[[5]], responses[[6]], responses[[7]], responses[[8]], responses[[9]], responses[[10]], responses[[11]], responses[[12]])
##
head(expandedTrees)
str(expandedTrees)


