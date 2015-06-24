### R code from vignette source 'disperseRmanual.Rnw'

###################################################
### code chunk number 1: disperseRmanual.Rnw:41-45
###################################################
library(disperseR)
myplot <- generatePlotMap()
head(myplot)
tail(myplot)


###################################################
### code chunk number 2: disperseRmanual.Rnw:52-56
###################################################
## exploring the structure of myplot
str(myplot)
## if we needed to convert a column
myplot$species <- as.numeric(myplot$species)


###################################################
### code chunk number 3: disperseRmanual.Rnw:64-70
###################################################
spatialDisperseDf <- data.frame(subplot=rep(1:3,5),
                                species=1,
                                numseedlings=rep(c(2,4,6), 5),
                                DBH=runif(15, 0, 100),
                                m=runif(15, 0, 20))
head(spatialDisperseDf)


###################################################
### code chunk number 4: disperseRmanual.Rnw:79-91
###################################################
randSubplots <- getBufferedPoints(x=myplot$x,
                                  y=myplot$y,
                                  buffer=20,
                                  n=250)

systSubplots <- getBufferedPoints(x=myplot$x,
                                    y=myplot$y,
                                    buffer=20,
                                    systematic=TRUE,
                                    by=15)
head(randSubplots)
head(systSubplots)


###################################################
### code chunk number 5: disperseRmanual.Rnw:99-110
###################################################
randSeedlingDensity <- sampleSubplots(randSubplots,
                                      myplot,
                                      subplotsize=25)
head(randSeedlingDensity)
str(randSeedlingDensity)

systSeedlingDensity <- sampleSubplots(systSubplots,
                                      myplot,
                                      subplotsize=10)
head(systSeedlingDensity)
str(systSeedlingDensity)


###################################################
### code chunk number 6: disperseRmanual.Rnw:119-128
###################################################
randParents <- getParentTrees(randSeedlingDensity, myplot)

systParents <- getParentTrees(systSeedlingDensity, myplot)

head(randParents)
nrow(randParents)

head(systParents)
nrow(systParents)


###################################################
### code chunk number 7: disperseRmanual.Rnw:137-146
###################################################
newplot <- myplot
newplot$species <- 1
newSeedlings <- sampleSubplots(systSubplots,
                                      newplot,
                                      subplotsize=10)
newParents <- getParentTrees(newSeedlings, newplot)

head(newParents)
unique(newParents$numseedlings)


###################################################
### code chunk number 8: disperseRmanual.Rnw:152-153
###################################################
formula <- "numseedlings~(dbh/30)^2 * exp(-m^3)"


