### R code from vignette source 'disperseRmanual.Rnw'

###################################################
### code chunk number 1: disperseRmanual.Rnw:43-47
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
### code chunk number 3: disperseRmanual.Rnw:65-78
###################################################

## make a sample seed data.frame by subsetting the included
## expandedTrees data.frame..
seeds <- myplot[myplot$stage=="seedling",]

## we'll need this one later...
adults <- myplot[myplot$stage=="tree",]
## show the start of results
myseedplots <- findSeedPlots(seeds, 1)
head(myseedplots)

## What are the possible densities of our seedplots?
unique(myseedplots$n)


###################################################
### code chunk number 4: disperseRmanual.Rnw:89-92
###################################################
parentTrees <- findAdultTrees(myseedplots, adults, 20)
head(parentTrees)
nrow(parentTrees)


###################################################
### code chunk number 5: disperseRmanual.Rnw:105-135
###################################################
head(expandedTrees)
str(expandedTrees)

## get unique plot/year combos
plotlist <- unique(expandedTrees[,c("plot", "measyear")])
rownames(plotlist) <- 1:nrow(plotlist)

## count the number of adult trees in a plot/year combination
plotlist$tree <- NA
for(i in 1:nrow(plotlist)){
  plotlist[i, "tree"] <- nrow(
                          expandedTrees[
                           expandedTrees$plot==plotlist[i, "plot"] &
                           expandedTrees$measyear==plotlist[i, "measyear"] &
                           expandedTrees$stage=="tree",])
}


## get number of seedlings in a plot/year combo
plotlist$seedlings <- NA
for(i in 1:nrow(plotlist)){
  plotlist[i, "seedlings"] <- nrow(
                               expandedTrees[
                               expandedTrees$plot==plotlist[i, "plot"] &
                               expandedTrees$measyear==plotlist[i, "measyear"] &
                               expandedTrees$stage=="seedling",])
}

## eliminate any plots that have only trees or seedlings
plotlist <- plotlist[plotlist$tree!=0 & plotlist$seedlings!=0,]


###################################################
### code chunk number 6: disperseRmanual.Rnw:140-154
###################################################
trinity01 <- expandedTrees[
              expandedTrees$plot=="trinity" &
              expandedTrees$measyear==2001,]
nrow(trinity01[trinity01$stage=="seedling",])
str(trinity01)

## set up for plot
## by stage
trinity01$colors <- ifelse(trinity01$stage=="seedling", "red", "blue")
## by species
specieslist <- unique(trinity01$species)
for(i in 1:length(specieslist)){
  trinity01[trinity01$species==specieslist[i],"pch"] <- as.numeric(i)
}


###################################################
### code chunk number 7: disperseRmanual.Rnw:160-161
###################################################
plot(trinity01$x, trinity01$y, pch=trinity01$pch, col=trinity01$colors)


###################################################
### code chunk number 8: disperseRmanual.Rnw:167-180
###################################################
## get seeds and adults ready
trinSeeds <- trinity01[trinity01$stage=="seedling" &
                         trinity01$species=="ABCO", ]
trinAdults <- trinity01[trinity01$stage=="tree" &
                         trinity01$species=="ABCO", ]

seedlingPlots <- findSeedPlots(trinSeeds, 1)
parentTrees <- findAdultTrees(seedlingPlots, trinAdults, 20)

## check that there are multiple seedling densities,
## and parentTrees looks right.
str(parentTrees)
unique(parentTrees$ri)


###################################################
### code chunk number 9: disperseRmanual.Rnw:199-200
###################################################
formula <- "log(ri)~log(dbh/30) + m^3"


###################################################
### code chunk number 10: disperseRmanual.Rnw:207-209
###################################################
myModel <- glm(formula, data=parentTrees)
summary(myModel)


###################################################
### code chunk number 11: disperseRmanual.Rnw:214-222
###################################################
STR <- exp(myModel$coefficients[1])
beta <- myModel$coefficients[2]
D <- -myModel$coefficients[3]

STR
beta
D



###################################################
### code chunk number 12: disperseRmanual.Rnw:235-244
###################################################
bellow <- expandedTrees[expandedTrees$plot=="bellow",]

## make a color column, set it to "black"
  bellow$col <- "black"
## Set the leftmost points, the points we're rotating on, to red
  bellow[bellow$x==min(bellow$x), "col"] <- "red"

## Rotate plot
  rotatebellow <- rotatePlot(bellow)


###################################################
### code chunk number 13: disperseRmanual.Rnw:248-253
###################################################
## plot
par(mfrow=c(2,1))
plot(bellow$x, bellow$y, col=bellow$col)
plot(rotatebellow$x, rotatebellow$y, col=rotatebellow$col)
par(mfrow=c(1,1))


###################################################
### code chunk number 14: disperseRmanual.Rnw:258-263
###################################################

rainbowColors <- rainbow(max(bellow$subplot))
palette(rainbowColors)
plot(rotatebellow$x, rotatebellow$y, col=rotatebellow$subplot)



