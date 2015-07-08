### R code from vignette source 'FindingSeedTraps.Rnw'

###################################################
### code chunk number 1: FindingSeedTraps.Rnw:17-24
###################################################
load("../data/plotinfo.RData")
load("../data/seedlings.RData")
load("../data/treeinfo.RData")

head(plotinfo)
head(seedlings)
head(treeinfo)


###################################################
### code chunk number 2: FindingSeedTraps.Rnw:29-33
###################################################
#install.packages("devtools")
library(devtools)
#install_github("ecology-rocks/disperseR")
library(disperseR)


###################################################
### code chunk number 3: FindingSeedTraps.Rnw:50-94
###################################################
## separate out bbbpipo
  bbbpipo <- treeinfo[treeinfo$PLOT=="BBBPIPO",]
  colnames(bbbpipo) <- c("plot", "subplot", "tag", "spp",
                         "ingrowth", "firstrec", "deathyear", "dbh1",
                         "dbh2", "dbh3", "dbh4", "dbh5",
                         "dbh6", "dbh7", "x", "y")


## pull out the plot origin from plotinfo
bbbpipoOrigin <- data.frame(x=plotinfo[plotinfo$Plot=="BBBPIPO", "UTME"],
                            y=plotinfo[plotinfo$Plot=="BBBPIPO", "UTMN"],
                            stringsAsFactors=FALSE)
bbbpipoOrigin

## In this example, you can rotate the plot and set the origin to the trueSW
## values. I'm doing it this way to show how the plot is rotated. This function
## takes your large tree dataframe (bbbpipo), and your "true southwestern corner"
## coordinates as a vector.
rotateBBB <- rotatePlot(bbbpipo,
                        truesw=c(bbbpipoOrigin[1,1],
                                 bbbpipoOrigin[1,2]),
                        origin=c(bbbpipoOrigin[1,1],
                                 bbbpipoOrigin[1,2]))

## check our work
plot(rotateBBB$x,
     rotateBBB$y,
     xlim=c(min(rotateBBB$x),
            max(bbbpipo$x)),
     ylim=c(min(bbbpipo$y),
            max(rotateBBB$y)),
     col="red",
     pch=4)
points(bbbpipo$x,
       bbbpipo$y,
       col="black",
       pch=2)

## Let's overwrite rotateBBB and let the origin be the default, c(0,0), for ease
## of reading and seeing later on. This is just changing the values of (x,y) by
## a uniform amount.
rotateBBB <- rotatePlot(bbbpipo,
                        truesw=c(bbbpipoOrigin[1,1],
                                 bbbpipoOrigin[1,2]))


###################################################
### code chunk number 4: FindingSeedTraps.Rnw:99-108
###################################################
## set our min and max values
corners <- c(min(rotateBBB$x),
             max(rotateBBB$x),
             min(rotateBBB$y),
             max(rotateBBB$y))

## make sure the plot is actually 100x100
corners[2]-corners[1]
corners[4]-corners[3]


###################################################
### code chunk number 5: FindingSeedTraps.Rnw:116-123
###################################################
## get coords
  xcoords <- getSubplotCoords(corners[1:2], increment=25)
  ycoords <- getSubplotCoords(corners[3:4], increment=25)

##make sure the values are right...
  xcoords
  ycoords


###################################################
### code chunk number 6: FindingSeedTraps.Rnw:128-136
###################################################


bbbpipoSubs <- buildBoxes(xcoords, ycoords)

  ## check our work
plot(rotateBBB$x, rotateBBB$y, pch=".")
points(bbbpipoSubs$POINT_X, bbbpipoSubs$POINT_Y, col="red", pch=4)



###################################################
### code chunk number 7: FindingSeedTraps.Rnw:149-156
###################################################
## look at results
finalsubplots <- assignSubplots(rotateBBB, bbbpipoSubs)

##take a look
head(finalsubplots)
tail(finalsubplots)
##write.csv(finalsubplots, file="bbbpipo-subplots.csv")


###################################################
### code chunk number 8: FindingSeedTraps.Rnw:161-171
###################################################
rainbowColors <- rainbow(max(rotateBBB$subplot))
palette(rainbowColors)
plot(rotateBBB$x,
     rotateBBB$y,
     col=rotateBBB$subplot,
     pch=".")
points(finalsubplots$POINT_X,
       finalsubplots$POINT_Y,
       col=finalsubplots$Subplot,
       pch=c("x", ".", ".", "."))


###################################################
### code chunk number 9: FindingSeedTraps.Rnw:178-180
###################################################
subplotsForTraps <- unique(seedlings[seedlings$PLOT_NAME=="BBBPIPO", "SUBPLOT"])
subplotsForTraps


###################################################
### code chunk number 10: FindingSeedTraps.Rnw:185-194
###################################################
trapcoords <- trapUTM(
  filename="../data/BBBPIPO_subpolygon_corners.csv",
  subplots=subplotsForTraps,
  site="BBBPIPO",
  bearing=0,
  plotcorns=list(T,paste("../data/","BBBPIPO","_traps.png",sep="")))

plot(rotateBBB$x, rotateBBB$y, pch=".")
points(trapcoords$XCoord, trapcoords$YCoord, col="red")


