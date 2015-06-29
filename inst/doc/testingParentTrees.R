## starting fresh

library(disperseR)

trinity01 <- expandedTrees[expandedTrees$plot=="trinity" & expandedTrees$measyear==2001,]
rownames(trinity01) <- trinity01$treeid

species <- unique(trinity01$species)
for(i in 1:length(species)){
  trinity01[trinity01$species==species[i],"pch"] <- i
}

trinSeeds <- trinity01[trinity01$stage=="seedling",]
trinAdults <- trinity01[trinity01$stage=="tree",]

species <- data.frame(species=unique(trinSeeds$species), count=NA, stringsAsFactors=FALSE)
for(i in 1:nrow(species)){
  species[i, "count"] <- nrow(trinSeeds[trinSeeds$species==species[i, "species"],])
}

species
## get bounds of the plot
minx <- min(trinity01$x)
maxx <- max(trinity01$x)
miny <- min(trinity01$y)
maxy <- max(trinity01$y)

## make seedling density plots
## I should cycle through each of the trinSeeds.
## For each trinseed, I should see if there are any trinSeeds within a bounding box
## If there are, I will include them in the count,
## Then remove those values (searchcounter switch from 0 to 1?) so I don't recount them
## I should get back a list of (x,y) coordinates that have N seedlings within M distance from them.
## I need to do this by species, so I'll design the function as taking a single species.

abco <- trinSeeds[trinSeeds$species=="ABCO",]
abcoTrees <- trinAdults[trinAdults$species=="ABCO",]

findSeedPlots <- function(seeds, m){
  responsetable <- data.frame(x=NA, y=NA, n=NA)
  seeds$used <- 0
  for(i in 1:nrow(seeds)){
    ## variable declarations

    count <- 1
    x <- seeds[i, "x"]
    y <- seeds[i, "y"]


    closeSeeds <- seeds[seeds$x > (x-m) & seeds$x < (x+m) & seeds$y > (y-m) & seeds$y < (y+m) & seeds$used==0,]
    count <- nrow(closeSeeds)
    seeds[seeds$treeid %in% closeSeeds$treeid,"used"] <- 1

    responsetable <- rbind(responsetable, c(x, y, count))
  }

  responsetable <- responsetable[responsetable$n > 0,]
  return(cleanResponse(responsetable, 1))
}

abcoSeeds <- findSeedPlots(abco, 1)
plot(abco$x, abco$y, pch=".")
symbols(abcoSeeds$x, abcoSeeds$y, squares=rep(1, nrow(abcoSeeds)), inches=FALSE, add=TRUE, fg="red")
points(trinAdults[trinAdults$species=="ABCO", "x"], trinAdults[trinAdults$species=="ABCO", "y"], pch="*", col="blue")
symbols(abcoSeeds$x, abcoSeeds$y, squares=rep(20, nrow(abcoSeeds)), inches=FALSE, fg="gray", lty="dotted", add=TRUE)


## ok, now we need to get a data.frame of all possible adults within range of the seedling plots.
findAdultTrees <- function(seedPlots, adults, m){
 ## initialize response table
  responsetable <- data.frame(treeid=NA, ri=NA, m=NA, dbh=NA, stringsAsFactors=FALSE)

  ##convert row numbers to treeids.
  rownames(adults) <- adults$treeid

  ## for each seedling plot...
  for(i in 1:nrow(seedPlots)){
    distmat <- as.matrix(
      dist(rbind(seedPlots[i,c("x", "y")], adults[,c("x", "y")]),
        diag=FALSE), rownames.force=TRUE)
    close <- distmat[distmat[,1]<=m & distmat[,1]>0,1]
    treeids <- as.numeric(names(close))
    print(head(treeids))
    dbhs <- adults[charmatch(treeids, adults$treeid),"dbh"]
    newdata <- data.frame(treeid=treeids, ri=seedPlots[i, "n"], m=unname(close), dbh=dbhs, stringsAsFactors=FALSE)

    responsetable <- rbind(responsetable, newdata)
  }
  return(cleanResponse(responsetable,1))

}
adults <- findAdultTrees(abcoSeeds, abcoTrees, 20)
head(adults)
adults[adults$treeid==1532,]
adults[adults$treeid==2650,]
adults[adults$treeid==5093,]

## now, to estimate parameters, we need to do some R magic. NLS() is pretty hard
## to converge, but we can convert it into a linear formula with a log
## transformation. For DBH, we're going to let the model estimate Beta, because
## it's easiest. In contrast, we'll keep Theta at 3 and transform it before we
## enter it into the model.

## standardize DBH without Beta
adults$stdDBH <- (adults$dbh/30)

## standardize m with Theta
adults$m3 <- adults$m^3


## ok, time to try the equation
formula <- "log(ri)~log(stdDBH) + m3"
store <- glm(formula, data=adults)
summary(store)
## yay, we have a model! Now, let's make sense of it. If we raise the intercept like so, e^i, that will convert it back to the true STR value.
STR <- exp(store$coefficients[1])

##Now the parameter for the log(dbh) is the exponent of the equation, or Beta.
Beta <- store$coefficients[2]

## And the parameter for m3 is -D
D <- -store$coefficients[3]
