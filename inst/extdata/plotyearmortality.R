## Need to calculate mortality rates
##
## Plan:::::
##   Subset all records with mortality years,
##   Remove their treeIDs from the records entirely.
##   For each year, from plot establishment to current
##      Remove any trees with a "firstrecorded" year greater than the current counting year
##      Get uniques by treeid so we have a count
##      Record plot, year, species; #alive
##      Repeat
##  For each year w/ mortality:
##      Subset by that year
##      Record plot, year, species; #dead
##      Repeat

alltrees <- expandedTrees

deadtrees <- alltrees[!is.na(alltrees$deathyear),]
deadkeys <- unique(deadtrees$treeid)

alivetrees <- alltrees[which(!(alltrees$treeid %in% deadkeys)),]
min(alivetrees$firstrec)

plotnames <- unique(alltrees$plot)
speciesnames <- unique(alltrees$species)
uniquefinal <- data.frame(plot="", species="", counts="", year="", stringsAsFactors=F)
for(i in 1982:2014){
  temptrees <- subset(alivetrees, firstrec<=i)
  uniques <- unique(temptrees[, c("plot", "species")])
  if(nrow(uniques) > 0){
    uniques$year <- i
  for(j in 1:nrow(uniques)){
    uniques[j, "counts"] <- nrow(temptrees[temptrees$plot==uniques[j, "plot"] & temptrees$species==uniques[j, "species"],])
    uniquefinal <- rbind(uniquefinal, uniques[j,])
    }
  }
  }

## get dead the same way
uniques <- unique(deadtrees[,c("plot", "species", "deathyear")])
for(i in 1:nrow(uniques)){
  temptrees <- deadtrees[deadtrees$plot==uniques[i, "plot"] &
                           deadtrees$species==uniques[i, "species"] &
                           deadtrees$deathyear==uniques[i, "deathyear"],]
  uniques[i, "counts"] <- nrow(temptrees)
}
colnames(uniquefinal) <- c("plot", "species", "alive", "year")
colnames(uniques) <- c("plot", "species", "year", "dead")
final <- merge(uniquefinal, uniques, all=T)
final <- final[!is.na(final$dead),]
final$alive <- as.numeric(final$alive)
final$mortrate <- final$dead / (final$alive + final$dead)

write.csv(final, "plot-year-mortality.csv")


alivebind <- aggregate(final$alive, by=list(final$plot, final$species), FUN=sum, na.rm=TRUE)

deadbind <- aggregate(final$dead, by=list(final$plot, final$species), FUN=sum, na.rm=TRUE)
plotspp <- merge(alivebind, deadbind)
colnames(plotspp) <- c("plot", "species", "alive", "dead")
plotspp$mortrate <- plotspp$dead / (plotspp$alive + plotspp$dead)
write.csv(plotspp, "plot-species-mort.csv")
