## SCR?

library(data.table)
library(nimble)
library(nimbleSCR)
library(basicMCMCplots)

## Create habitat grid
grid <- fread('scr/activity_ARU_coordinates.csv')
  head(grid)
  plot(grid$y ~ grid$x)
  

## Format detections
acData <- fread('output/05_ac_2021_merged.csv')
  
  ## create new dataframes
  sexSTOC <- acData[,c('SITESTN','FEMALE_N','MALE_N','nDaysSampled','dist_m','reproState')]
  sexSTOC$stn <- sapply(strsplit(as.character(sexSTOC$SITESTN), '\\_'), '[', 2)
  
  ## NAs to 0s
  sexSTOC$FEMALE_N <- ifelse(is.na(sexSTOC$FEMALE_N), 0, sexSTOC$FEMALE_N)
  sexSTOC$MALE_N <- ifelse(is.na(sexSTOC$MALE_N), 0, sexSTOC$MALE_N)
  
  ## 
  
  
  # ## convert to binary (and convert NAs to 0s)
  # sexSTOC$female <- ifelse(is.na(sexSTOC$FEMALE_N) | sexSTOC$FEMALE_N == 0, 0, 1)  
  # sexSTOC$male   <- ifelse(is.na(sexSTOC$MALE_N) | sexSTOC$MALE_N == 0, 0, 1)  
  # 
  # sexSTOC[,-c(1:2)][order(sexSTOC$dist_m),]
  
  ## re-format dataframe as Bernoulli (each row is a combo of groups)
  femaleSTOC <- dcast(sexSTOC[,c(4:6)], dist_m + reproState ~ female, 
                      fun.aggregate = length, value.var = 'female')
  colnames(femaleSTOC)[c(3:4)] <- c('No','Yes')
  femaleSTOC$reproState <- factor(femaleSTOC$reproState, levels = c('pair','nest','fledged')) 
  
  maleSTOC <- dcast(sexSTOC[,c(4:5,7)], dist_m + reproState ~ male, 
                    fun.aggregate = length, value.var = 'male')
  colnames(maleSTOC)[c(3:4)] <- c('No','Yes')
  maleSTOC$reproState <- factor(maleSTOC$reproState, levels = c('pair','nest','fledged')) #order levels
############


## Create habitat grid
coordsHabitatGridCenter <- cbind(rep(seq(75, 5, by = -10), 10),
                                 sort(rep(seq(5, 100, by = 10), 8)))
colnames(coordsHabitatGridCenter) <- c("x","y")
coordsHabitatGridCenter


## Create trap grid
coordsObsCenter <- cbind(rep(seq(15, 65, by = 10), 8),
                         sort(rep(seq(15, 85, by = 10), 6)))
colnames(coordsObsCenter) <- c("x","y")
coordsObsCenter

## Plot check
plot(coordsHabitatGridCenter[,"y"] ~ coordsHabitatGridCenter[,"x"],
     xlim = c(0,80), ylim = c(0,100),
     pch = 1, cex = 1.5) 
points(coordsObsCenter[,"y"] ~ coordsObsCenter[,"x"], col="red", pch=16 ) 
par(xpd=TRUE)
legend(x = 7, y = 13,
       legend=c("Habitat window centers", "Observation window centers"),
       pt.cex = c(1.5,1),
       horiz = T,
       pch=c(1,16),
       col=c("black", "red"),
       bty = 'n')
