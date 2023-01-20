## models

library(data.table)
library(ggplot2)

## read in data

  acData <- fread('output/05_ac_2021_merged.csv')
  head(acData)  
  
## format for model
  
  dataAnySTOC <- acData[,c('STOC_ANY_N','nDaysSampled','dist_m','reproState')]
  head(dataAnySTOC)  
    table(dataAnySTOC$STOC_ANY_N, useNA = 'always') 
  
  #convert to binary (and convert NAs to 0s)
  dataAnySTOC$anySTOC <- ifelse(is.na(dataAnySTOC$STOC_ANY_N) | dataAnySTOC$STOC_ANY_N == 0, 0, 1)
    
  
## binomial GLM
  
  mod1 <- glm(anySTOC ~ nDaysSampled + dist_m + reproState, family = binomial, data = dataAnySTOC)

  summary(mod1)  
  