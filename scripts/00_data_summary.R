## data summaries for AC "big grid" data for ORTWS abstract and analysis

library(data.table)
library(ggplot2)

## Re-create data from Word Doc '2021AC_owl detection tallies+survey removals.docx'
## (Chris will send me the raw data I can merge with tag backups, but do this for now)

#detection table using 1 mile survey removal threshold (other options are 0, 1km, 2km)

det2021 <- data.frame('distance_km' = c(0,1,1.7,2,2.6,3),
                      'pair' = c(729,55,9,0,0,0),
                      'male' = c(3235,874,333,106,39,24),
                      'female' = c(1203,153,18,8,0,0),
                      'unknown' = c(1176,111,30,7,1,0),
                      'total' = c(4911,1086,372,121,40,24))
det2021

det2021long <- melt(det2021, id.vars = c('distance_km'))
  
ggplot(det2021long, aes(x = distance_km, y = value, fill = variable)) + 
  geom_bar(stat = 'identity') +
  ggtitle('all')

ggplot(det2021long[det2021long$variable %in% 'female',], aes(x = distance_km, y = value)) +
  geom_bar(stat = 'identity') + 
  # labs(title = 'Females') + 
  ggtitle('Females')


## Calculate proportion within certain distance thresholds
det2021t <- data.frame(t(det2021[,-1]))
colnames(det2021t) <- det2021[,1]
det2021t

det2021t$total <- det2021t[,1] + det2021t[,2] + det2021t[,3] + det2021t[,4] + det2021t[,5] + det2021t[,6]
det2021t

##definitely a better way to do this.....
det2021t$prop0 <- det2021t$`0` / det2021t$total
det2021t$prop1 <- det2021t$`1` / det2021t$total
det2021t$prop17 <- det2021t$`1.7` / det2021t$total
det2021t$prop2 <- det2021t$`2` / det2021t$total
det2021t$prop26 <- det2021t$`2.6` / det2021t$total
det2021t$prop3 <- det2021t$`3` / det2021t$total

  det2021t
  
  ##e.g., for females: 0.8704776 + 0.11070912 = 0.9811867 of detections were within 0-1 km
  ##e.g., for total: 0.7493134+0.16570034+0.018462008+0.003661886 = 0.9371376 of detections were within 2 km
  