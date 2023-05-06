## data summaries

library(data.table)
library(ggplot2)

## Load data
ac21_merged <- fread('output/05_ac_2021_merged.csv'); ac21_merged <- ac21_merged[,-1]
ac22_merged <- fread('output/05_ac_2022_merged.csv'); ac22_merged <- ac22_merged[,-1]

  #merge years
  colnames(ac21_merged) <- c('SITE_STN','Earliest','Latest','duration',
                             'STOC_ANY','STOC_4N','STOC_IRREG','STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR',
                             'FEMALE','MALE','UNK','JUV',
                             'reproState','Distance','utmx_new','utmy_new','dist_intended')
  ac21_merged$year <- '2021'; ac22_merged$year <- '2022'
  ac_merged <- rbind(ac21_merged, ac22_merged, fill = TRUE)

  #separate by sex  
  anySTOC <- ac_merged[,c('STOC_ANY','duration','Distance','dist_intended','reproState')]
  fem <- ac_merged[,c('FEMALE','duration','Distance','dist_intended','reproState')]
  male <- ac_merged[,c('MALE','duration','Distance','dist_intended','reproState')]

  
## Summarize durations
  mean(ac_merged$duration) #mean 111
  min(ac_merged$duration); max(ac_merged$duration) #range 6 - 140
  mean(ac_merged[ac_merged$year %in% '2021',]$duration)  #108 in 2021 (where did I get 120 for abstract?)
  mean(ac_merged[ac_merged$year %in% '2022',]$duration)  #115 in 2022
  
  
## Summarize % detections by distance
  
  #use actual or intended distances?
  plot(ac_merged$dist_intended ~ ac_merged$Distance)
  
  ggplot(ac_merged, aes(x = Distance, y = STOC_ANY, color = as.factor(dist_intended))) + 
    geom_point() +
    facet_grid(~year) + ggtitle('Any STOC') +
    theme_bw()
  
  ggplot(ac_merged[ac_merged$FEMALE > 0,], aes(x = Distance, y = FEMALE, color = as.factor(dist_intended))) +  #or color = as.factor(dist_intended)
    geom_point() +
    facet_grid(~year) + ggtitle('Female STOC') +
    theme_bw()
  
  ggplot(ac_merged, aes(x = Distance, y = MALE, color = as.factor(dist_intended))) + 
    geom_point() +
    facet_grid(~year) + ggtitle('Male STOC') +
    theme_bw()
  
  #should use actual, but proceed with intended for now to be consistent with what I've reported before
  
  #2021
  sum(ac_merged[!is.na(ac_merged$STOC_ANY) & ac_merged$year == '2021',]$STOC_ANY) #6554 detections of any stoc in 2021
  sum(ac_merged[!is.na(ac_merged$STOC_ANY) & ac_merged$year == '2021' & 
                  ac_merged$dist_intended <= 1000,]$STOC_ANY) #5997 within 
  
  sum(ac_merged[!is.na(ac_merged$STOC_ANY) & ac_merged$year == '2021',]$STOC_ANY)
  
  
  #2021
  anystoc21 <- anySTOC_21[!is.na(anySTOC_21$STOC_ANY_N),]
    (tot_any_21 <- sum(anystoc21$STOC_ANY_N))                            #6554 detections of any STOC in 2021
    sum(anystoc21[anystoc21$dist_intended <= 1000,]$STOC_ANY_N) / tot_any_21  #92% within 1 km
    sum(anystoc21[anystoc21$dist_intended <= 2000,]$STOC_ANY_N) / tot_any_21  #99% within 2 km
    sum(anystoc21[anystoc21$dist_intended <= 3000,]$STOC_ANY_N) / tot_any_21  #100% within 3 km
    
  fem21 <- fem_21[!is.na(fem_21$FEMALE_N),]
    (tot_f_21 <- sum(fem_21$FEMALE_N))                       #1382 detections of female STOC in 2021
    sum(fem21[fem21$dist_intended <= 1000,]$FEMALE_N) / tot_f_21  #98% within 1 km  
    sum(fem21[fem21$dist_intended <= 2000,]$FEMALE_N) / tot_f_21  #100% within 2 km
    
  male21 <- male_21[!is.na(male_21$MALE_N),]
    (tot_m_21 <- sum(male21$MALE_N))                         #4611 detections of male STOC in 2021
    sum(male21[male21$dist_intended <= 1000,]$MALE_N) / tot_m_21  #89% within 2 km
    sum(male21[male21$dist_intended <= 2000,]$MALE_N) / tot_m_21  #99% within 2 km
    sum(male21[male21$dist_intended <= 3000,]$MALE_N) / tot_m_21  #100% within 3 km
    
    
  #2022  
  anystoc22 <- anySTOC_22[!is.na(anySTOC_22$STOC_ANY),]
    (tot_any_22 <- sum(anystoc22$STOC_ANY))                            #7578 detections of any STOC in 2021
    sum(anystoc22[anystoc22$dist_intended <= 1000,]$STOC_ANY) / tot_any_22  #51% within 1 km
    sum(anystoc22[anystoc22$dist_intended <= 2000,]$STOC_ANY) / tot_any_22  #84% within 2 km
    sum(anystoc22[anystoc22$dist_intended <= 3000,]$STOC_ANY) / tot_any_22  #100% within 3 km
    
  fem22 <- fem_22[!is.na(fem_22$FEMALE),]
    (tot_f_22 <- sum(fem22$FEMALE))                       #4630 detections of female STOC in 2021
    sum(fem22[fem22$dist_intended <= 1000,]$FEMALE) / tot_f_22  #49% within 1 km
    sum(fem22[fem22$dist_intended <= 2000,]$FEMALE) / tot_f_22  #85% within 2 km
    sum(fem22[fem22$dist_intended <= 3000,]$FEMALE) / tot_f_22  #100% within 3 km    
    
    male21 <- male_21[!is.na(male_21$MALE_N),]
    (tot_m_21 <- sum(male21$MALE_N))                         #4611 detections of male STOC in 2021
    sum(male21[male21$dist_intended <= 1000,]$MALE_N) / tot_m_21  #89% within 1 km
    sum(male21[male21$dist_intended <= 2000,]$MALE_N) / tot_m_21  #99% within 2 km
    sum(male21[male21$dist_intended <= 2000,]$MALE_N) / tot_m_21  #100% within 2 km
    