## data summaries

library(data.table)
library(ggplot2)
library(tidyverse)

## Load data
ac21_merged <- fread('output/05_ac_2021_merged.csv'); ac21_merged <- ac21_merged[,-1]
ac22_merged <- fread('output/05_ac_2022_merged.csv'); ac22_merged <- ac22_merged[,-1]

  #merge years
  colnames(ac21_merged)[c(17:18,22:23)] <- c('Earliest','Latest','utmx_new','utmy_new')
  
  ac21_merged$year <- '2021'; ac22_merged$year <- '2022'
  ac_merged <- rbind(ac21_merged, ac22_merged, fill = TRUE)
  ac_merged$SITE_YR <- paste(substr(ac_merged$SITE_STN, 1,2), ac_merged$year, sep = '_')

  #separate by sex  
  anyFNLC <- ac_merged[,c('SITE_STN','STOC_4N','duration','Distance','dist_intended','reproState')]
  femFNLC <- ac_merged[,c('SITE_STN','FEMALE_FNLC_OR_PAIR','duration','Distance','dist_intended','reproState')]
  maleFNLC <- ac_merged[,c('SITE_STN','MALE_FNLC_OR_PAIR','duration','Distance','dist_intended','reproState')]

  
## Summarize durations
  mean(ac_merged$duration) #mean 111 days
  min(ac_merged$duration); max(ac_merged$duration) #range 6 - 140 days
    mean(ac_merged[ac_merged$year %in% '2021',]$duration)  #mean 108 days in 2021 
    mean(ac_merged[ac_merged$year %in% '2022',]$duration)  #mean 115 days in 2022
  
  
## Summarize detections by site
  sum_table <- ac_merged %>% group_by(SITE_YR) %>% 
    summarise('earliest' = min(Earliest), 'latest' = max(Latest), 'n_ARUs' = length(unique(SITE_STN)),
              'repro_state' = unique(reproState),
              'STOC_ANY' = sum(STOC_ANY, na.rm = T), 'STOC_FNLC' = sum(STOC_4N, na.rm = T),
              'FEMALE_FNLC' = sum(FEMALE_FNLC, na.rm = T), 'MALE_FNLC' = sum(MALE_FNLC, na.rm = T),
              'PAIR' = sum(STOC_PAIR, na.rm = T), 'BARK' = sum(STOC_BARK, na.rm = T),
              'WHIS' = sum(STOC_WHIS, na.rm = T), 'BEG' = sum(STOC_BEG, na.rm = T),
              'IRREG' = sum(STOC_IRREG, na.rm = T))
  sum_table
  
  write.csv(sum_table, '/Users/caraappel/Documents/_RESEARCH/Activity centers/plots/tables/summary_table.csv')
  
  #convert to long and plot
  sum_table_long <- sum_table %>% 
    gather(key = "variable", value = "value", -c(SITE_YR, earliest, latest, n_ARUs, repro_state))
    
  ggplot(sum_table_long[sum_table_long$variable %in% c('SITE_YR','STOC_FNLC','FEMALE_FNLC','MALE_FNLC'),], 
         aes(x = SITE_YR, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = 'Four-note location calls by site',
         x = "Site", y = "Number of clips") +
    theme_minimal() + theme(legend.title =  element_blank())
  
  ggsave('/Users/caraappel/Documents/_RESEARCH/Activity centers/plots/summary_fnlc.png', 
         units="in", width=8, height=5, dpi=600, bg = 'transparent')
  
  ggplot(sum_table_long[sum_table_long$variable %in% c('SITE_YR','PAIR','BARK','WHIS','BEG'),],  #and IRREG?
         aes(x = SITE_YR, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = 'Other calls by site',
         x = "Site", y = "Number of clips") +
    theme_minimal() + theme(legend.title = element_blank())
  
  ggsave('/Users/caraappel/Documents/_RESEARCH/Activity centers/plots/summary_other_calls.png', 
         units="in", width=8, height=5, dpi=600, bg = 'transparent')
  

###################    
  
## Summarize detections by date and distance
  
  
    
###################    
    
## Summarize % detections by distance
  
  #use actual or intended distances?
  plot(ac_merged$dist_intended ~ ac_merged$Distance)
  
  ggplot(ac_merged, aes(x = Distance, y = STOC_4N, color = as.factor(dist_intended))) + 
    geom_point() +
    facet_grid(~year) + ggtitle('Any STOC FNLC') +
    theme_bw()
  
  ggplot(ac_merged[ac_merged$FEMALE_FNLC_OR_PAIR > 0,], aes(x = Distance, y = FEMALE_FNLC_OR_PAIR, color = as.factor(dist_intended))) +  #or color = as.factor(dist_intended)
    geom_point() +
    facet_grid(~year) + ggtitle('Female STOC') +
    theme_bw()
  
  ggplot(ac_merged[ac_merged$MALE_FNLC_OR_PAIR > 0,], aes(x = Distance, y = MALE_FNLC_OR_PAIR, color = as.factor(dist_intended))) + 
    geom_point() +
    facet_grid(~year) + ggtitle('Male STOC') +
    theme_bw()
  
  #should use actual, but proceed with intended for now to be consistent with what I've reported before
  

  #2021
  anystoc21 <- ac21_merged[!is.na(ac21_merged$STOC_ANY),]
    (tot_any_21 <- sum(anystoc21$STOC_ANY))                            #6554 detections of any STOC in 2021
    sum(anystoc21[anystoc21$dist_intended <= 1000,]$STOC_ANY) / tot_any_21  #92% within 1 km
    sum(anystoc21[anystoc21$dist_intended <= 2000,]$STOC_ANY) / tot_any_21  #99% within 2 km
    sum(anystoc21[anystoc21$dist_intended <= 3000,]$STOC_ANY) / tot_any_21  #100% within 3 km
    
  fem21 <- fem_21[!is.na(fem_21$FEMALE),]
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
    