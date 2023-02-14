## exploring detections by date and call type

library(data.table)
library(ggplot2)

## load data

ac21_agg <- fread('output/06_ac_21_by_station_night.csv')


## ## Visualizations ####

#detections by calendar day
head(ac21_agg)
ac21_agg$reproState <- ifelse(grepl('DC', ac21_agg$SITESTN), 'fledged1',
                              ifelse(grepl('MC', ac21_agg$SITESTN), 'nest',
                                     ifelse(grepl('UG', ac21_agg$SITESTN), 'fledged2', 'pair')))   

ac21_agg_area <- aggregate(ac21_agg[,c('STOC_BARK_N','STOC_WHIS_N','STOC_BEG_N','STOC_PAIR_N','STOC_IRREG_N',
                                       'STOC_4N_N','STOC_ANY_N','STVA_IRREG_N','STVA_PAIR_N','STVA_BEG_N','STVA_INSP_N','STVA_8N_N',
                                       'STVA_ANY_N','MALE_N','FEMALE_N','UNK_N','JUV_N')], 
                           by = list('reproState' = as.factor(ac21_agg$reproState), 'date' = as.factor(ac21_agg$date)),
                           FUN = sum)

ggplot(ac21_agg, aes(x = date)) +
  geom_point(aes(y = STOC_ANY_N, color = 'Any'), size = 2, alpha = 0.8) +
  geom_point(aes(y = MALE_N, color = 'Male'), size = 2, alpha = 0.8) +
  geom_point(aes(y = FEMALE_N, color = 'Female'), size = 2, alpha = 0.8) +
  facet_wrap(vars(reproState), nrow = 2) +
  labs(y = 'number of detections', color = 'Sex') +
  scale_color_manual(values = c('Female' = 'darkorange','Male' = 'blue','Any' = 'gray')) +
  theme(panel.background = element_rect(fill = 'transparent'),
        axis.line = element_line(),
        axis.title = element_text(size = 18),
        # axis.title.x = element_blank(),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        # legend.position = 'none',
        # legend.background = element_rect(fill='transparent'))
  )


#another way
head(dh_weekly_stoc_any)
