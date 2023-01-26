## data prep for 2021 AC "big grid" data -- creating daily/weekly detection histories

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)

## Read in data ####

  #detections of residents only from 01_data_prep (with surveys removed and detections within 1 mile of surveys also removed)
  ac21stoc <- fread('output/03_ac_STOC_2021_noSurveys1mile.csv') 
  
  #Julie's dataframe with dates for all the stations (not just ones with STOC detected)
  # ac21stva <- fread('COA_AC_Work/from_Julie/data_output/COAAC_21_strixSeason_wide.csv')   #this one has STVA (but not weekly; can't use here)
  ac21jj <- fread('COA_AC_Work/from_Julie/data_output/summary_stnDates_COAAC2021.csv') #this one has start/end dates **WHY ARE THEY DIFFERENT?**
  
  #reformat columns to merge
  ac21stoc$SITESTN <- paste(toupper(substr(ac21stoc$SITE_STN, 1, 2)), 
                            substr(ac21stoc$SITE_STN, 5, 6), sep = '_')  
  ac21jj$SITESTN <- paste(substr(ac21jj$site, 5,6), 
                          sapply(strsplit(as.character(ac21jj$site), '\\-'),'[',2), sep = '_')
  
    unique(ac21stoc$SITESTN) #45 stations had resident STOC detections (no surv within 1 mile)
    unique(ac21jj$SITESTN)   #145 total stations were surveyed
  

## Create detection history template ####
  ac21jj
  
  #format dates
  ac21jj$start <- as.POSIXct(strptime(ac21jj$min, format = '%Y-%m-%d'), tz = 'America/Los_Angeles')
  ac21jj$stop <- as.POSIXct(strptime(ac21jj$max, format = '%Y-%m-%d'), tz = 'America/Los_Angeles')
  
  #get earliest/latest deployment/retrieval dates
  (beg <- ac21jj$start[order(ac21jj$start)][1])
  (end <- ac21jj$stop[order(ac21jj$stop)][length(ac21jj$stop)])
  
  ac21jj$duration <- ac21jj$stop - ac21jj$start #not sure why dates are different and this doesn't match nDaysSampled
  
  #generate list of all dates within date range above
  (nDays <- interval(beg, end)/days(1))
  range <- format(beg + days(0:nDays), format = '%Y-%m-%d') #every date between beg and end
  range_df <- as.POSIXct(strptime(range, '%Y-%m-%d'), tz = 'America/Los_Angeles')
    
  #make empty template
  dethist <- data.frame('SITESTN' = as.character(rep(ac21jj$SITESTN, each = length(range_df))),
                        'date' = rep(range_df, nrow(ac21jj)))
  nrow(dethist)
    #should be 145 sites * 138 days = 19728 but it's actually 145*39=20155
    #why are there 139 unique days here?
  
  merged <- merge(ac21jj[,c('SITESTN','start','stop','duration')], dethist, by = 'SITESTN', all = TRUE)
  
  #fill in whether each unit was surveyed on each date of the season (includes start/end dates as surveyed)
  merged$surv <- NA
  merged$surv <- ifelse(merged$date >= floor_date(merged$start, unit = 'day') & 
                          merged$date <= floor_date(merged$stop, unit = 'day'), 1,0)  
  
  #add week numbers
  (max_wk <- ceiling(max(as.numeric(ac21jj$duration/7), na.rm = TRUE)))
  
  dh_weeks <- NULL
  for(ss in unique(merged$SITESTN)){
    dh_site <- merged[merged$SITESTN %in% ss,]
    dh_site <- dh_site[order(dh_site$date),]
    dh_site$week <- c(rep('-', interval(min(dh_site$date), unique(dh_site$start))/days(1)),
                      rep((rep(1:max_wk, each = 7))[0:(nrow(dh_site)-ceiling(interval(min(dh_site$date),
                                                                                      unique(dh_site$start))/days(1)))]))
    dh_weeks <- rbind(dh_weeks, dh_site)
  }

  dh_weeks[is.na(dh_weeks$week),]$week <- '-'

  nrow(merged)
  nrow(dh_weeks)  #good, should match
  
      
## Merge detections with template ####
    
  #format dates (use 'NIGHT' field!)
  ac21stoc$NIGHT <- as.POSIXct(strptime(ac21stoc$NIGHT, '%m/%d/%Y'), tz = 'America/Los_Angeles')
  
  #make sure dates fall within beg/end above
  (earliest <- ac21stoc$NIGHT[order(ac21stoc$NIGHT)][1]) 
  (latest <- ac21stoc$NIGHT[order(ac21stoc$NIGHT)][length(ac21stoc$NIGHT)])

  #aggregate detections by station/date
  ac21_agg <- aggregate(ac21stoc[,c('STOC_BARK_N','STOC_WHIS_N','STOC_BEG_N','STOC_PAIR_N','STOC_IRREG_N',
                                    'STOC_4N_N','STOC_ANY_N','STVA_IRREG_N','STVA_PAIR_N','STVA_BEG_N','STVA_INSP_N','STVA_8N_N',
                                    'STVA_ANY_N','MALE_N','FEMALE_N','UNK_N','JUV_N')],
                        by = list('SITESTN' = as.factor(ac21stoc$SITESTN), 'NIGHT' = as.factor(ac21stoc$NIGHT)), 
                        FUN = sum)
  
    #save
    # write.csv(ac21_agg, 'output/06_ac_21_by_station_night.csv')

    
  #merge with detection history template
  head(dh_weeks)  
  head(ac21_agg) #will need to choose which species/call type to use
    ac21_agg$date <- as.POSIXct(strptime(ac21_agg$NIGHT, '%Y-%m-%d'), tz = 'America/Los_Angeles') #for merging
  
  dh_stoc <- merge(dh_weeks, ac21_agg, by = c('SITESTN','date'), all.x = TRUE)

    dh_stoc  
    nrow(dh_weeks)
    nrow(dh_stoc)  #should have same number as dh_weeks
    
  #don't need 'NIGHT' column
    dh_stoc$NIGHT <- NULL

  #should change NAs to 0s here? do below for weekly.
      
  #save!
    # write.csv(dh_stoc, 'output/07_ac_21_dethist_long.csv')

    
## Aggregate detection histories by week (NEED TO CHOOSE CALL TYPES AT THIS POINT) ####
  
  #change NAs to 0s when they occur within survey period; leave others as NAs
  dh_stoc$STOC_4N_N <- ifelse(is.na(dh_stoc$STOC_4N_N) & dh_stoc$surv == 1, 0, dh_stoc$STOC_4N_N)
  dh_stoc$STOC_ANY_N <- ifelse(is.na(dh_stoc$STOC_ANY_N) & dh_stoc$surv == 1, 0, dh_stoc$STOC_ANY_N)
  dh_stoc$FEMALE_N <- ifelse(is.na(dh_stoc$FEMALE_N) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE_N)
  dh_stoc$MALE_N <- ifelse(is.na(dh_stoc$MALE_N) & dh_stoc$surv == 1, 0, dh_stoc$MALE_N)
  dh_stoc$STVA_8N_N <- ifelse(is.na(dh_stoc$STVA_8N_N) & dh_stoc$surv == 1, 0, dh_stoc$STVA_8N_N)
  dh_stoc$STVA_ANY_N <- ifelse(is.na(dh_stoc$STVA_ANY_N) & dh_stoc$surv == 1, 0, dh_stoc$STVA_ANY_N)  
  
    table(dh_stoc[dh_stoc$surv == 1,]$STVA_ANY_N, useNA = 'always') #to compare before/after above
  
  #format 'week' so the columns will be ordered correctly
  dh_stoc$week <- factor(dh_stoc$week, levels = c('-',seq(1,20,1)))
  
  #convert long to wide
  
  ## STOC_4n
    dh_weekly_stoc_4n <- dcast(dh_stoc, SITESTN + start ~ week, value.var = 'STOC_4N_N',
                               fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_4n <- dh_weekly_stoc_4n[,-3]

  ## STOC_ANY
    dh_weekly_stoc_any <- dcast(dh_stoc, SITESTN + start ~ week, value.var = 'STOC_ANY_N',
                               fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_any <- dh_weekly_stoc_any[,-3]
    
  ## STOC_FEMALE
    dh_weekly_stoc_female <- dcast(dh_stoc, SITESTN + start ~ week, value.var = 'FEMALE_N', 
                                   fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female <- dh_weekly_stoc_female[,-'-']
    
  ## STOC_MALE
    dh_weekly_stoc_male <- dcast(dh_stoc, SITESTN + start ~ week, value.var = 'MALE_N',
                                 fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male <- dh_weekly_stoc_male[,-'-']

  ## Can't do STVA here because we don't have all detections (just ones tagged while reviewing STOC)
    
  ## Save!
    write.csv(dh_weekly_stoc_4n, 'output/weekly_dethist/08_dh_ac_2021_stoc4n.csv')
    write.csv(dh_weekly_stoc_any, 'output/weekly_dethist/08_dh_ac_2021_stocAny.csv')
    write.csv(dh_weekly_stoc_female, 'output/weekly_dethist/08_dh_ac_2021_stocFemale.csv')    
    write.csv(dh_weekly_stoc_male, 'output/weekly_dethist/08_dh_ac_2021_stocMale.csv')    

  ## Do some quality control  ...  
    
    
## Summarize number of nights with detections ####
  head(ac21_agg)
    
  #aggregate sites and nights
  nights_stoc_4n <- data.frame(table(ac21_agg[ac21_agg$STOC_4N_N > 0,]$SITESTN)); colnames(nights_stoc_4n) <- c('SITESTN','nights_STOC_4N')
  nights_stoc_any <- data.frame(table(ac21_agg[ac21_agg$STOC_ANY_N > 0,]$SITESTN)); colnames(nights_stoc_any) <- c('SITESTN','nights_STOC_ANY')
  nights_stoc_female <- data.frame(table(ac21_agg[ac21_agg$FEMALE_N > 0,]$SITESTN)); colnames(nights_stoc_female) <- c('SITESTN','nights_STOC_FEMALE')   
  nights_stoc_male <- data.frame(table(ac21_agg[ac21_agg$MALE_N > 0,]$SITESTN)); colnames(nights_stoc_male) <- c('SITESTN','nights_STOC_MALE')   
  
  #merge all together
  nights_list <- list(nights_stoc_4n, nights_stoc_any, nights_stoc_female, nights_stoc_male)
  nights_all <- nights_list %>% reduce(full_join, by = 'SITESTN')
    nights_all
  
  #add total number of nights surveyed for each station  
  head(ac21jj)  
  nights_all <- merge(nights_all, ac21jj[,c('SITESTN','duration')], by = 'SITESTN', all.x = TRUE)
    nights_all
    
  #save
  write.csv(nights_all, 'output/09_ac_21_nights_with_det.csv')  
    
    
## And summarize number of weeks with detections ####
    
  
  