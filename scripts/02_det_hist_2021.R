## Creating daily/weekly detection histories for 2021 AC "big grid" data 

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)

## Read in data ----------------------------------------------------------------

  #detections of focal pair only from 01_data_prep script
  ac21stoc <- fread('output/03_ac_STOC_2021_noSurveys1mile.csv') 
  
  #Julie's dataframe with start/end dates for all the stations (not just ones with STOC detected)
  ac21jj <- fread('2021/COA_AC_Work/from_Julie/data_output/COAAC_2021_SampleSummary_fromNoise.csv')
  
  #format columns to merge
  ac21jj$SITE_STN <- paste(substr(ac21jj$site, 5,6), 
                           sapply(strsplit(as.character(ac21jj$site), '\\-'),'[',2), sep = '_')
  
    unique(ac21stoc$SITE_STN) #45 stations had resident STOC detections (no surv within 1 mile)
    unique(ac21jj$SITE_STN)   #145 total stations were surveyed
  

## STEP 1: LONG-FORMAT DETECTION HISTORY #######################################  
    
## Create detection history template -------------------------------------------
    
  #format dates
  ac21jj$start <- as.POSIXct(strptime(ac21jj$min_date, format = '%Y-%m-%d'), tz = 'America/Los_Angeles')
  ac21jj$stop <- as.POSIXct(strptime(ac21jj$max_date, format = '%Y-%m-%d'), tz = 'America/Los_Angeles')
  
  #get earliest/latest deployment/retrieval dates
  (beg <- ac21jj$start[order(ac21jj$start)][1])
  (end <- ac21jj$stop[order(ac21jj$stop)][length(ac21jj$stop)])
  
  ac21jj$duration <- difftime(ac21jj$stop, ac21jj$start) #nDays is unique days while duration is the difference
  
  #generate list of all dates within date range above
  (nDays <- interval(beg, end)/days(1))
  range <- format(beg + days(0:nDays), format = '%Y-%m-%d') #every date between beg and end
  range_df <- as.POSIXct(strptime(range, '%Y-%m-%d'), tz = 'America/Los_Angeles')
    
  #make empty template
  dethist <- data.frame('SITE_STN' = as.character(rep(ac21jj$SITE_STN, each = length(range_df))),
                        'date' = rep(range_df, nrow(ac21jj)))
  nrow(dethist)
    #should be 145 sites * 138 days = 19728 but it's actually 145*39=20155
    #why are there 139 unique days here -- bc it includes start day I think
  
  merged <- merge(ac21jj[,c('SITE_STN','start','stop','duration')], dethist, by = 'SITE_STN', all = TRUE)
  
  #fill in whether each unit was surveyed on each date of the season (includes start/end dates as surveyed)
  merged$surv <- NA
  merged$surv <- ifelse(merged$date >= floor_date(merged$start, unit = 'day') & 
                          merged$date <= floor_date(merged$stop, unit = 'day'), 1,0)  
  

## Add weeks (left-justified) --------------------------------------------------  
  (max_wk <- ceiling(max(as.numeric(ac21jj$duration/7), na.rm = TRUE)))
  
  dh_weeks <- NULL
  for(ss in unique(merged$SITE_STN)){
    dh_site <- merged[merged$SITE_STN %in% ss,]
    dh_site <- dh_site[order(dh_site$date),]
    dh_site$week_left <- c(rep('-', interval(min(dh_site$date), unique(dh_site$start))/days(1)),
                         rep((rep(1:max_wk, each = 7))[0:(nrow(dh_site)-ceiling(interval(min(dh_site$date),
                                                                                      unique(dh_site$start))/days(1)))]))
    dh_weeks <- rbind(dh_weeks, dh_site)
  }

  dh_weeks[is.na(dh_weeks$week_left),]$week_left <- '-'

  nrow(merged)
  nrow(dh_weeks)  #good, should match
  

## Add weeks (staggered entry) -------------------------------------------------
  rangeDFwks <- data.frame('date' = range_df, 'week' = rep(1:max_wk, each = 7)[1:length(range_df)])
  
  dh_weeks$week_staggered <- factor(rangeDFwks$week[match(dh_weeks$date, rangeDFwks$date)])
  
  table(dh_weeks[dh_weeks$week_staggered == '2',]$date) #quick QC; try a few and match up with rangeDFweeks
  table(dh_weeks$week_staggered, useNA = 'always') #good, no NAs
  
  #save week date key
  # write.csv(rangeDFwks, 'output/08_weekly_dates_staggered_2021.csv')
  
  
## Aggregate detections by night -----------------------------------------------

  #format dates (use 'NIGHT' field!)
  ac21stoc$NIGHT <- as.POSIXct(strptime(ac21stoc$NIGHT, '%m/%d/%Y'), tz = 'America/Los_Angeles')
  
  #is it ok to use 'NIGHT' field instead of using timestamp above? check...
  nrow(ac21stoc[(date(ac21stoc$timestamp) != (date(ac21stoc$NIGHT))) & ac21stoc$AM_PM %in% 'PM',])   #good, 0
  nrow(ac21stoc[(date(ac21stoc$timestamp) != (date(ac21stoc$NIGHT)+1)) & ac21stoc$AM_PM %in% 'AM',]) #good, 0
    #if detections are PM, the 'NIGHT' should be the same as the timestamp date
    #if detections are AM, the 'NIGHT' should be one day later than the timestamp date
    #so yes, it's OK to use the 'NIGHT' field
  
  #make sure earliest and latest 'NIGHTS' with detections fall within survey beg/end above
  (earliest <- ac21stoc$NIGHT[order(ac21stoc$NIGHT)][1]) 
  (latest <- ac21stoc$NIGHT[order(ac21stoc$NIGHT)][length(ac21stoc$NIGHT)])

  #aggregate detections by station/date
  ac21_agg <- aggregate(ac21stoc[,c('STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR','STOC_IRREG',
                                    'STOC_4N','STOC_ANY','MALE','FEMALE','UNK','JUV',
                                    'FEMALE_FNLC','MALE_FNLC','FEMALE_FNLC_OR_PAIR','MALE_FNLC_OR_PAIR')],
                        by = list('SITE_STN' = as.factor(ac21stoc$SITE_STN), 'NIGHT' = as.factor(ac21stoc$NIGHT)), 
                        FUN = sum)
  

## Merge detections with det hist template -------------------------------------
  head(dh_weeks)  
  head(ac21_agg) 
  
  #format date for merging
  ac21_agg$date <- as.POSIXct(strptime(ac21_agg$NIGHT, '%Y-%m-%d'), tz = 'America/Los_Angeles') #for merging
  
  #merge!
  dh_stoc <- merge(dh_weeks, ac21_agg, by = c('SITE_STN','date'), all.x = TRUE)

    dh_stoc  
    nrow(dh_weeks)
    nrow(dh_stoc)  #should have same number as dh_weeks
    
  #don't need 'NIGHT' column
    dh_stoc$NIGHT <- NULL

  ## Change NAs to 0s within survey period; leave others as NAs ----------------
    dh_stoc$STOC_BARK <- ifelse(is.na(dh_stoc$STOC_BARK) & dh_stoc$surv == 1, 0, dh_stoc$STOC_BARK)
    dh_stoc$STOC_WHIS <- ifelse(is.na(dh_stoc$STOC_WHIS) & dh_stoc$surv == 1, 0, dh_stoc$STOC_WHIS)
    dh_stoc$STOC_BEG <- ifelse(is.na(dh_stoc$STOC_BEG) & dh_stoc$surv == 1, 0, dh_stoc$STOC_BEG)
    dh_stoc$STOC_PAIR <- ifelse(is.na(dh_stoc$STOC_PAIR) & dh_stoc$surv == 1, 0, dh_stoc$STOC_PAIR)
    dh_stoc$STOC_IRREG <- ifelse(is.na(dh_stoc$STOC_IRREG) & dh_stoc$surv == 1, 0, dh_stoc$STOC_IRREG)
    dh_stoc$UNK <- ifelse(is.na(dh_stoc$UNK) & dh_stoc$surv == 1, 0, dh_stoc$UNK)
    dh_stoc$JUV <- ifelse(is.na(dh_stoc$JUV) & dh_stoc$surv == 1, 0, dh_stoc$JUV)
    dh_stoc$STOC_4N <- ifelse(is.na(dh_stoc$STOC_4N) & dh_stoc$surv == 1, 0, dh_stoc$STOC_4N)
    dh_stoc$STOC_ANY <- ifelse(is.na(dh_stoc$STOC_ANY) & dh_stoc$surv == 1, 0, dh_stoc$STOC_ANY)
    dh_stoc$FEMALE <- ifelse(is.na(dh_stoc$FEMALE) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE)
    dh_stoc$MALE <- ifelse(is.na(dh_stoc$MALE) & dh_stoc$surv == 1, 0, dh_stoc$MALE)
    dh_stoc$FEMALE_FNLC <- ifelse(is.na(dh_stoc$FEMALE_FNLC) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE_FNLC)
    dh_stoc$MALE_FNLC <- ifelse(is.na(dh_stoc$MALE_FNLC) & dh_stoc$surv == 1, 0, dh_stoc$MALE_FNLC)
    dh_stoc$FEMALE_FNLC_OR_PAIR <- ifelse(is.na(dh_stoc$FEMALE_FNLC_OR_PAIR) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE_FNLC_OR_PAIR)
    dh_stoc$MALE_FNLC_OR_PAIR <- ifelse(is.na(dh_stoc$MALE_FNLC_OR_PAIR) & dh_stoc$surv == 1, 0, dh_stoc$MALE_FNLC_OR_PAIR)
    
    #to compare before/after above (only thing that should change is some of the NAs will become 0s):
    table(dh_stoc[dh_stoc$surv == 1,]$STOC_4N, useNA = 'always') 
    table(dh_stoc[dh_stoc$surv == 1,]$MALE_FNLC_OR_PAIR, useNA = 'always') 
    
    #format 'week' so the columns will be ordered correctly
    dh_stoc$week_left <- factor(dh_stoc$week_left, levels = c('-',seq(1,as.numeric(max_wk),1)))
      levels(dh_stoc$week_left)
    class(dh_stoc$week_staggered); levels(dh_stoc$week_staggered) #already a factor    
    

## SAVE ------------------------------------------------------------------------
  write.csv(ac21_agg, 'output/06_ac_21_by_station_night.csv') #output 05/19/23 with FNLC separated
  write.csv(dh_stoc, 'output/07_ac_21_dethist_long.csv')

  
################################################################################ 
  
## STEP 2: WEEKLY (WIDE) DETECTION HISTORY #####################################      
  
  # dh_stoc <- fread('output/07_ac_21_dethist_long.csv'); dh_stoc <- dh_stoc[,-c('V1')]  #if necessary
  
    #check classes (detection columns should be numeric)
    sapply(dh_stoc, class)  
    # dh_stoc$STOC_4N <- as.numeric(dh_stoc$STOC_4N); dh_stoc$STOC_ANY <- as.numeric(dh_stoc$STOC_ANY)
    # dh_stoc$FEMALE <- as.numeric(dh_stoc$FEMALE) ; dh_stoc$MALE <- as.numeric(dh_stoc$MALE)
    # dh_stoc$STOC_PAIR <- as.numeric(dh_stoc$STOC_PAIR) ; dh_stoc$STOC_IRREG <- as.numeric(dh_stoc$STOC_IRREG)
     
  
## Convert long to wide (LEFT-JUSTIFIED) ---------------------------------------
  
  ## STOC_4n
    dh_weekly_stoc_4n <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'STOC_4N',
                               fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_4n <- dh_weekly_stoc_4n[,-3]
    dh_weekly_stoc_4n$detWeeks <- rowSums(dh_weekly_stoc_4n[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_4n$survWeeks <- rowSums(!is.na(dh_weekly_stoc_4n[,c(3:22)]))

  ## STOC_ANY
    dh_weekly_stoc_any <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'STOC_ANY',
                               fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_any <- dh_weekly_stoc_any[,-3]
    dh_weekly_stoc_any$detWeeks <- rowSums(dh_weekly_stoc_any[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_any$survWeeks <- rowSums(!is.na(dh_weekly_stoc_any[,c(3:22)]))
    
  ## STOC_FEMALE
    dh_weekly_stoc_female <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'FEMALE', 
                                   fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female <- dh_weekly_stoc_female[,-3]
    dh_weekly_stoc_female$detWeeks <- rowSums(dh_weekly_stoc_female[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female[,c(3:22)]))
    
  ## STOC_MALE
    dh_weekly_stoc_male <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'MALE',
                                 fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male <- dh_weekly_stoc_male[,-3]
    dh_weekly_stoc_male$detWeeks <- rowSums(dh_weekly_stoc_male[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male[,c(3:22)]))
    
  ## STOC_PAIR
    dh_weekly_stoc_pair <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'STOC_PAIR',
                                 fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_pair <- dh_weekly_stoc_pair[,-3]
    dh_weekly_stoc_pair$detWeeks <- rowSums(dh_weekly_stoc_pair[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_pair$survWeeks <- rowSums(!is.na(dh_weekly_stoc_pair[,c(3:22)]))
    
  ## STOC_IRREG
    dh_weekly_stoc_irreg <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'STOC_IRREG',
                                  fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_irreg <- dh_weekly_stoc_irreg[,-3]
    dh_weekly_stoc_irreg$detWeeks <- rowSums(dh_weekly_stoc_irreg[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_irreg$survWeeks <- rowSums(!is.na(dh_weekly_stoc_irreg[,c(3:22)]))
    
  ## FEMALE_FNLC
    dh_weekly_stoc_female_fnlc <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'FEMALE_FNLC',
                                  fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc <- dh_weekly_stoc_female_fnlc[,-3]
    dh_weekly_stoc_female_fnlc$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc[,c(3:22)]))
    
  ## MALE_FNLC
    dh_weekly_stoc_male_fnlc <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'MALE_FNLC',
                                        fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc <- dh_weekly_stoc_male_fnlc[,-3]
    dh_weekly_stoc_male_fnlc$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc[,c(3:22)]))
    
  ## FEMALE_FNLC_OR_PAIR
    dh_weekly_stoc_female_fnlc_or_pair <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'FEMALE_FNLC_OR_PAIR',
                                        fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc_or_pair <- dh_weekly_stoc_female_fnlc_or_pair[,-3]
    dh_weekly_stoc_female_fnlc_or_pair$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc_or_pair[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc_or_pair$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc_or_pair[,c(3:22)]))
    
  ## MALE_FNLC_OR_PAIR
    dh_weekly_stoc_male_fnlc_or_pair <- dcast(dh_stoc, SITE_STN + start ~ week_left, value.var = 'MALE_FNLC_OR_PAIR',
                                      fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc_or_pair <- dh_weekly_stoc_male_fnlc_or_pair[,-3]
    dh_weekly_stoc_male_fnlc_or_pair$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc_or_pair[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc_or_pair$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc_or_pair[,c(3:22)]))    
    

## Convert long to wide (STAGGERED-ENTRY) --------------------------------------
    
  ## STOC_4n
    dh_weekly_stoc_4n_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'STOC_4N',
                                  fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_4n_st$detWeeks <- rowSums(dh_weekly_stoc_4n_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_4n_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_4n_st[,c(3:22)]))
    
  ## STOC_ANY
    dh_weekly_stoc_any_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'STOC_ANY',
                                   fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_any_st$detWeeks <- rowSums(dh_weekly_stoc_any_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_any_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_any_st[,c(3:22)]))
    
  ## STOC_FEMALE
    dh_weekly_stoc_female_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'FEMALE', 
                                      fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_st$detWeeks <- rowSums(dh_weekly_stoc_female_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_st[,c(3:22)]))
    
  ## STOC_MALE
    dh_weekly_stoc_male_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'MALE',
                                    fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_st$detWeeks <- rowSums(dh_weekly_stoc_male_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_st[,c(3:22)]))    
    
  ## STOC_PAIR
    dh_weekly_stoc_pair_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'STOC_PAIR',
                                    fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_pair_st$detWeeks <- rowSums(dh_weekly_stoc_pair_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_pair_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_pair_st[,c(3:22)]))
    
  ## STOC_IRREG
    dh_weekly_stoc_irreg_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'STOC_IRREG',
                                     fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_irreg_st$detWeeks <- rowSums(dh_weekly_stoc_pair_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_irreg_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_irreg_st[,c(3:22)]))
    
  ## FEMALE_FNLC
    dh_weekly_stoc_female_fnlc_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'FEMALE_FNLC',
                                        fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc_st$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc_st[,c(3:22)]))
    
  ## MALE_FNLC
    dh_weekly_stoc_male_fnlc_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'MALE_FNLC',
                                      fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc_st$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc_st[,c(3:22)]))
    
  ## FEMALE_FNLC_OR_PAIR
    dh_weekly_stoc_female_fnlc_or_pair_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'FEMALE_FNLC_OR_PAIR',
                                                fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc_or_pair_st$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc_or_pair_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc_or_pair_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc_or_pair_st[,c(3:22)]))
    
  ## MALE_FNLC_OR_PAIR
    dh_weekly_stoc_male_fnlc_or_pair_st <- dcast(dh_stoc, SITE_STN + start ~ week_staggered, value.var = 'MALE_FNLC_OR_PAIR',
                                              fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc_or_pair_st$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc_or_pair_st[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc_or_pair_st$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc_or_pair_st[,c(3:22)]))    
    
    
## Summarize number of nights with detections ----------------------------------
  head(ac21_agg)
    
  #aggregate sites and nights
  nights_stoc_4n <- data.frame(table(ac21_agg[ac21_agg$STOC_4N > 0,]$SITE_STN)); colnames(nights_stoc_4n) <- c('SITE_STN','nights_STOC_4N')
  nights_stoc_any <- data.frame(table(ac21_agg[ac21_agg$STOC_ANY > 0,]$SITE_STN)); colnames(nights_stoc_any) <- c('SITE_STN','nights_STOC_ANY')
  nights_stoc_female <- data.frame(table(ac21_agg[ac21_agg$FEMALE > 0,]$SITE_STN)); colnames(nights_stoc_female) <- c('SITE_STN','nights_STOC_FEMALE')   
  nights_stoc_male <- data.frame(table(ac21_agg[ac21_agg$MALE > 0,]$SITE_STN)); colnames(nights_stoc_male) <- c('SITE_STN','nights_STOC_MALE')   
  nights_stoc_pair <- data.frame(table(ac21_agg[ac21_agg$STOC_PAIR > 0,]$SITE_STN)); colnames(nights_stoc_pair) <- c('SITE_STN','nights_STOC_PAIR')   
  nights_stoc_irreg <- data.frame(table(ac21_agg[ac21_agg$STOC_IRREG > 0,]$SITE_STN)); colnames(nights_stoc_irreg) <- c('SITE_STN','nights_STOC_IRREG')   
  nights_stoc_female_fnlc <- data.frame(table(ac21_agg[ac21_agg$FEMALE_FNLC > 0,]$SITE_STN)); colnames(nights_stoc_female_fnlc) <- c('SITE_STN','nights_STOC_FEMALE_FNLC')   
  nights_stoc_male_fnlc <- data.frame(table(ac21_agg[ac21_agg$MALE_FNLC > 0,]$SITE_STN)); colnames(nights_stoc_male_fnlc) <- c('SITE_STN','nights_STOC_MALE_FNLC')   
  nights_stoc_female_fnlc_or_pair <- data.frame(table(ac21_agg[ac21_agg$FEMALE_FNLC_OR_PAIR > 0,]$SITE_STN)); colnames(nights_stoc_female_fnlc_or_pair) <- c('SITE_STN','nights_STOC_FEMALE_FNLC_OR_PAIR')   
  nights_stoc_male_fnlc_or_pair <- data.frame(table(ac21_agg[ac21_agg$MALE_FNLC_OR_PAIR > 0,]$SITE_STN)); colnames(nights_stoc_male_fnlc_or_pair) <- c('SITE_STN','nights_STOC_MALE_FNLC_OR_PAIR')   
  
  #merge all together
  nights_list <- list(nights_stoc_4n, nights_stoc_any, nights_stoc_pair, nights_stoc_irreg, 
                      nights_stoc_female, nights_stoc_female_fnlc_or_pair, nights_stoc_female_fnlc,
                      nights_stoc_male, nights_stoc_male_fnlc_or_pair, nights_stoc_male_fnlc)
  nights_all <- nights_list %>% reduce(full_join, by = 'SITE_STN')
    head(nights_all)
  
  #add back in all stations, and the total number of nights surveyed for each station  
  head(ac21jj)  
  nights_all <- merge(nights_all, ac21jj[,c('SITE_STN','duration')], by = 'SITE_STN', all = TRUE)
    head(nights_all)
    nights_all[is.na(nights_all)] <- 0 #change NAs to 0s

            
## Save! -----------------------------------------------------------------------
    write.csv(dh_weekly_stoc_4n, 'output/08_weekly_dethist_left/08_dh_ac_2021_stoc4n_left.csv')
    write.csv(dh_weekly_stoc_any, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocAny_left.csv')
    write.csv(dh_weekly_stoc_female, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocFemale_left.csv')
    write.csv(dh_weekly_stoc_male, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocMale_left.csv')
    write.csv(dh_weekly_stoc_pair, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocPair_left.csv')
    write.csv(dh_weekly_stoc_female_fnlc, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocFemaleFNLC_left.csv')
    write.csv(dh_weekly_stoc_female_fnlc_or_pair, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocFemaleFNLCorPair_left.csv')
    write.csv(dh_weekly_stoc_male_fnlc, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocMaleFNLC_left.csv')
    write.csv(dh_weekly_stoc_male_fnlc_or_pair, 'output/08_weekly_dethist_left/08_dh_ac_2021_stocMaleFNLCorPair_left.csv')
    
    write.csv(dh_weekly_stoc_4n_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stoc4n_staggered.csv')
    write.csv(dh_weekly_stoc_any_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocAny_staggered.csv')
    write.csv(dh_weekly_stoc_female_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocFemale_staggered.csv')
    write.csv(dh_weekly_stoc_male_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocMale_staggered.csv')
    write.csv(dh_weekly_stoc_pair_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocPair_staggered.csv')
    write.csv(dh_weekly_stoc_female_fnlc_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocFemaleFNLC_staggered.csv')
    write.csv(dh_weekly_stoc_female_fnlc_or_pair_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocFemaleFNLCorPair_staggered.csv')
    write.csv(dh_weekly_stoc_male_fnlc_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocMaleFNLC_staggered.csv')
    write.csv(dh_weekly_stoc_male_fnlc_or_pair_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2021_stocMaleFNLCorPair_staggered.csv')
    
    write.csv(nights_all, 'output/09_ac_21_nights_with_det.csv')
    
    #mot recent output 051923 (with FNLC separately)
    #most recent output 051023 (redo do add 'staggered' dh for 2021)    

    

    