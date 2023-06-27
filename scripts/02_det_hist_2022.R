## Creating daily/weekly detection histories for 2022 AC "big grid" data 

library(data.table)
library(ggplot2)
library(lubridate)
library(tidyverse)

## Read in data ----------------------------------------------------------------

  #detections of focal pair only from 01_data_prep script
  ac22stocRes <- fread('output/02_ac_STOC_2022_residents.csv') ; ac22stocRes <- ac22stocRes[,-c('V1')]
  
  #cleaned up duration and distance info
  ac22merged <- fread('output/05_ac_2022_merged.csv') ; ac22merged <- ac22merged[,-c('V1')]
  

## STEP 1: LONG-FORMAT DETECTION HISTORY #######################################  
  
## Create detection history template -------------------------------------------
  
  #format dates
  ac22merged$start <- as.POSIXct(strptime(ac22merged$Earliest, '%Y-%m-%d'), tz = 'America/Los_Angeles')
  ac22merged$stop <- as.POSIXct(strptime(ac22merged$Latest, '%Y-%m-%d'), tz = 'America/Los_Angeles')
  
  #get earliest/latest deployment/retrieval dates
  (beg <- ac22merged$start[order(ac22merged$start)][1])
  (end <- ac22merged$stop[order(ac22merged$stop)][length(ac22merged$stop)])
  
  #generate list of all dates within date range above
  (nDays <- interval(beg, end)/days(1))
  range <- format(beg + days(0:nDays), format = '%Y-%m-%d')
  range_df <- as.POSIXct(strptime(range, '%Y-%m-%d'), tz = 'America/Los_Angeles')
    
  #make empty template
  dethist <- data.frame('SITE_STN' = as.character(rep(ac22merged$SITE_STN, each = length(range_df))),
                        'date' = rep(range_df, nrow(ac22merged)))
  nrow(dethist)
    #should be 146 sites * 141 days = 20586 but it's actually 145*142 = 20732
    #why are there 142 unique days here -- bc it includes start day I think
  
  merged <- merge(ac22merged[,c('SITE_STN','start','stop','duration','Distance','dist_intended','reproState')], dethist, 
                  by = 'SITE_STN', all = TRUE)
  
  #fill in whether each unit was surveyed on each date of the season (includes start/end dates as surveyed)
  merged$surv <- NA
  merged$surv <- ifelse(merged$date >= floor_date(merged$start, unit = 'day') & 
                          merged$date <= floor_date(merged$stop, unit = 'day'), 1,0)  
  
  
## Add weeks (left-justified) --------------------------------------------------  
  (max_wk <- ceiling(difftime(range_df[length(range_df)], range_df[1], units = 'weeks')))
  
  dh_weeks <- NULL
  for(ss in unique(merged$SITE_STN)){
    dh_site <- merged[merged$SITE_STN %in% ss,]
    dh_site <- dh_site[order(dh_site$date),]
    dh_site$week_left <- c(rep('-', interval(min(dh_site$date), unique(dh_site$start))/days(1)),
                           rep((rep(1:max_wk, each = 7))[0:(nrow(dh_site)-ceiling(interval(min(dh_site$date),
                                                                                           unique(dh_site$start))/days(1)))]))
    dh_weeks <- rbind(dh_weeks, dh_site)
  }

  table(dh_weeks$week_left, useNA = 'always')    #change NA to '-' if needed
  # dh_weeks[is.na(dh_weeks$week),]$week_left <- '-'   

  nrow(merged)
  nrow(dh_weeks)  #good, should match
  

## Add weeks (staggered entry) -------------------------------------------------
  rangeDFwks <- data.frame('date' = range_df, 'week' = rep(1:max_wk, each = 7)[1:length(range_df)])
  
  dh_weeks$week_staggered <- factor(rangeDFwks$week[match(dh_weeks$date, rangeDFwks$date)])
  
    table(dh_weeks[dh_weeks$week_staggered == '2',]$date) #quick QC; try a few and match up with rangeDFweeks
    table(dh_weeks$week_staggered, useNA = 'always') #good, no NAs

    #save week date key
    # write.csv(rangeDFwks, 'output/08_weekly_dates_staggered_2022.csv')


## Add weeks (staggered entry to match 2021) -----------------------------------
  #read in week dates from 2021
  wksCombined <- fread('output/08_weekly_dates_staggered_2021.csv')
  wksCombined$date <- as.POSIXct(strptime(wksCombined$date, '%Y-%m-%d'),  tz = 'America/Los_Angeles')
    
  #what's the earliest day?
  (beg2 <- min(wksCombined$date) %m+% years(1))  #April 27 (convert to 2022 here using lubridate function)
  
  #create new date range
  (nDays2 <- interval(beg2, end)/days(1))
  range2 <- format(beg2 + days(0:nDays2), format = '%Y-%m-%d')
  range_df2 <- as.POSIXct(strptime(range2, '%Y-%m-%d'), tz = 'America/Los_Angeles')
  
  #create weeks    
  (max_wk2 <- ceiling(difftime(range_df2[length(range_df2)], range_df2[1], units = 'weeks')))
  rangeDFwks2 <- data.frame('date' = range_df2, 'week' = rep(1:(max_wk2+1), each = 7)[1:length(range_df2)])
    
  #add to dh template
  dh_weeks$week_stag_both_years <- factor(rangeDFwks2$week[match(dh_weeks$date, rangeDFwks2$date)])
  
    table(dh_weeks[dh_weeks$week_stag_both_years == '2',]$date) #quick QC; try a few and match up with rangeDFweeks
    table(dh_weeks$week_stag_both_years, useNA = 'always') #good, no NAs
  
  #save week date key
  rangeDFwks2$date_2021 <- rangeDFwks2$date %m-% years(1)
  rangeDFwks2$date_2022 <- rangeDFwks2$date; rangeDFwks2$date <- NULL
    # write.csv(rangeDFwks2, 'output/08_weekly_dates_staggered_21-22.csv')
    
            
## Aggregate detections by night -----------------------------------------------
    
  #format dates (use 'NIGHT' field!)
  ac22stocRes$NIGHT <- as.POSIXct(strptime(ac22stocRes$NIGHT, '%m/%d/%Y'), tz = 'America/Los_Angeles')
  
    #is it ok to use 'NIGHT' field instead of using timestamp above? check...
    nrow(ac22stocRes[(date(ac22stocRes$timestamp) != (date(ac22stocRes$NIGHT))) & ac22stocRes$AM_PM %in% 'PM',])   #good, 0
    nrow(ac22stocRes[(date(ac22stocRes$timestamp) != (date(ac22stocRes$NIGHT)+1)) & ac22stocRes$AM_PM %in% 'AM',]) #good, 0
      #if detections are PM, the 'NIGHT' should be the same as the timestamp date
      #if detections are AM, the 'NIGHT' should be one day later than the timestamp date
      #so yes, it's OK to use the 'NIGHT' field
  
  #make sure earliest and latest 'NIGHTS' with detections fall within survey beg/end above
  (earliest <- ac22stocRes$NIGHT[order(ac22stocRes$NIGHT)][1]) 
  (latest <- ac22stocRes$NIGHT[order(ac22stocRes$NIGHT)][length(ac22stocRes$NIGHT)])

  #aggregate detections by station/date
  ac22byNight <- aggregate(ac22stocRes[,c('STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR','STOC_PAIR_MFF',
                                          'STOC_IRREG','STOC_4N','STOC_ANY','MALE','FEMALE','FEMALE2',
                                          'UNK','JUV','FEMALE_FNLC','MALE_FNLC','FEMALE_FNLC_OR_PAIR',
                                          'MALE_FNLC_OR_PAIR')],
                        by = list('SITE_STN' = as.factor(ac22stocRes$SITE_STN), 'NIGHT' = as.factor(ac22stocRes$NIGHT)), 
                        FUN = sum)
  
    
## Merge detections with det hist template -------------------------------------
  head(dh_weeks)  
  head(ac22byNight) 
  
  #format date for merging
  ac22byNight$date <- as.POSIXct(strptime(ac22byNight$NIGHT, '%Y-%m-%d'), tz = 'America/Los_Angeles')

  #merge!    
  dh_stoc <- merge(dh_weeks, ac22byNight, by = c('SITE_STN','date'), all.x = TRUE)

    head(dh_stoc)
    nrow(dh_weeks)
    nrow(dh_stoc)  #should have same number as dh_weeks
    
  #don't need 'NIGHT' column
    dh_stoc$NIGHT <- NULL

  ## Change NAs to 0s within survey period; leave others as NAs ----------------
    dh_stoc$STOC_BARK <- ifelse(is.na(dh_stoc$STOC_BARK) & dh_stoc$surv == 1, 0, dh_stoc$STOC_BARK)
    dh_stoc$STOC_WHIS <- ifelse(is.na(dh_stoc$STOC_WHIS) & dh_stoc$surv == 1, 0, dh_stoc$STOC_WHIS)
    dh_stoc$STOC_BEG <- ifelse(is.na(dh_stoc$STOC_BEG) & dh_stoc$surv == 1, 0, dh_stoc$STOC_BEG)
    dh_stoc$STOC_PAIR <- ifelse(is.na(dh_stoc$STOC_PAIR) & dh_stoc$surv == 1, 0, dh_stoc$STOC_PAIR)
    dh_stoc$STOC_PAIR_MFF <- ifelse(is.na(dh_stoc$STOC_PAIR_MFF) & dh_stoc$surv == 1, 0, dh_stoc$STOC_PAIR_MFF)
    dh_stoc$STOC_IRREG <- ifelse(is.na(dh_stoc$STOC_IRREG) & dh_stoc$surv == 1, 0, dh_stoc$STOC_IRREG)
    dh_stoc$UNK <- ifelse(is.na(dh_stoc$UNK) & dh_stoc$surv == 1, 0, dh_stoc$UNK)
    dh_stoc$JUV <- ifelse(is.na(dh_stoc$JUV) & dh_stoc$surv == 1, 0, dh_stoc$JUV)
    dh_stoc$STOC_4N <- ifelse(is.na(dh_stoc$STOC_4N) & dh_stoc$surv == 1, 0, dh_stoc$STOC_4N)
    dh_stoc$STOC_ANY <- ifelse(is.na(dh_stoc$STOC_ANY) & dh_stoc$surv == 1, 0, dh_stoc$STOC_ANY)
    dh_stoc$FEMALE <- ifelse(is.na(dh_stoc$FEMALE) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE)
    dh_stoc$FEMALE2 <- ifelse(is.na(dh_stoc$FEMALE2) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE2)
    dh_stoc$MALE <- ifelse(is.na(dh_stoc$MALE) & dh_stoc$surv == 1, 0, dh_stoc$MALE)
    dh_stoc$FEMALE_FNLC <- ifelse(is.na(dh_stoc$FEMALE_FNLC) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE_FNLC)
    dh_stoc$FEMALE_FNLC_OR_PAIR <- ifelse(is.na(dh_stoc$FEMALE_FNLC_OR_PAIR) & dh_stoc$surv == 1, 0, dh_stoc$FEMALE_FNLC_OR_PAIR)
    dh_stoc$MALE_FNLC <- ifelse(is.na(dh_stoc$MALE_FNLC) & dh_stoc$surv == 1, 0, dh_stoc$MALE_FNLC)
    dh_stoc$MALE_FNLC_OR_PAIR <- ifelse(is.na(dh_stoc$MALE_FNLC_OR_PAIR) & dh_stoc$surv == 1, 0, dh_stoc$MALE_FNLC_OR_PAIR)
    
      #to compare before/after above (only thing that should change is some of the NAs will become 0s):
      table(dh_stoc[dh_stoc$surv == 1,]$FEMALE_FNLC, useNA = 'always') 
    
  #format 'week' so the columns will be ordered correctly
    dh_stoc$week_left <- factor(dh_stoc$week_left, levels = c('-',seq(1,as.numeric(max_wk),1)))
    class(dh_stoc$week_staggered); levels(dh_stoc$week_staggered) #already a factor    
    class(dh_stoc$week_stag_both_years); levels(dh_stoc$week_stag_both_years) #already a factor    
    
    
## SAVE ------------------------------------------------------------------------
    write.csv(ac22byNight, 'output/06_ac_22_by_station_night.csv')  #most recent output 06/22/23 with new 'week_stag_both_years'
    write.csv(dh_stoc, 'output/07_ac_22_dethist_long.csv')

    
################################################################################ 
    
## STEP 2: WEEKLY (WIDE) DETECTION HISTORY #####################################      

  # dh_stoc <- fread('output/07_ac_22_dethist_long.csv'); dh_stoc <- dh_stoc[,-c('V1')]  #if necessary

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
    dh_weekly_stoc_female_fnlc <- dcast(dh_stoc, SITE_STN + start ~ (week_left), value.var = 'FEMALE_FNLC',
                                        fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc <- dh_weekly_stoc_female_fnlc[,-3]
    dh_weekly_stoc_female_fnlc$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc[,c(3:22)]))
    
  ## MALE_FNLC
    dh_weekly_stoc_male_fnlc <- dcast(dh_stoc, SITE_STN + start ~ (week_left), value.var = 'MALE_FNLC',
                                      fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc <- dh_weekly_stoc_male_fnlc[,-3]
    dh_weekly_stoc_male_fnlc$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc[,c(3:22)]))
    
  ## FEMALE_FNLC_OR_PAIR
    dh_weekly_stoc_female_fnlc_or_pair <- dcast(dh_stoc, SITE_STN + start ~ (week_left), value.var = 'FEMALE_FNLC_OR_PAIR',
                                                fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc_or_pair <- dh_weekly_stoc_female_fnlc_or_pair[,-3]
    dh_weekly_stoc_female_fnlc_or_pair$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc_or_pair[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc_or_pair$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc_or_pair[,c(3:22)]))
    
  ## MALE_FNLC_OR_PAIR
    dh_weekly_stoc_male_fnlc_or_pair <- dcast(dh_stoc, SITE_STN + start ~ (week_left), value.var = 'MALE_FNLC_OR_PAIR',
                                              fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc_or_pair <- dh_weekly_stoc_male_fnlc_or_pair[,-3]
    dh_weekly_stoc_male_fnlc_or_pair$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc_or_pair[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc_or_pair$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc_or_pair[,c(3:22)]))    
    
    
    
## Convert long to wide (STAGGERED-ENTRY) --------------------------------------
    
  ## STOC_4n
    dh_weekly_stoc_4n_st <- dcast(dh_stoc, SITE_STN + start ~ as.integer(week_staggered), value.var = 'STOC_4N',
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


## Convert long to wide (STAGGERED-ENTRY TO MATCH 2021 DATES) ------------------
    
  ## STOC_4n
    dh_weekly_stoc_4n_stb <- dcast(dh_stoc, SITE_STN + start ~ as.integer(week_stag_both_years), value.var = 'STOC_4N',
                                  fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_4n_stb$detWeeks <- rowSums(dh_weekly_stoc_4n_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_4n_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_4n_stb[,c(3:22)]))
    
  ## STOC_ANY
    dh_weekly_stoc_any_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'STOC_ANY',
                                   fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_any_stb$detWeeks <- rowSums(dh_weekly_stoc_any_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_any_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_any_stb[,c(3:22)]))
    
  ## STOC_FEMALE
    dh_weekly_stoc_female_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'FEMALE', 
                                      fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_stb$detWeeks <- rowSums(dh_weekly_stoc_female_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_stb[,c(3:22)]))
    
  ## STOC_MALE
    dh_weekly_stoc_male_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'MALE',
                                    fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_stb$detWeeks <- rowSums(dh_weekly_stoc_male_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_stb[,c(3:22)]))    
    
  ## STOC_PAIR
    dh_weekly_stoc_pair_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'STOC_PAIR',
                                    fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_pair_stb$detWeeks <- rowSums(dh_weekly_stoc_pair_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_pair_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_pair_stb[,c(3:22)]))
    
  ## STOC_IRREG
    dh_weekly_stoc_irreg_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'STOC_IRREG',
                                     fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_irreg_stb$detWeeks <- rowSums(dh_weekly_stoc_pair_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_irreg_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_irreg_stb[,c(3:22)]))
    
  ## FEMALE_FNLC  
    dh_weekly_stoc_female_fnlc_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'FEMALE_FNLC', 
                                           fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc_stb$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc_stb[,c(3:22)]))
    
  ## MALE_FNLC  
    dh_weekly_stoc_male_fnlc_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'MALE_FNLC', 
                                         fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc_stb$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc_stb[,c(3:22)]))  
    
  ## FEMALE_FNLC_OR_PAIR
    dh_weekly_stoc_female_fnlc_or_pair_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'FEMALE_FNLC_OR_PAIR', 
                                                   fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_female_fnlc_or_pair_stb$detWeeks <- rowSums(dh_weekly_stoc_female_fnlc_or_pair_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_female_fnlc_or_pair_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_female_fnlc_or_pair_stb[,c(3:22)]))
    
  ## MALE_FNLC_OR_PAIR
    dh_weekly_stoc_male_fnlc_or_pair_stb <- dcast(dh_stoc, SITE_STN + start ~ week_stag_both_years, value.var = 'MALE_FNLC_OR_PAIR', 
                                                 fun.aggregate = function(x) if(all(is.na(x)) == TRUE) NA_real_ else sum(x, na.rm = TRUE))
    dh_weekly_stoc_male_fnlc_or_pair_stb$detWeeks <- rowSums(dh_weekly_stoc_male_fnlc_or_pair_stb[,c(3:22)] != 0, na.rm = TRUE)
    dh_weekly_stoc_male_fnlc_or_pair_stb$survWeeks <- rowSums(!is.na(dh_weekly_stoc_male_fnlc_or_pair_stb[,c(3:22)]))  
    
    
            
## Summarize number of nights with detections ----------------------------------
  head(ac22byNight)
    
  #aggregate sites and nights
  nights_stoc_4n <- data.frame(table(ac22byNight[ac22byNight$STOC_4N > 0,]$SITE_STN)); colnames(nights_stoc_4n) <- c('SITE_STN','nights_STOC_4N')
  nights_stoc_any <- data.frame(table(ac22byNight[ac22byNight$STOC_ANY > 0,]$SITE_STN)); colnames(nights_stoc_any) <- c('SITE_STN','nights_STOC_ANY')
  nights_stoc_female <- data.frame(table(ac22byNight[ac22byNight$FEMALE > 0,]$SITE_STN)); colnames(nights_stoc_female) <- c('SITE_STN','nights_STOC_FEMALE')   
  nights_stoc_male <- data.frame(table(ac22byNight[ac22byNight$MALE > 0,]$SITE_STN)); colnames(nights_stoc_male) <- c('SITE_STN','nights_STOC_MALE')   
  nights_stoc_pair <- data.frame(table(ac22byNight[ac22byNight$STOC_PAIR > 0,]$SITE_STN)); colnames(nights_stoc_pair) <- c('SITE_STN','nights_STOC_PAIR')   
  nights_stoc_irreg <- data.frame(table(ac22byNight[ac22byNight$STOC_IRREG > 0,]$SITE_STN)); colnames(nights_stoc_irreg) <- c('SITE_STN','nights_STOC_IRREG')
  nights_stoc_female_fnlc <- data.frame(table(ac22byNight[ac22byNight$FEMALE_FNLC > 0,]$SITE_STN)); colnames(nights_stoc_female_fnlc) <- c('SITE_STN','nights_STOC_FEMALE_FNLC')   
  nights_stoc_female_fnlc_or_pair <- data.frame(table(ac22byNight[ac22byNight$FEMALE_FNLC_OR_PAIR > 0,]$SITE_STN)); colnames(nights_stoc_female_fnlc_or_pair) <- c('SITE_STN','nights_STOC_FEMALE_FNLC_OR_PAIR')   
  nights_stoc_male_fnlc <- data.frame(table(ac22byNight[ac22byNight$MALE_FNLC > 0,]$SITE_STN)); colnames(nights_stoc_male_fnlc) <- c('SITE_STN','nights_STOC_MALE_FNLC')   
  nights_stoc_male_fnlc_or_pair <- data.frame(table(ac22byNight[ac22byNight$MALE_FNLC_OR_PAIR > 0,]$SITE_STN)); colnames(nights_stoc_male_fnlc_or_pair) <- c('SITE_STN','nights_STOC_MALE_FNLC_OR_PAIR')   
  
  #merge all together
  nights_list <- list(nights_stoc_4n, nights_stoc_any, nights_stoc_pair, nights_stoc_irreg,
                      nights_stoc_female, nights_stoc_female_fnlc_or_pair, nights_stoc_female_fnlc,
                      nights_stoc_male, nights_stoc_male_fnlc_or_pair, nights_stoc_male_fnlc)
  nights_all <- nights_list %>% reduce(full_join, by = 'SITE_STN')
    head(nights_all)
  
  #add back in all stations, and the total number of nights surveyed for each station  
  head(ac22merged)  
  nights_all <- merge(nights_all, ac22merged[,c('SITE_STN','duration')], by = 'SITE_STN', all = TRUE)
    head(nights_all)
    nights_all[is.na(nights_all)] <- 0 #change NAs to 0s
    
 
## Save! -----------------------------------------------------------------------
    write.csv(dh_weekly_stoc_4n, 'output/08_weekly_dethist_left/08_dh_ac_2022_stoc4n_left.csv')
    write.csv(dh_weekly_stoc_any, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocAny_left.csv')
    write.csv(dh_weekly_stoc_female, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocFemale_left.csv')
    write.csv(dh_weekly_stoc_male, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocMale_left.csv')
    write.csv(dh_weekly_stoc_pair, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocPair_left.csv')
    write.csv(dh_weekly_stoc_female_fnlc, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocFemaleFNLC_left.csv')
    write.csv(dh_weekly_stoc_female_fnlc_or_pair, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocFemaleFNLCorPair_left.csv')
    write.csv(dh_weekly_stoc_male_fnlc, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocMaleFNLC_left.csv')
    write.csv(dh_weekly_stoc_male_fnlc_or_pair, 'output/08_weekly_dethist_left/08_dh_ac_2022_stocMaleFNLCorPair_left.csv')
    
    
    write.csv(dh_weekly_stoc_4n_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stoc4n_staggered.csv')
    write.csv(dh_weekly_stoc_any_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocAny_staggered.csv')
    write.csv(dh_weekly_stoc_female_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocFemale_staggered.csv')
    write.csv(dh_weekly_stoc_male_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocMale_staggered.csv')
    write.csv(dh_weekly_stoc_pair_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocPair_staggered.csv')
    write.csv(dh_weekly_stoc_female_fnlc_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocFemaleFNLC_staggered.csv')
    write.csv(dh_weekly_stoc_female_fnlc_or_pair_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocFemaleFNLCorPair_staggered.csv')
    write.csv(dh_weekly_stoc_male_fnlc_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocMaleFNLC_staggered.csv')
    write.csv(dh_weekly_stoc_male_fnlc_or_pair_st, 'output/08_weekly_dethist_staggered/08_dh_ac_2022_stocMaleFNLCorPair_staggered.csv')
    
    
    write.csv(dh_weekly_stoc_4n_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stoc4n_stag_both.csv')
    write.csv(dh_weekly_stoc_any_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocAny_stag_both.csv')
    write.csv(dh_weekly_stoc_female_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocFemale_stag_both.csv')
    write.csv(dh_weekly_stoc_male_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocMale_stag_both.csv')
    write.csv(dh_weekly_stoc_pair_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocPair_stag_both.csv')
    write.csv(dh_weekly_stoc_female_fnlc_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocFemaleFNLC_stag_both.csv')
    write.csv(dh_weekly_stoc_female_fnlc_or_pair_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocFemaleFNLCorPair_stag_both.csv')
    write.csv(dh_weekly_stoc_male_fnlc_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocMaleFNLC_stag_both.csv')
    write.csv(dh_weekly_stoc_male_fnlc_or_pair_stb, 'output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocMaleFNLCorPair_stag_both.csv')
    
    
    write.csv(nights_all, 'output/09_ac_22_nights_with_det.csv')
        
    ## most recent outputs 06/22/23 with new 'week_both_years'
    ## most recent outputs 05/19/23 with FNLC separate
    
    