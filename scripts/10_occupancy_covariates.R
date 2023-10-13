# formatting and saving covariates for occupancy models using activity center (big grid) data

library(data.table)
library(tidyverse)


## SITE-LEVEL COVARIATES -------------------------------------------------------

## Distance, site, reproductive state, nesting attempt -------------------------

  #load detections with site-level data
  info21 <- fread('output/05_ac_2021_merged.csv')
  info22 <- fread('output/05_ac_2022_merged.csv')
  
  #pull out columns we need and format
  site21 <- info21[,c('SITE_STN','duration','reproState','Distance','dist_intended','XNAD83FP','YNAD83FP')]
    colnames(site21) <- c('site_stn','duration','repro_state','dist_actual','dist_intended','utm_x','utm_y')
  site22 <- info22[,c('SITE_STN','duration','reproState','Distance','dist_intended','utmx_new','utmy_new')]
    colnames(site22) <- c('site_stn','duration','repro_state','dist_actual','dist_intended','utm_x','utm_y')
  
  site21$site_name <- sapply(strsplit(as.character(site21$site_stn), '\\_'), '[', 1)
  site22$site_name <- sapply(strsplit(as.character(site22$site_stn), '\\_'), '[', 1)
  
  site21$nested <- ifelse(site21$repro_state %in% c('female','pair'), 'n', 'y')  
  site22$nested <- ifelse(site22$repro_state %in% c('female','pair'), 'n', 'y')  
  
  #reorder and clean up
  site21 <- site21[order(site21$site_stn),]
  site22 <- site22[order(site22$site_stn),]
  
  #standardize if needed
  site21$duration_std      <- scale(site21$duration)
  site21$dist_intended_std <- scale(site21$dist_intended)
  site21$dist_actual_std   <- scale(site21$dist_actual)
  
    hist(site21$dist_actual)
    hist(site21$dist_actual_std)
  
  site22$duration_std      <- scale(site22$duration)
  site22$dist_intended_std <- scale(site22$dist_intended)
  site22$dist_actual_std   <- scale(site22$dist_actual)
    
    hist(site22$dist_actual)
    hist(site22$dist_actual_std)
        
  #can keep all columns?
    head(site21[,c('site_stn','repro_state','site_name','duration_std','dist_intended_std','dist_actual_std')])
    head(site22[,c('site_stn','repro_state','site_name','duration_std','dist_intended_std','dist_actual_std')])
    
  #combine both years
    site21$year <- '2021'; site22$year <- '2022'
    sites_21_22 <- rbind(site21, site22)
    
  #save
    # site_level_covar <- list(site21,site22)
    # saveRDS(site_level_covar, 'output/10_occ_covariates/10_site_covariates.rds')
    write.csv(site21, 'output/10_occ_covariates/10_site_level_2021.csv')
    write.csv(site22, 'output/10_occ_covariates/10_site_level_2022.csv')
    write.csv(sites_21_22, 'output/10_occ_covariates/10_site_level_2021-2022.csv')

    
## NR habitat ------------------------------------------------------------------
  #read in db from my GIS files  
  gis_21 <- fread('/Users/caraappel/Documents/_RESEARCH/GIS/ActivityCenters/COA_AC_2021_STN/COA_AC_2021_STN.csv')
  # gis_22 <- fread('/Users/caraappel/Documents/_RESEARCH/GIS/ActivityCenters/AC_2022_locations/ac_2022_locations_1.csv') #don't actually need this
  
  #read in NR tables from Julie
  nr_21_mean <- fread('/Users/caraappel/Documents/_RESEARCH/Activity centers/nr_hab_covar/mn_cts_500mBUFF_2021.csv')
  nr_22_mean <- fread('/Users/caraappel/Documents/_RESEARCH/Activity centers/nr_hab_covar/nsoCTS_500m_2022c.csv')
  nr_21_sum <- fread('/Users/caraappel/Documents/_RESEARCH/Activity centers/nr_hab_covar/nsoHAB_500m_2021.csv')
  nr_22_sum <- fread('/Users/caraappel/Documents/_RESEARCH/Activity centers/nr_hab_covar/nsoHAB_500m_2022c.csv')
  
  #merge mean and sum tables
  nr_21 <- merge(nr_21_mean, nr_21_sum, by = c('OID_','STN_ID','ZONE_CODE'))
  nr_22 <- merge(nr_22_mean, nr_22_sum, by = c('OID_','stn','ZONE_CODE'))
  
  #match with site names (2021)
  nr_21$SITE_STN <- gis_21$SITESTN[match(nr_21$STN_ID, gis_21$STN_ID)]
  
  #format site names (2022)
  nr_22$stn_no <- substr(nr_22$stn, nchar(nr_22$stn)-1, nchar(nr_22$stn))
  nr_22$site <- ifelse(grepl('Baker', nr_22$stn), 'BC',
                       ifelse(grepl('Drift', nr_22$stn), 'DC',
                              ifelse(grepl('Cummins', nr_22$stn), 'CC',
                                     ifelse(grepl('Limpy', nr_22$stn), 'LM', NA))))
  table(nr_22$site, useNA = 'a')
  nr_22$SITE_STN <- paste(nr_22$site, nr_22$stn_no, sep = '_')
  
  #save
  write.csv(nr_21, '/Users/caraappel/Documents/_RESEARCH/Activity centers/nr_hab_covar/nr_cleaned_2021.csv')
  write.csv(nr_22, '/Users/caraappel/Documents/_RESEARCH/Activity centers/nr_hab_covar/nr_cleaned_2022.csv')
  
  #append to site-level tables and save
  site21$nr_mean <- nr_21$MEAN[match(site21$site_stn, nr_21$SITE_STN)]
  site21$nr_sum  <- nr_21$SUM[match(site21$site_stn, nr_21$SITE_STN)]
  
  site22$nr_mean <- nr_22$MEAN[match(site22$site_stn, nr_22$SITE_STN)]
  site22$nr_sum  <- nr_22$SUM[match(site22$site_stn, nr_22$SITE_STN)]
  
  saveRDS(list(site21, site22), 'output/10_occ_covariates/10_site_covariates.rds')
  sites_21_22 <- rbind(site21, site22)
 
  write.csv(site21, 'output/10_occ_covariates/10_site_level_2021.csv')
  write.csv(site22, 'output/10_occ_covariates/10_site_level_2022.csv')
  write.csv(sites_21_22, 'output/10_occ_covariates/10_site_level_2021-2022.csv')
    #most recent 10/09/23 
    #need to add barred owl data too (scroll down)
  
  
## SURVEY-LEVEL ----------------------------------------------------------------

## Noise (mean daily SPL) -- staggered for years separately --------------------
  noise21 <- readRDS('2021/COA_AC_Work/from_Julie/Codes/noise_2021_COA_AC.rds')
  noise22 <- readRDS('2022/ac_22_dailyNoise.rds')
    head(noise21)
    head(noise22)
    
  #load in week dates and match them up (staggered)
  dates21 <- fread('output/08_weekly_dates_staggered_2021.csv')    
  dates22 <- fread('output/08_weekly_dates_staggered_2022.csv')

  noise21$week_st <- dates21$week[match(noise21$DATE2, dates21$date)]
  noise22$week_st <- dates22$week[match(noise22$DATE2, dates22$date)]
  
  #clean up site names
  noise21$site_area <- ifelse(grepl('MC', noise21$site), 'MC',
                              ifelse(grepl('UG', noise21$site), 'UG',
                                     ifelse(grepl('WC', noise21$site), 'WC', 'DC')))
  noise21$site_name <- paste(noise21$site_area,
                             sapply(strsplit(as.character(noise21$site), '\\-'), '[', 2), sep = '_')
  
  noise22$site_area <- ifelse(grepl('Baker', noise22$site), 'BC',
                              ifelse(grepl('DC', noise22$site), 'DC',
                                     ifelse(grepl('CC', noise22$site), 'CC', 'LM')))
  noise22$site_name <- paste(noise22$site_area,
                             sapply(strsplit(as.character(noise22$site), '\\-'), '[', 2), sep = '_')
  
  #standardize daily 'mnSPL'
  noise21$mnSPL_std <- scale(noise21$mnSPL)
  noise22$mnSPL_std <- scale(noise22$mnSPL)
  
  #summarize 'mnSPL' by week
  noise21_wk <- group_by(noise21, week_st, site_name) %>% summarise(meanSPL = mean(mnSPL))
  noise22_wk <- group_by(noise22, week_st, site_name) %>% summarise(meanSPL = mean(mnSPL))

  #standardize weekly 'mnSPL *or should we summarize the standardized daily meanSPL...?*
  noise21_wk$meanSPL_std <- scale(noise21_wk$meanSPL)
  noise22_wk$meanSPL_std <- scale(noise22_wk$meanSPL)
  
  #convert to wide (daily)
  noise21_dy_wide     <- pivot_wider(noise21, id_cols = site_name, names_from = DATE2, values_from = mnSPL)
  noise21_dy_wide_std <- pivot_wider(noise21, id_cols = site_name, names_from = DATE2, values_from = mnSPL_std)

  noise22_dy_wide     <- pivot_wider(noise22, id_cols = site_name, names_from = DATE2, values_from = mnSPL)
  noise22_dy_wide_std <- pivot_wider(noise22, id_cols = site_name, names_from = DATE2, values_from = mnSPL_std)
    
  #convert to wide (weekly)
  noise21_wk_wide     <- pivot_wider(noise21_wk, id_cols = site_name, names_from = week_st, values_from = meanSPL) 
  noise21_wk_wide_std <- pivot_wider(noise21_wk, id_cols = site_name, names_from = week_st, values_from = meanSPL_std) 
  
  noise22_wk_wide     <- pivot_wider(noise22_wk, id_cols = site_name, names_from = week_st, values_from = meanSPL)
  noise22_wk_wide_std <- pivot_wider(noise22_wk, id_cols = site_name, names_from = week_st, values_from = meanSPL_std)
  
  #sort by site name
  noise21_dy_wide     <- noise21_dy_wide[order(noise21_dy_wide$site_name),]
  noise21_dy_wide_std <- noise21_dy_wide_std[order(noise21_dy_wide_std$site_name),] 
  
  noise22_dy_wide     <- noise22_dy_wide[order(noise22_dy_wide$site_name),]
  noise22_dy_wide_std <- noise22_dy_wide_std[order(noise22_dy_wide_std$site_name),] 
  
  noise21_wk_wide     <- noise21_wk_wide[order(noise21_wk_wide$site_name),] 
  noise21_wk_wide_std <- noise21_wk_wide_std[order(noise21_wk_wide_std$site_name),] 
  
  noise22_wk_wide     <- noise22_wk_wide[order(noise22_wk_wide$site_name),] 
  noise22_wk_wide_std <- noise22_wk_wide_std[order(noise22_wk_wide_std$site_name),] 
  
  #save
  # write.csv(noise21_dy_wide,     'output/10_occ_covariates/10_noise_daily_raw_2021.csv')
  # write.csv(noise21_dy_wide_std, 'output/10_occ_covariates/10_noise_daily_std_2021.csv')
  # 
  # write.csv(noise22_dy_wide,     'output/10_occ_covariates/10_noise_daily_raw_2022.csv')
  # write.csv(noise22_dy_wide_std, 'output/10_occ_covariates/10_noise_daily_std_2022.csv')
  # 
  # write.csv(noise21_wk_wide,     'output/10_occ_covariates/10_noise_weekly_raw_2021.csv')
  # write.csv(noise21_wk_wide_std, 'output/10_occ_covariates/10_noise_weekly_std_2021.csv')
  # 
  # write.csv(noise22_wk_wide,     'output/10_occ_covariates/10_noise_weekly_raw_2022.csv')
  # write.csv(noise22_wk_wide_std, 'output/10_occ_covariates/10_noise_weekly_std_2022.csv')
  
  
## Noise (mean daily SPL) -- staggered for both years combined ------------
  head(noise21)
  head(noise22)
  
  #load in week dates and match them up (left-justified)
  # dates21_left <- fread('output/07_ac_21_dethist_long.csv'); colnames(dates21_left)[c(2:3,8)] <- c('site_name', 'DATE2','week_left')
  # dates22_left <- fread('output/07_ac_22_dethist_long.csv'); colnames(dates22_left)[2:3] <- c('site_name', 'DATE2')

  #load in week dates (staggered with matching calendar days for both years)
  dates <- fread('output/08_weekly_dates_staggered_21-22.csv')
  
  #match them up  
  # noise21 <- merge(noise21, dates21_left[,c('site_name','DATE2','week_left')], by = c('site_name','DATE2'), all.x = TRUE)
  # noise22 <- merge(noise22, dates22_left[,c('site_name','DATE2','week_left')], by = c('site_name','DATE2'), all.x = TRUE)
  noise21$week_stag <- dates$week[match(noise21$DATE2, dates$date_2021)]  
  noise22$week_stag <- dates$week[match(noise22$DATE2, dates$date_2022)]
  
  #convert weeks to integers so they sort correctly
  # noise21$week_left <- as.integer(noise21$week_left)
  # noise22$week_left <- as.integer(noise22$week_left)
  class(noise21$week_stag); class(noise22$week_stag) #already integers
  
  #summarize 'mnSPL' by week (left)
  # noise21_wk_left <- group_by(noise21, week_left, site_name) %>% summarise(meanSPL = mean(mnSPL))
  # noise22_wk_left <- group_by(noise22, week_left, site_name) %>% summarise(meanSPL = mean(mnSPL))
  
  #summarize 'mnSPL' by week (staggered - matching)
  noise21_wk_stag <- group_by(noise21, week_stag, site_name) %>% summarise(meanSPL = mean(mnSPL))
  noise22_wk_stag <- group_by(noise22, week_stag, site_name) %>% summarise(meanSPL = mean(mnSPL))
  
  #standardize weekly 'mnSPL *or should we summarize the standardized daily meanSPL...?*
  # noise21_wk_left$meanSPL_std <- scale(noise21_wk_left$meanSPL)
  # noise22_wk_left$meanSPL_std <- scale(noise22_wk_left$meanSPL)
  noise21_wk_stag$meanSPL_std <- scale(noise21_wk_stag$meanSPL)
  noise22_wk_stag$meanSPL_std <- scale(noise22_wk_stag$meanSPL)
  
  #convert to wide (weekly)
  # noise21_wk_left_wide     <- pivot_wider(noise21_wk_left, id_cols = site_name, names_from = week_left, values_from = meanSPL) 
  # noise21_wk_left_wide_std <- pivot_wider(noise21_wk_left, id_cols = site_name, names_from = week_left, values_from = meanSPL_std) 
  noise21_wk_stag_wide     <- pivot_wider(noise21_wk_stag, id_cols = site_name, names_from = week_stag, values_from = meanSPL)
  noise21_wk_stag_wide_std <- pivot_wider(noise21_wk_stag, id_cols = site_name, names_from = week_stag, values_from = meanSPL_std)
  
  # noise22_wk_left_wide     <- pivot_wider(noise22_wk_left, id_cols = site_name, names_from = week_left, values_from = meanSPL)
  # noise22_wk_left_wide_std <- pivot_wider(noise22_wk_left, id_cols = site_name, names_from = week_left, values_from = meanSPL_std)
  noise22_wk_stag_wide     <- pivot_wider(noise22_wk_stag, id_cols = site_name, names_from = week_stag, values_from = meanSPL)
  noise22_wk_stag_wide_std <- pivot_wider(noise22_wk_stag, id_cols = site_name, names_from = week_stag, values_from = meanSPL_std)
  
  #sort by site name
  # noise21_wk_left_wide     <- noise21_wk_left_wide[order(noise21_wk_left_wide$site_name),] 
  # noise21_wk_left_wide_std <- noise21_wk_left_wide_std[order(noise21_wk_left_wide_std$site_name),] 
  noise21_wk_stag_wide     <- noise21_wk_stag_wide[order(noise21_wk_stag_wide$site_name),]
  noise21_wk_stag_wide_std <- noise21_wk_stag_wide_std[order(noise21_wk_stag_wide_std$site_name),]
  
  # noise22_wk_left_wide     <- noise22_wk_left_wide[order(noise22_wk_left_wide$site_name),] 
  # noise22_wk_left_wide_std <- noise22_wk_left_wide_std[order(noise22_wk_left_wide_std$site_name),] 
  noise22_wk_stag_wide     <- noise22_wk_stag_wide[order(noise22_wk_stag_wide$site_name),] 
  noise22_wk_stag_wide_std <- noise22_wk_stag_wide_std[order(noise22_wk_stag_wide_std$site_name),] 

    
  #combine years together
  #daily
    ##hmm... I will need to left-justify days if I really want to do this  
  
  #weekly (left)
  # noise21_wk_left_wide$year <- '2021'; noise22_wk_left_wide$year <- '2022'
  # noise21_wk_left_wide_std$year <- '2021'; noise22_wk_left_wide_std$year <- '2022'
  # 
  # noise_weekly_combined_raw <- bind_rows(noise21_wk_left_wide, noise22_wk_left_wide)
  # noise_weekly_combined_std <- bind_rows(noise21_wk_left_wide_std, noise22_wk_left_wide_std)

  #weekly (staggered with matching dates)
  noise21_wk_stag_wide$year     <- '2021'; noise22_wk_stag_wide$year <- '2022'
  noise21_wk_stag_wide_std$year <- '2021'; noise22_wk_stag_wide_std$year <- '2022'
  
  noise_weekly_combined_raw <- bind_rows(noise21_wk_stag_wide, noise22_wk_stag_wide)
  noise_weekly_combined_std <- bind_rows(noise21_wk_stag_wide_std, noise22_wk_stag_wide_std)
    
  #save
  write.csv(noise_weekly_combined_raw, 'output/10_occ_covariates/10_noise_weekly_raw_21-22.csv')
  write.csv(noise_weekly_combined_std, 'output/10_occ_covariates/10_noise_weekly_std_21-22.csv')
 
  
## Effort (recording seconds) -- staggered for years separately ----------------
  head(noise21)
  head(noise22)   #use 'durS' from this
  
  effort21 <- noise21;   effort22 <- noise22
  
  #correct sites with ~continuous recording
  max(effort21$durS); mean(effort21$durS)
  max(effort22$durS); mean(effort22$durS)
  
    #what's the intended recording amount? 
      #2.5 hours at sunrise + 2.5 hrs at sunset + 10 min for 3 hours = 5.5 hrs = 19800 seconds
      #but there's rounding, so how about change anything above 6 hrs (21600 sec) to 6 hrs
  
      View(effort21[effort21$durS > 21600,]) #we didn't get any spotted owls these sites/weeks anyway
      View(effort22[effort22$durS > 21600,]) #we did get spotted owls at DC_12 weeks 7 and 12, but other weeks as well
      
    #change these to 21600
    effort21[effort21$durS > 21600,]$durS <- 21600
    effort22[effort22$durS > 21600,]$durS <- 21600
    
    max(effort21$durS); mean(effort21$durS) #good
    max(effort22$durS); mean(effort22$durS) #good
    
  #now standardize durS
  effort21$durS_std <- scale(effort21$durS)
  effort22$durS_std <- scale(effort22$durS)
  
    #skip to years combined below or continue from here  
  
  #summarize 'durS' by week
  effort21_wk <- group_by(effort21, week_st, site_name) %>% summarise(totalS = sum(durS))
  effort22_wk <- group_by(effort22, week_st, site_name) %>% summarise(totalS = sum(durS))
  
  #standardize weekly 'durS' *or should we summarize the standardized daily durS?*
  effort21_wk$totalS_std <- scale(effort21_wk$totalS)
  effort22_wk$totalS_std <- scale(effort22_wk$totalS)
  
  #convert to wide (daily)
  effort21_dy_wide     <- pivot_wider(effort21, id_cols = site_name, names_from = DATE2, values_from = durS)
  effort21_dy_wide_std <- pivot_wider(effort21, id_cols = site_name, names_from = DATE2, values_from = durS_std)
  
  effort22_dy_wide     <- pivot_wider(effort22, id_cols = site_name, names_from = DATE2, values_from = durS)
  effort22_dy_wide_std <- pivot_wider(effort22, id_cols = site_name, names_from = DATE2, values_from = durS_std)
  
  #convert to wide (weekly)
  effort21_wk_wide     <- pivot_wider(effort21_wk, id_cols = site_name, names_from = week_st, values_from = totalS) 
  effort21_wk_wide_std <- pivot_wider(effort21_wk, id_cols = site_name, names_from = week_st, values_from = totalS_std) 
  
  effort22_wk_wide     <- pivot_wider(effort22_wk, id_cols = site_name, names_from = week_st, values_from = totalS)
  effort22_wk_wide_std <- pivot_wider(effort22_wk, id_cols = site_name, names_from = week_st, values_from = totalS_std)
  
  #sort by site name
  effort21_dy_wide     <- effort21_dy_wide[order(effort21_dy_wide$site_name),]
  effort21_dy_wide_std <- effort21_dy_wide_std[order(effort21_dy_wide_std$site_name),] 
  
  effort22_dy_wide     <- effort22_dy_wide[order(effort22_dy_wide$site_name),]
  effort22_dy_wide_std <- effort22_dy_wide_std[order(effort22_dy_wide_std$site_name),] 
  
  effort21_wk_wide     <- effort21_wk_wide[order(effort21_wk_wide$site_name),] 
  effort21_wk_wide_std <- effort21_wk_wide_std[order(effort21_wk_wide_std$site_name),] 
  
  effort22_wk_wide     <- effort22_wk_wide[order(effort22_wk_wide$site_name),] 
  effort22_wk_wide_std <- effort22_wk_wide_std[order(effort22_wk_wide_std$site_name),] 
  
  #save
  # write.csv(effort21_dy_wide,     'output/10_occ_covariates/10_effort_daily_raw_2021.csv')
  # write.csv(effort21_dy_wide_std, 'output/10_occ_covariates/10_effort_daily_std_2021.csv')
  # 
  # write.csv(effort22_dy_wide,     'output/10_occ_covariates/10_effort_daily_raw_2022.csv')
  # write.csv(effort22_dy_wide_std, 'output/10_occ_covariates/10_effort_daily_std_2022.csv')
  # 
  # write.csv(effort21_wk_wide,     'output/10_occ_covariates/10_effort_weekly_raw_2021.csv')
  # write.csv(effort21_wk_wide_std, 'output/10_occ_covariates/10_effort_weekly_std_2021.csv')
  # 
  # write.csv(effort22_wk_wide,     'output/10_occ_covariates/10_effort_weekly_raw_2022.csv')
  # write.csv(effort22_wk_wide_std, 'output/10_occ_covariates/10_effort_weekly_std_2022.csv')
   
   
## Effort (recording seconds) -- left-justified for both years combined ---------------
  head(effort21)
  head(effort22) 
  
  #load in week dates (staggered with matching calendar days for both years)
  dates <- fread('output/08_weekly_dates_staggered_21-22.csv')
  
  #match up weeks (will overwrite 'week_stag' from original -- were they already right?)
  effort21$week_stag <- dates$week[match(effort21$DATE2, dates$date_2021)]
  effort22$week_stag <- dates$week[match(effort22$DATE2, dates$date_2022)]
  
  #summarize 'durS' by week
    # effort21_wk_left <- group_by(effort21, week_left, site_name) %>% summarise(totalS = sum(durS))
    # effort22_wk_left <- group_by(effort22, week_left, site_name) %>% summarise(totalS = sum(durS))
    effort21_wk_stag <- group_by(effort21, week_stag, site_name) %>% summarise(totalS = sum(durS))
    effort22_wk_stag <- group_by(effort22, week_stag, site_name) %>% summarise(totalS = sum(durS))
  
  #standardize weekly 'durS' 
    # effort21_wk_left$totalS_std <- scale(effort21_wk_left$totalS)
    # effort22_wk_left$totalS_std <- scale(effort22_wk_left$totalS)
    effort21_wk_stag$totalS_std <- scale(effort21_wk_stag$totalS)
    effort22_wk_stag$totalS_std <- scale(effort22_wk_stag$totalS)
    
  #convert to wide (weekly)
    # effort21_wk_left_wide     <- pivot_wider(effort21_wk_left, id_cols = site_name, names_from = week_left, values_from = totalS)
    # effort21_wk_left_wide_std <- pivot_wider(effort21_wk_left, id_cols = site_name, names_from = week_left, values_from = totalS_std)
    effort21_wk_stag_wide     <- pivot_wider(effort21_wk_stag, id_cols = site_name, names_from = week_stag, values_from = totalS)
    effort21_wk_stag_wide_std <- pivot_wider(effort21_wk_stag, id_cols = site_name, names_from = week_stag, values_from = totalS_std)
  
    # effort22_wk_left_wide     <- pivot_wider(effort22_wk_left, id_cols = site_name, names_from = week_left, values_from = totalS)
    # effort22_wk_left_wide_std <- pivot_wider(effort22_wk_left, id_cols = site_name, names_from = week_left, values_from = totalS_std)
    effort22_wk_stag_wide     <- pivot_wider(effort22_wk_stag, id_cols = site_name, names_from = week_stag, values_from = totalS)
    effort22_wk_stag_wide_std <- pivot_wider(effort22_wk_stag, id_cols = site_name, names_from = week_stag, values_from = totalS_std)
  
  #sort by site name
    # effort21_wk_left_wide     <- effort21_wk_left_wide[order(effort21_wk_left_wide$site_name),]  
    # effort21_wk_left_wide_std <- effort21_wk_left_wide_std[order(effort21_wk_left_wide_std$site_name),]  
    effort21_wk_stag_wide    <- effort21_wk_stag_wide[order(effort21_wk_stag_wide$site_name),]
    effort21_wk_stag_wide_std <- effort21_wk_stag_wide_std[order(effort21_wk_stag_wide_std$site_name),]
    
    # effort22_wk_left_wide     <- effort22_wk_left_wide[order(effort22_wk_left_wide$site_name),]
    # effort22_wk_left_wide_std <- effort22_wk_left_wide_std[order(effort22_wk_left_wide_std$site_name),]
    effort22_wk_stag_wide     <- effort22_wk_stag_wide[order(effort22_wk_stag_wide$site_name),]
    effort22_wk_stag_wide_std <- effort22_wk_stag_wide_std[order(effort22_wk_stag_wide_std$site_name),]
    
  #combine years together
    # effort21_wk_left_wide$year <- '2021'; effort22_wk_left_wide$year <- '2022'
    # effort21_wk_left_wide_std$year <- '2021'; effort22_wk_left_wide_std$year <- '2022'
    effort21_wk_stag_wide$year     <- '2021'; effort22_wk_stag_wide$year <- '2022'  
    effort21_wk_stag_wide_std$year <- '2021'; effort22_wk_stag_wide_std$year <- '2022'  
    
    # effort_weekly_combined_raw <- bind_rows(effort21_wk_left_wide, effort22_wk_left_wide)
    # effort_weekly_combined_std <- bind_rows(effort21_wk_left_wide_std, effort22_wk_left_wide_std)
    effort_weekly_combined_raw <- bind_rows(effort21_wk_stag_wide, effort22_wk_stag_wide)
    effort_weekly_combined_std <- bind_rows(effort21_wk_stag_wide_std, effort22_wk_stag_wide_std)
      
  #save
  write.csv(effort_weekly_combined_raw, 'output/10_occ_covariates/10_effort_weekly_raw_21-22.csv')
  write.csv(effort_weekly_combined_std, 'output/10_occ_covariates/10_effort_weekly_std_21-22.csv')

  
## Barred owls (number of detections) -- staggered for years separately -----------
  stva21 <- fread('2021/Predictions_COA_AC_2021/stva_daily_counts_adjusted_2021.csv')
    head(stva21)
  stva22 <- fread('2022/AC_2022_PREDICTIONS/stva_daily_counts_adjusted_2022.csv')
    head(stva22)  
    
  #combine all barred owl call types
  stva21$STVA_ANY <- stva21$INSP + stva21$STVA + stva21$STVA_IRREG
  stva22$STVA_ANY <- stva22$INSP + stva22$STVA + stva22$STVA_IRREG
  
  #standardize daily 'STVA_ANY'
  stva21$STVA_ANY_std <- scale(stva21$STVA_ANY)
  stva22$STVA_ANY_std <- scale(stva22$STVA_ANY)    
    
    #skip to below for both years combined, or continue for years individually
    
  #load in week dates and match them up (staggered)
  dates21 <- fread('output/08_weekly_dates_staggered_2021.csv')    
  dates22 <- fread('output/08_weekly_dates_staggered_2022.csv')
  
  stva21$week_st <- dates21$week[match(stva21$Date, dates21$date)]
  stva22$week_st <- dates22$week[match(stva22$Date, dates22$date)]
  
  #summarize 'STVA_ANY' by week (total)
  stva21_wk <- group_by(stva21, week_st, Site) %>% summarise(STVA_ANY = sum(STVA_ANY))
  stva22_wk <- group_by(stva22, week_st, Site) %>% summarise(STVA_ANY = sum(STVA_ANY))
  
  #standardize weekly 'STVA_ANY'
  stva21_wk$STVA_ANY_std <- scale(stva21_wk$STVA_ANY)
  stva22_wk$STVA_ANY_std <- scale(stva22_wk$STVA_ANY)

  #convert to wide (daily)
  stva21_dy_wide     <- pivot_wider(stva21, id_cols = Site, names_from = Date, values_from = STVA_ANY)
  stva21_dy_wide_std <- pivot_wider(stva21, id_cols = Site, names_from = Date, values_from = STVA_ANY_std)
  
  stva22_dy_wide     <- pivot_wider(stva22, id_cols = Site, names_from = Date, values_from = STVA_ANY)
  stva22_dy_wide_std <- pivot_wider(stva22, id_cols = Site, names_from = Date, values_from = STVA_ANY_std)
  
  #convert to wide (weekly)
  stva21_wk_wide     <- pivot_wider(stva21_wk, id_cols = Site, names_from = week_st, values_from = STVA_ANY)
  stva21_wk_wide_std <- pivot_wider(stva21_wk, id_cols = Site, names_from = week_st, values_from = STVA_ANY_std)
  
  stva22_wk_wide     <- pivot_wider(stva22_wk, id_cols = Site, names_from = week_st, values_from = STVA_ANY) 
  stva22_wk_wide_std <- pivot_wider(stva22_wk, id_cols = Site, names_from = week_st, values_from = STVA_ANY_std)
  
  #sort by site name
  stva21_dy_wide     <- stva21_dy_wide[order(stva21_dy_wide$Site),]
  stva21_dy_wide_std <- stva21_dy_wide_std[order(stva21_dy_wide_std$Site),]

  stva22_dy_wide     <- stva22_dy_wide[order(stva22_dy_wide$Site),]
  stva22_dy_wide_std <- stva22_dy_wide_std[order(stva22_dy_wide_std$Site),]
  
  stva21_wk_wide     <- stva21_wk_wide[order(stva21_wk_wide$Site),]
  stva21_wk_wide_std <- stva21_wk_wide_std[order(stva21_wk_wide_std$Site),]
  
  stva21_wk_wide     <- stva22_wk_wide[order(stva22_wk_wide$Site),] 
  stva_wk_wide_std   <- stva22_wk_wide_std[order(stva22_wk_wide_std$Site),]

  #save
  write.csv(stva21_dy_wide,     'output/10_occ_covariates/10_stva_daily_raw_2021.csv')
  write.csv(stva21_dy_wide_std, 'output/10_occ_covariates/10_stva_daily_std_2021.csv')
  
  write.csv(stva22_dy_wide,     'output/10_occ_covariates/10_stva_daily_raw_2022.csv')
  write.csv(stva22_dy_wide_std, 'output/10_occ_covariates/10_stva_daily_std_2022.csv')
   
  write.csv(stva21_wk_wide,     'output/10_occ_covariates/10_stva_weekly_raw_2021.csv')
  write.csv(stva21_wk_wide_std, 'output/10_occ_covariates/10_stva_weekly_std_2021.csv')
   
  write.csv(stva22_wk_wide,     'output/10_occ_covariates/10_stva_weekly_raw_2022.csv')
  write.csv(stva22_wk_wide_std, 'output/10_occ_covariates/10_stva_weekly_std_2022.csv')
  
  
  
## Barred owls (number of detections) -- staggered with matching week dates for both years ------------
  head(stva21)
  head(stva22)
  
  #load in week dates and match them up (left-justified)
    # dates21_left <- fread('output/07_ac_21_dethist_long.csv'); colnames(dates21_left)[c(2:3)] <- c('Site', 'Date')
    # dates22_left <- fread('output/07_ac_22_dethist_long.csv'); colnames(dates22_left)[c(2:3)] <- c('Site', 'Date')
    
    # stva21 <- merge(stva21, dates21_left[,c('Site','Date','week_left')], by = c('Site','Date'), all.x = TRUE)
    # stva22 <- merge(stva22, dates22_left[,c('Site','Date','week_left')], by = c('Site','Date'), all.x = TRUE)
    
  #load in weeks (matching dates for both years)
    dates <- fread('output/08_weekly_dates_staggered_21-22.csv')
  
  #match up
  stva21$week_stag <- dates$week[match(stva21$Date, dates$date_2021)]
  stva22$week_stag <- dates$week[match(stva22$Date, dates$date_2022)]
  
  #convert weeks to integers so they sort correctly
    # stva21$week_left <- as.integer(stva21$week_left)
    # stva22$week_left <- as.integer(stva22$week_left)
    class(stva21$week_stag); class(stva22$week_stag) #already integers
  
  #summarize 'STVA_ANY' by week (left)
    # stva21_wk_left <- group_by(stva21, week_left, Site) %>% summarise(STVA_ANY = sum(STVA_ANY))
    # stva22_wk_left <- group_by(stva22, week_left, Site) %>% summarise(STVA_ANY = sum(STVA_ANY))
    stva21_wk_stag <- group_by(stva21, week_stag, Site) %>% summarise(STVA_ANY = sum(STVA_ANY))
    stva22_wk_stag <- group_by(stva22, week_stag, Site) %>% summarise(STVA_ANY = sum(STVA_ANY))
    
  #standardize weekly 'STVA_ANY' 
    # stva21_wk_left$STVA_ANY_std <- scale(stva21_wk_left$STVA_ANY)
    # stva22_wk_left$STVA_ANY_std <- scale(stva22_wk_left$STVA_ANY)
    stva21_wk_stag$STVA_ANY_std <- scale(stva21_wk_stag$STVA_ANY)
    stva22_wk_stag$STVA_ANY_std <- scale(stva22_wk_stag$STVA_ANY)
    
  #convert to wide (weekly)
    # stva21_wk_left_wide <- pivot_wider(stva21_wk_left, id_cols = Site, names_from = week_left, values_from = STVA_ANY)
    # stva21_wk_left_wide_std <- pivot_wider(stva21_wk_left, id_cols = Site, names_from = week_left, values_from = STVA_ANY_std)
    stva21_wk_stag_wide     <- pivot_wider(stva21_wk_stag, id_cols = Site, names_from = week_stag, values_from = STVA_ANY)
    stva21_wk_stag_wide_std <- pivot_wider(stva21_wk_stag, id_cols = Site, names_from = week_stag, values_from = STVA_ANY_std)
    
    # stva22_wk_left_wide <- pivot_wider(stva22_wk_left, id_cols = Site, names_from = week_left, values_from = STVA_ANY)
    # stva22_wk_left_wide_std <- pivot_wider(stva22_wk_left, id_cols = Site, names_from = week_left, values_from = STVA_ANY_std)
    stva22_wk_stag_wide     <- pivot_wider(stva22_wk_stag, id_cols = Site, names_from = week_stag, values_from = STVA_ANY)
    stva22_wk_stag_wide_std <- pivot_wider(stva22_wk_stag, id_cols = Site, names_from = week_stag, values_from = STVA_ANY_std)
    
  #sort by site name
    # stva21_wk_left_wide <- stva21_wk_left_wide[order(stva21_wk_left_wide$Site),]
    # stva21_wk_left_wide_std <- stva21_wk_left_wide_std[order(stva21_wk_left_wide_std$Site),]
    stva21_wk_stag_wide <- stva21_wk_stag_wide[order(stva21_wk_stag_wide$Site),]
    stva21_wk_stag_wide_std <- stva21_wk_stag_wide_std[order(stva21_wk_stag_wide_std$Site),]
    
    # stva22_wk_left_wide <- stva22_wk_left_wide[order(stva22_wk_left_wide$Site),]
    # stva22_wk_left_wide_std <- stva22_wk_left_wide_std[order(stva22_wk_left_wide_std$Site),]
    stva22_wk_stag_wide <- stva22_wk_stag_wide[order(stva22_wk_stag_wide$Site),]
    stva22_wk_stag_wide_std <- stva22_wk_stag_wide_std[order(stva22_wk_stag_wide_std$Site),]
  
  #combine years together
  
    #daily
    ##hmm... I will need to left-justify days if I really want to do this  
  
    #weekly (left)
    # stva21_wk_left_wide$year     <- '2021' ; stva22_wk_left_wide$year     <- '2022'
    # stva21_wk_left_wide_std$year <- '2021' ; stva22_wk_left_wide_std$year <- '2022'
    # 
    # stva_weekly_combined_raw <- bind_rows(stva21_wk_left_wide, stva22_wk_left_wide)
    # stva_weekly_combined_std <- bind_rows(stva21_wk_left_wide_std, stva22_wk_left_wide_std)

    #weekly (staggered with matching weeks)
    stva21_wk_stag_wide$year <- '2021'; stva22_wk_stag_wide$year <- '2022'
    stva21_wk_stag_wide_std$year <- '2021'; stva22_wk_stag_wide_std$year <- '2022'
    
    stva_weekly_combined_raw <- bind_rows(stva21_wk_stag_wide, stva22_wk_stag_wide)
    stva_weekly_combined_std <- bind_rows(stva21_wk_stag_wide_std, stva22_wk_stag_wide_std)
        
  #save
  write.csv(stva_weekly_combined_raw, 'output/10_occ_covariates/10_stva_weekly_raw_21-22.csv')
  write.csv(stva_weekly_combined_std, 'output/10_occ_covariates/10_stva_weekly_std_21-22.csv')
  
  #convert to site-level
  stva_weekly_combined_raw <- stva_weekly_combined_raw %>%
    rowwise() %>%
    mutate(n_detections = sum(c_across(c(2:21,23:24)), na.rm = TRUE))
  write.csv(stva_weekly_combined_raw[,c('Site','n_detections')], 'output/10_occ_covariates/10_stva_wekly_raw_21-22_site.csv')
  

## Barred owls (number of days with detections per week) -- staggered with matching week dates for both years ------------
  head(stva21)
  head(stva22)

  #summarize 'STVA_ANY' by week (how many days had detections each week)
  stva21_wk_stag_days <- group_by(stva21, week_stag, Site) %>% summarise(STVA_ANY_Days = length(STVA_ANY[STVA_ANY > 0]))
  stva22_wk_stag_days <- group_by(stva22, week_stag, Site) %>% summarise(STVA_ANY_Days = length(STVA_ANY[STVA_ANY > 0]))
  
  #standardize?
  stva21_wk_stag_days$STVA_ANY_Days_std <- scale(stva21_wk_stag_days$STVA_ANY_Days)
  stva22_wk_stag_days$STVA_ANY_Days_std <- scale(stva22_wk_stag_days$STVA_ANY_Days)
  
  #convert to wide
  stva21_wk_stag_days_wide     <- pivot_wider(stva21_wk_stag_days, id_cols = Site, names_from = week_stag, values_from = STVA_ANY_Days)
  stva21_wk_stag_days_wide_std <- pivot_wider(stva21_wk_stag_days, id_cols = Site, names_from = week_stag, values_from = STVA_ANY_Days_std)

  stva22_wk_stag_days_wide     <- pivot_wider(stva22_wk_stag_days, id_cols = Site, names_from = week_stag, values_from = STVA_ANY_Days)
  stva22_wk_stag_days_wide_std <- pivot_wider(stva22_wk_stag_days, id_cols = Site, names_from = week_stag, values_from = STVA_ANY_Days_std)

  #sort by site name
  stva21_wk_stag_days_wide     <- stva21_wk_stag_days_wide[order(stva21_wk_stag_days_wide$Site),]
  stva21_wk_stag_days_wide_std <- stva21_wk_stag_days_wide_std[order(stva21_wk_stag_days_wide_std$Site),]
  
  stva22_wk_stag_days_wide     <- stva22_wk_stag_days_wide[order(stva22_wk_stag_days_wide$Site),]
  stva22_wk_stag_days_wide_std <- stva22_wk_stag_days_wide_std[order(stva22_wk_stag_days_wide_std$Site),]

  #combine years together
  stva21_wk_stag_days_wide$year <- '2021'; stva22_wk_stag_days_wide$year <- '2022'
  stva21_wk_stag_days_wide_std$year <- '2021'; stva22_wk_stag_days_wide_std$year <- '2022'
  
  stva_weekly_days_combined_raw <- bind_rows(stva21_wk_stag_days_wide, stva22_wk_stag_days_wide)
  stva_weekly_days_combined_std <- bind_rows(stva21_wk_stag_days_wide_std, stva22_wk_stag_days_wide_std)

  #save
  write.csv(stva_weekly_days_combined_raw, 'output/10_occ_covariates/10_stva_daysPerWeek_raw_21-22.csv')
  write.csv(stva_weekly_days_combined_std, 'output/10_occ_covariates/10_stva_daysPerWeek_std_21-22.csv')
  
  #convert to site-level
  stva_weekly_days_combined_raw <- stva_weekly_days_combined_raw %>%
    rowwise() %>%
    mutate(n_days = sum(c_across(c(2:21,23:24)), na.rm = TRUE))
  write.csv(stva_weekly_days_combined_raw[,c('Site','n_days'),], 'output/10_occ_covariates/10_stva_daysPerWeek_raw_21-22_site.csv')
  

## Add stva to site-level
  site_level <- fread('output/10_occ_covariates/10_site_level_2021-2022.csv')
    head(site_level)

  site_level$bo_total <- stva_weekly_combined_raw$n_detections[match(site_level$site_stn, stva_weekly_combined_raw$Site)]
  site_level$bo_days  <- stva_weekly_days_combined_raw$n_days[match(site_level$site_stn, stva_weekly_days_combined_raw$Site)]

  write.csv(site_level, 'output/10_occ_covariates/10_site_level_2021-2022.csv')  
  