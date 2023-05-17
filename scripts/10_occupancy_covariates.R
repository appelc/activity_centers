# formatting and saving covariates for occupancy models using activity center (big grid) data

library(data.table)
library(tidyverse)


## SITE-LEVEL COVARIATES -------------------------------------------------------

## Distance, site, reproductive state, nesting attempt -------------------------

  #load detections with site-level data
  info21 <- fread('output/05_ac_2021_merged.csv')
  info22 <- fread('output/05_ac_2022_merged.csv')
  
  #pull out columns we need and format
  site21 <- info21[,c('SITESTN','duration','reproState','Distance','dist_intended','XNAD83FP','YNAD83FP')]
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
    
  #combine and save
    site_level_covar <- list(site21,site22)
    saveRDS(site_level_covar, 'output/10_occ_covariates/10_site_covariates.rds')
 
    
## Barred owls -----------------------------------------------------------------
    
    
    
## NR habitat ------------------------------------------------------------------
    
    
  
## SURVEY-LEVEL ----------------------------------------------------------------

## Noise (mean daily SPL) ------------------------------------------------------
  noise21 <- readRDS('2021/COA_AC_Work/from_Julie/Codes/noise_2021_COA_AC.rds')
  noise22 <- readRDS('2022/ac_22_dailyNoise.rds')
    head(noise21)
    head(noise22)
    
  #load in week dates and match them up
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
  write.csv(noise21_dy_wide,     'output/10_occ_covariates/10_noise_daily_raw_2021.csv')
  write.csv(noise21_dy_wide_std, 'output/10_occ_covariates/10_noise_daily_std_2021.csv')
  
  write.csv(noise22_dy_wide,     'output/10_occ_covariates/10_noise_daily_raw_2022.csv')
  write.csv(noise22_dy_wide_std, 'output/10_occ_covariates/10_noise_daily_std_2022.csv')
  
  write.csv(noise21_wk_wide,     'output/10_occ_covariates/10_noise_weekly_raw_2021.csv')
  write.csv(noise21_wk_wide_std, 'output/10_occ_covariates/10_noise_weekly_std_2021.csv')
  
  write.csv(noise22_wk_wide,     'output/10_occ_covariates/10_noise_weekly_raw_2022.csv')
  write.csv(noise22_wk_wide_std, 'output/10_occ_covariates/10_noise_weekly_std_2022.csv')
  
  
## Effort (recording seconds) --------------------------------------------------
  head(noise21)
  head(noise22)   #use 'durS' from this
  
  #standardize daily 'durS'
  effort21 <- noise21; effort21$durS_std <- scale(effort21$durS)
  effort22 <- noise22; effort22$durS_std <- scale(effort22$durS)
  
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
  write.csv(effort21_dy_wide,     'output/10_occ_covariates/10_effort_daily_raw_2021.csv')
  write.csv(effort21_dy_wide_std, 'output/10_occ_covariates/10_effort_daily_std_2021.csv')
  
  write.csv(effort22_dy_wide,     'output/10_occ_covariates/10_effort_daily_raw_2022.csv')
  write.csv(effort22_dy_wide_std, 'output/10_occ_covariates/10_effort_daily_std_2022.csv')
  
  write.csv(effort21_wk_wide,     'output/10_occ_covariates/10_effort_weekly_raw_2021.csv')
  write.csv(effort21_wk_wide_std, 'output/10_occ_covariates/10_effort_weekly_std_2021.csv')
  
  write.csv(effort22_wk_wide,     'output/10_occ_covariates/10_effort_weekly_raw_2022.csv')
  write.csv(effort22_wk_wide_std, 'output/10_occ_covariates/10_effort_weekly_std_2022.csv')
    