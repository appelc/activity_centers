## Occupancy models with activity center ("big grid") data

library(data.table)
library(unmarked)
library(ggplot2)
library(reshape2)
library(dplyr)


## Set up folder for model outputs & figures -----------------------------------
  ifelse(!dir.exists(paste('output/11_occ_models', Sys.Date(), sep = '/')), 
         dir.create(paste('output/11_occ_models', Sys.Date(), sep = '/')), 'Folder exists already')
  print(paste('created folder', paste('output/11_occ_models', Sys.Date(), sep = '/')))
  
  ifelse(!dir.exists(paste('figures/occ_models', Sys.Date(), sep = '/')), 
         dir.create(paste('figures/occ_models', Sys.Date(), sep = '/')), 'Folder exists already')
  print(paste('created folder', paste('figures/occ_models', Sys.Date(), sep = '/')))


## Set up dataframe for model results ------------------------------------------
  modResults <- structure(list(model = character(), type = character(), cov = character(),
                               psi_est = numeric(), psi_se = numeric(), p_est = numeric(), p_se = numeric()),
                          class = 'data.frame')    

  
## Load covariates -------------------------------------------------------------
  
  ## Site-level
  site_covars <- fread('output/10_occ_covariates/10_site_level_2021-2022.csv')

    ## Make sure it's sorted alphabetically by site
    site_covars$year_site <- paste(site_covars$year, site_covars$site_stn, sep = '_')
    site_covars <- site_covars[order(site_covars$year_site),]
  
    #also need Barred owl and NR habitat covariates
      
    
  ## Observation-level
  effort <- fread('output/10_occ_covariates/10_effort_weekly_std_21-22.csv', header = TRUE)
  noise  <- fread('output/10_occ_covariates/10_noise_weekly_std_21-22.csv', header = TRUE)
  
    ## Make sure sites are sorted alphabetically, then delete that column
    effort$year_site <- paste(effort$year, effort$site_name, sep = '_')
      effort <- effort[order(effort$year_site),]
      effort_covar <- effort[,-c('V1','site_name','year','year_site')]
        
    noise$year_site <- paste(noise$year, noise$site_name, sep = '_')
      noise <- noise[order(noise$year_site),]
      noise_covar <- noise[,-c('V1','site_name','year','year_site')]
      
    ## Replace NAs with mean value (0)
    effort_covar[is.na(effort_covar)] <- mean(unlist(effort_covar), na.rm = TRUE) #mean should be ~0 because it's standardized
    noise_covar[is.na(noise_covar)] <- mean(unlist(noise_covar), na.rm = TRUE)
    
    ## Format some site-level as observation-level
    dist_obs_covar <- matrix(data = site_covars$dist_actual_std, nrow = 291, ncol = 21)
    nest_obs_covar <- matrix(data = site_covars$nested, nrow = 291, ncol = 21)
    site_obs_covar <- matrix(data = site_covars$site_name, nrow = 291, ncol = 21)
    
    ## Create time-varying covariate
    time <- matrix(paste('visit', rep(1:ncol(effort_covar)), sep = '_'),
                   nrow = nrow(effort_covar),
                   ncol = ncol(effort_covar), 
                   byrow = TRUE)
  
    
## Load detection histories ----------------------------------------------------
  dh_any    <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocAny_left.csv')
  dh_female <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocFemale_left.csv')
  dh_male   <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocMale_left.csv')
  dh_female_fnlcp <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocFemaleFNLCorPair_left.csv')
  dh_male_fnlcp   <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocMaleFNLCorPair_left.csv')
  dh_female_fnlc <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocFemaleFNLC_left.csv')
  dh_male_fnlc   <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocMaleFNLC_left.csv')
  
  ## Make sure sites are sorted alphabetically, then delete that column  
  dh_any    <- dh_any[order(dh_any$V1,)];      dh_any <- dh_any[,-c('V1')]
  dh_female <- dh_female[order(dh_female$V1)]; dh_female <- dh_female[,-c('V1')]
  dh_male   <- dh_male[order(dh_male$V1)];     dh_male <- dh_male[,-c('V1')]
  dh_female_fnlcp <- dh_female_fnlcp[order(dh_female_fnlcp$V1)]; dh_female_fnlcp <- dh_female_fnlcp[,-c('V1')]
  dh_male_fnlcp   <- dh_male_fnlcp[order(dh_male_fnlcp$V1)];     dh_male_fnlcp <- dh_male_fnlcp[,-c('V1')]
  dh_female_fnlc <- dh_female_fnlc[order(dh_female_fnlc$V1)]; dh_female_fnlc <- dh_female_fnlc[,-c('V1')]  
  dh_male_fnlc   <- dh_male_fnlc[order(dh_male_fnlc$V1)];     dh_male_fnlc <- dh_male_fnlc[,-c('V1')]

      
## Create input files ----------------------------------------------------------    
  dim(dh_any); dim(dh_female); dim(dh_male); dim(dh_female_fnlcp); dim(dh_male_fnlcp); dim(dh_female_fnlc); dim(dh_male_fnlc)
  dim(site_covars); dim(effort_covar); dim(noise_covar)
  
  input_any <- unmarkedFrameOccu(y = dh_any, obsCovs = list(effort = effort_covar,
                                                            noise = noise_covar,
                                                            dist = dist_obs_covar,
                                                            nest = nest_obs_covar,
                                                            site = site_obs_covar, 
                                                            time = time), siteCovs = site_covars)
    str(input_any)
    plot(input_any, main = 'Any STOC')    #optionally 'panels = 4' e.g., to split up y-axis
    summary(input_any) #100/291 sites with at least one detection (naive occ = 0.344)   
    
  input_female <- unmarkedFrameOccu(y = dh_female, obsCovs = list(effort = effort_covar,
                                                                  noise = noise_covar,
                                                                  dist = dist_obs_covar,
                                                                  nest = nest_obs_covar,
                                                                  site = site_obs_covar,
                                                                  time = time), siteCovs = site_covars)
    str(input_female)    
    plot(input_female, main = 'Females')
    summary(input_female) #55/291 sites with at least one detection (naive occ = 0.189)
    
  input_female_fnlcp <- unmarkedFrameOccu(y = dh_female_fnlcp, obsCovs = list(effort = effort_covar,
                                                                  noise = noise_covar,
                                                                  dist = dist_obs_covar,
                                                                  nest = nest_obs_covar,
                                                                  site = site_obs_covar,
                                                                  time = time), siteCovs = site_covars)
    str(input_female_fnlcp)  
    plot(input_female_fnlcp, main = 'Female FNLC or Pair')
    summary(input_female_fnlcp) #naive occ = 53/291 = 0.182
  
  input_female_fnlc <- unmarkedFrameOccu(y = dh_female_fnlc, obsCovs = list(effort = effort_covar,
                                                                            noise = noise_covar,
                                                                            dist = dist_obs_covar,
                                                                            nest = nest_obs_covar,
                                                                            site = site_obs_covar,
                                                                            time = time), siteCovs = site_covars)
    str(input_female_fnlc)  
    plot(input_female_fnlc, main = 'Female FNLC only')
    summary(input_female_fnlc) #naive occ = 53/291 = 0.182
    
  input_male <- unmarkedFrameOccu(y = dh_male, obsCovs = list(effort = effort_covar,
                                                              noise = noise_covar,
                                                              dist = dist_obs_covar,
                                                              nest = nest_obs_covar,
                                                              site = site_obs_covar,
                                                              time = time), siteCovs = site_covars)  
    str(input_male)
    plot(input_male, main = 'Males')
    summary(input_male) #88/291 sites with at least one detection (naive occ = 0.302)
    
    
  input_male_fnlcp <- unmarkedFrameOccu(y = dh_male_fnlcp, obsCovs = list(effort = effort_covar,
                                                              noise = noise_covar,
                                                              dist = dist_obs_covar,
                                                              nest = nest_obs_covar,
                                                              site = site_obs_covar,
                                                              time = time), siteCovs = site_covars)  
    str(input_male_fnlcp)
    plot(input_male_fnlcp, main = 'Male FNLC or pair')
    summary(input_male_fnlcp) #naive occ = 88/291 = 0.302
    
  input_male_fnlc <- unmarkedFrameOccu(y = dh_male_fnlc, obsCovs = list(effort = effort_covar,
                                                                        noise = noise_covar,
                                                                        dist = dist_obs_covar,
                                                                        nest = nest_obs_covar,
                                                                        site = site_obs_covar,
                                                                        time = time), siteCovs = site_covars)  
    str(input_male_fnlc)
    plot(input_male_fnlc, main = 'Male FNLC')
    summary(input_male_fnlc) #naive occ = 88/291 = 0.302          
    
        
## Run models ------------------------------------------------------------------

  #ANY detections (constant psi)
    p1_any <- occu(~1                           ~1, data = input_any)
    p2_any <- occu(~dist                        ~1, data = input_any)
    p3_any <- occu(~dist + site                 ~1, data = input_any)
    p4_any <- occu(~dist +        nest          ~1, data = input_any)
    p5_any <- occu(~dist +               effort ~1, data = input_any)
    p6_any <- occu(~                     effort ~1, data = input_any)
    p7_any <- occu(~dist + site +        effort ~1, data = input_any) #top model
    p8_any <- occu(~dist +        nest + effort ~1, data = input_any)
    
    modSel(fitList(p1_any, p2_any, p3_any, p4_any, p5_any, p6_any, p7_any, p8_any))
      #top mod is p7; next is p3 but deltaAIC is 37         
    
    
  #FEMALE detections (constant psi)
    p1_fem <- occu(~1                           ~1, data = input_female)
    p2_fem <- occu(~dist                        ~1, data = input_female)
    p3_fem <- occu(~dist + site                 ~1, data = input_female)
    p4_fem <- occu(~dist +        nest          ~1, data = input_female)
    p5_fem <- occu(~dist +               effort ~1, data = input_female)
    p6_fem <- occu(~                     effort ~1, data = input_female)
    p7_fem <- occu(~dist + site +        effort ~1, data = input_female) #top model
    p8_fem <- occu(~dist +        nest + effort ~1, data = input_female)
    
    modSel(fitList(p1_fem, p2_fem, p3_fem, p4_fem, p5_fem, p6_fem, p7_fem, p8_fem))
    #top mod is p7; next is p3 but deltaAIC is 8     
    
    
  #FEMALE (FNLC or pair) detections (constant psi)
    p1_fem_fnlcp <- occu(~1                           ~1, data = input_female_fnlcp)
    p2_fem_fnlcp <- occu(~dist                        ~1, data = input_female_fnlcp)
    p3_fem_fnlcp <- occu(~dist + site                 ~1, data = input_female_fnlcp)
    p4_fem_fnlcp <- occu(~dist +        nest          ~1, data = input_female_fnlcp)
    p5_fem_fnlcp <- occu(~dist +               effort ~1, data = input_female_fnlcp)
    p6_fem_fnlcp <- occu(~                     effort ~1, data = input_female_fnlcp)
    p7_fem_fnlcp <- occu(~dist + site +        effort ~1, data = input_female_fnlcp) #top model
    p8_fem_fnlcp <- occu(~dist +        nest + effort ~1, data = input_female_fnlcp)
    
    modSel(fitList(p1_fem_fnlcp, p2_fem_fnlcp, p3_fem_fnlcp, p4_fem_fnlcp, p5_fem_fnlcp, 
                   p6_fem_fnlcp, p7_fem_fnlcp, p8_fem_fnlcp))
    #top mod is p7; next is p3 but deltaAIC is 6     
    
    
  #FEMALE (FNLC or pair) detections (constant psi)
    p1_fem_fnlc <- occu(~1                           ~1, data = input_female_fnlc)
    p2_fem_fnlc <- occu(~dist                        ~1, data = input_female_fnlc)
    p3_fem_fnlc <- occu(~dist + site                 ~1, data = input_female_fnlc)
    p4_fem_fnlc <- occu(~dist +        nest          ~1, data = input_female_fnlc)
    p5_fem_fnlc <- occu(~dist +               effort ~1, data = input_female_fnlc)
    p6_fem_fnlc <- occu(~                     effort ~1, data = input_female_fnlc)
    p7_fem_fnlc <- occu(~dist + site +        effort ~1, data = input_female_fnlc) #top model
    p8_fem_fnlc <- occu(~dist +        nest + effort ~1, data = input_female_fnlc)
    
    modSel(fitList(p1_fem_fnlc, p2_fem_fnlc, p3_fem_fnlc, p4_fem_fnlc, p5_fem_fnlc, 
                   p6_fem_fnlc, p7_fem_fnlc, p8_fem_fnlc))
    #top mod is p7; next is p3 but deltaAIC is 6
    
    
  #MALE detections (constant psi)
    p1_mal <- occu(~1                           ~1, data = input_male)
    p2_mal <- occu(~dist                        ~1, data = input_male)
    p3_mal <- occu(~dist + site                 ~1, data = input_male)
    p4_mal <- occu(~dist +        nest          ~1, data = input_male)
    p5_mal <- occu(~dist +               effort ~1, data = input_male)
    p6_mal <- occu(~                     effort ~1, data = input_male)
    p7_mal <- occu(~dist + site +        effort ~1, data = input_male) #top model
    p8_mal <- occu(~dist +        nest + effort ~1, data = input_male)
    
    modSel(fitList(p1_mal, p2_mal, p3_mal, p4_mal, p5_mal, p6_mal, p7_mal, p8_mal))
    #top mod is p7; next is p3 but deltaAIC is 30
    
  
  #MALE (FNLC or pair) detections (constant psi)
    p1_mal_fnlcp <- occu(~1                           ~1, data = input_male_fnlcp)
    p2_mal_fnlcp <- occu(~dist                        ~1, data = input_male_fnlcp)
    p3_mal_fnlcp <- occu(~dist + site                 ~1, data = input_male_fnlcp)
    p4_mal_fnlcp <- occu(~dist +        nest          ~1, data = input_male_fnlcp)
    p5_mal_fnlcp <- occu(~dist +               effort ~1, data = input_male_fnlcp)
    p6_mal_fnlcp <- occu(~                     effort ~1, data = input_male_fnlcp)
    p7_mal_fnlcp <- occu(~dist + site +        effort ~1, data = input_male_fnlcp) #top model
    p8_mal_fnlcp <- occu(~dist +        nest + effort ~1, data = input_male_fnlcp)
    
    modSel(fitList(p1_mal_fnlcp, p2_mal_fnlcp, p3_mal_fnlcp, p4_mal_fnlcp, p5_mal_fnlcp, 
                   p6_mal_fnlcp, p7_mal_fnlcp, p8_mal_fnlcp))
    #top mod is p7; next is p3 but deltaAIC is 30
    
    
  #MALE (FNLC or pair) detections (constant psi)
    p1_mal_fnlc <- occu(~1                           ~1, data = input_male_fnlc)
    p2_mal_fnlc <- occu(~dist                        ~1, data = input_male_fnlc)
    p3_mal_fnlc <- occu(~dist + site                 ~1, data = input_male_fnlc)
    p4_mal_fnlc <- occu(~dist +        nest          ~1, data = input_male_fnlc)
    p5_mal_fnlc <- occu(~dist +               effort ~1, data = input_male_fnlc)
    p6_mal_fnlc <- occu(~                     effort ~1, data = input_male_fnlc)
    p7_mal_fnlc <- occu(~dist + site +        effort ~1, data = input_male_fnlc) #top model
    p8_mal_fnlc <- occu(~dist +        nest + effort ~1, data = input_male_fnlc)
    
    modSel(fitList(p1_mal_fnlc, p2_mal_fnlc, p3_mal_fnlc, p4_mal_fnlc, p5_mal_fnlc, 
                   p6_mal_fnlc, p7_mal_fnlc, p8_mal_fnlc))
    #top mod is p7; next is p3 but deltaAIC is 29   
    
    
  #Examine results

    
  #Plot
    #format covariate values to predict on
    dist_grid   <- sort(c(0,seq(min(site_covars$dist_actual_std), max(site_covars$dist_actual_std), length = 50)))
    effort_grid <- sort(c(0,seq(min(effort_covar), max(effort_covar), length = 50)))
    site_grid   <- unique(site_covars$site_name) 
    
      new_covar <- expand.grid('dist' = dist_grid, 'effort' = effort_grid, 'site' = site_grid)
    
    #predict
    det_pred_p7any <- predict(p7_any, type = 'det', new_covar)
    det_pred_p7fem <- predict(p7_fem, type = 'det', new_covar)
    det_pred_p7mal <- predict(p7_mal, type = 'det', new_covar)
    
    occ_pred_p7any <- predict(p7_any, type = 'state', new_covar)
    occ_pred_p7fem <- predict(p7_fem, type = 'state', new_covar)
    occ_pred_p7mal <- predict(p7_mal, type = 'state', new_covar)

    #format for plotting and back-transform cov
    effort_raw <- fread('output/10_occ_covariates/10_effort_weekly_raw_21-22.csv')
    effort_raw$site <- sapply(strsplit(effort_raw$site_name, '\\_'), '[', 1)
    effort_raw_dc <- data.frame('site' = 'DC',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'DC',]))
    effort_raw_mc <- data.frame('site' = 'MC',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'MC',]))
    effort_raw_ug <- data.frame('site' = 'UG',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'UG',]))
    effort_raw_wc <- data.frame('site' = 'WC',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'WC',]))
    effort_raw_bc <- data.frame('site' = 'BC',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'BC',]))
    effort_raw_cc <- data.frame('site' = 'CC',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'CC',]))
    effort_raw_lm <- data.frame('site' = 'LM',
                                'effort_raw' = unlist(effort_raw[, c(3:22,24)][effort_raw$site %in% 'LM',]))
    effort_raw_df <- do.call('rbind', list(effort_raw_dc, effort_raw_mc, effort_raw_ug, effort_raw_wc,
                                           effort_raw_bc, effort_raw_cc, effort_raw_lm))
    
    #
    pred_wide_any <- cbind(new_covar, 'det' = det_pred_p7any$Predicted, 'occ' = occ_pred_p7any$Predicted)
    lci_wide_any  <- cbind(new_covar, 'det' = det_pred_p7any$lower, 'occ' = occ_pred_p7any$lower)
    uci_wide_any  <- cbind(new_covar, 'det' = det_pred_p7any$upper, 'occ' = occ_pred_p7any$upper)
 
    pred_long_any <- melt(pred_wide_any, id.vars = c('dist','effort','site'), value.name = 'value')
    lci_long_any  <- melt(lci_wide_any, id.vars = c('dist','effort','site'), value.name = 'LCI')
    uci_long_any  <- melt(uci_wide_any, id.vars = c('dist','effort','site'), value.name = 'UCI')
    
    plot_data_any <- cbind(pred_long_any, 'LCI' = lci_long_any$LCI, 'UCI' = uci_long_any$UCI)
    plot_data_any$dist_bt   <- (plot_data_any$dist * sd(site_covars$dist_actual)) + mean(site_covars$dist_actual)
    plot_data_any$effort_bt <- (plot_data_any$effort * sd(effort_raw_df$effort_raw, na.rm = TRUE)) + 
      mean(effort_raw_df$effort_raw, na.rm = TRUE)
    
    #
    pred_wide_fem <- cbind(new_covar, 'det' = det_pred_p7fem$Predicted, 'occ' = occ_pred_p7fem$Predicted)
    lci_wide_fem  <- cbind(new_covar, 'det' = det_pred_p7fem$lower, 'occ' = occ_pred_p7fem$lower)
    uci_wide_fem  <- cbind(new_covar, 'det' = det_pred_p7fem$upper, 'occ' = occ_pred_p7fem$upper)
    
    pred_long_fem <- melt(pred_wide_fem, id.vars = c('dist','effort','site'), value.name = 'value')
    lci_long_fem  <- melt(lci_wide_fem, id.vars = c('dist','effort','site'), value.name = 'LCI')
    uci_long_fem  <- melt(uci_wide_fem, id.vars = c('dist','effort','site'), value.name = 'UCI')
    
    plot_data_fem <- cbind(pred_long_fem, 'LCI' = lci_long_fem$LCI, 'UCI' = uci_long_fem$UCI)
    plot_data_fem$dist_bt   <- (plot_data_fem$dist * sd(site_covars$dist_actual)) + mean(site_covars$dist_actual)
    plot_data_fem$effort_bt <- (plot_data_fem$effort * sd(effort_raw_df$effort_raw, na.rm = TRUE)) + 
      mean(effort_raw_df$effort_raw, na.rm = TRUE)
    
    #
    pred_wide_mal <- cbind(new_covar, 'det' = det_pred_p7mal$Predicted, 'occ' = occ_pred_p7mal$Predicted)
    lci_wide_mal  <- cbind(new_covar, 'det' = det_pred_p7mal$lower, 'occ' = occ_pred_p7mal$lower)
    uci_wide_mal  <- cbind(new_covar, 'det' = det_pred_p7mal$upper, 'occ' = occ_pred_p7mal$upper)
    
    pred_long_mal <- melt(pred_wide_mal, id.vars = c('dist','effort','site'), value.name = 'value')
    lci_long_mal  <- melt(lci_wide_mal, id.vars = c('dist','effort','site'), value.name = 'LCI')
    uci_long_mal  <- melt(uci_wide_mal, id.vars = c('dist','effort','site'), value.name = 'UCI')
    
    plot_data_mal <- cbind(pred_long_mal, 'LCI' = lci_long_mal$LCI, 'UCI' = uci_long_mal$UCI)
    plot_data_mal$dist_bt   <- (plot_data_mal$dist * sd(site_covars$dist_actual)) + mean(site_covars$dist_actual)
    plot_data_mal$effort_bt <- (plot_data_mal$effort * sd(effort_raw_df$effort_raw, na.rm = TRUE)) + 
      mean(effort_raw_df$effort_raw, na.rm = TRUE) 
    
  #Plot    
    plot_data_any$group <- 'Any'; plot_data_fem$group <- 'Female'; plot_data_mal$group <- 'Male'
    plot_combine <- do.call('rbind', list(plot_data_any, plot_data_fem, plot_data_mal))
    
    #All together
    site_covars$site <- site_covars$site_name
    plot_p7_dist <- ggplot(plot_combine[plot_combine$effort == 0 & plot_combine$variable %in% 'det',], 
                              aes(x = dist_bt/1000, y = value, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) + #color=NA to remove outlines
      ylim(0,1) +
      ylab('Weekly detection probability (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle('p(distance + effort) psi(.)') +
      geom_rug(data = site_covars, aes(x = dist_actual/1000), inherit.aes = FALSE) +
      facet_grid(~ site) + theme_bw()
    plot_p7_dist
    
    #Any
    plot_p7any_dist <- ggplot(plot_data_any[plot_data_any$effort == 0 & plot_data_any$variable %in% 'det',], 
                         aes(x = dist_bt/1000, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
      ylim(0,1) +
      ylab('Weekly detection probability (\u00B1 95 CI)') +
      xlab('Distance (km)') +
      ggtitle('Any detections: p(distance + effort) psi(.)') +
      geom_rug(data = site_covars, aes(x = dist_intended/1000), inherit.aes = FALSE) +
      facet_grid(~ site) + theme_bw()
    plot_p7any_dist
    
    plot_p7any_effort <- ggplot(plot_data_any[plot_data_any$dist == 0 & plot_data_any$variable %in% 'det',], 
                         aes(x = effort_bt, y = value)) +
      geom_line() +
      geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
      ylim(0,1) +
      ylab('Weekly detection probability (\u00B1 95 CI)') +
      xlab('Effort') +
      ggtitle('Any detections: p(distance + effort) psi(.)') +
      geom_rug(data = effort_raw_df, aes(x = effort_raw), inherit.aes = FALSE) +
      facet_grid(~ site) + theme_bw()
    plot_p7any_effort
    
      #Ok, there's really just one outlier at Miller Creek with very large effort. Should truncate that
      #There shouldn't be *this* strong of a relationsihpo with effort... we didn't actually sample out that much
    
    
## Time-varying 'p' ------------------------------------------------------------
    
  #ANY detections (constant psi) - time-varying p
    p1_any_time <- occu(~1                                  ~1, data = input_any)
    p2_any_time <- occu(~dist + time                 + time ~1, data = input_any)
    p3_any_time <- occu(~dist + site                 + time ~1, data = input_any) #top model
    p4_any_time <- occu(~dist +        nest          + time ~1, data = input_any)
    p5_any_time <- occu(~dist +               effort + time ~1, data = input_any) #error
    p6_any_time <- occu(~                     effort + time ~1, data = input_any) #error
    p7_any_time <- occu(~dist + site +        effort + time ~1, data = input_any) #error
    p8_any_time <- occu(~dist +        nest + effort + time ~1, data = input_any) #error
    p9_any_time <- occu(~                              time ~1, data = input_any)
    
    modSel(fitList(p1_any_time, p2_any_time, p3_any_time, p4_any_time, p9_any_time))
    #top mod is p3; next is p2 but deltaAIC is 116
    
    
  #FEMALE FNLCP detections (constant psi) - time-varying p
    p1_fem_fnlcp_time <- occu(~1                                  ~1, data = input_female_fnlcp)
    p2_fem_fnlcp_time <- occu(~dist                        + time ~1, data = input_female_fnlcp)
    p3_fem_fnlcp_time <- occu(~dist + site                 + time ~1, data = input_female_fnlcp) #top model
    p4_fem_fnlcp_time <- occu(~dist +        nest          + time ~1, data = input_female_fnlcp) #error? NAs
    p5_fem_fnlcp_time <- occu(~dist +               effort + time ~1, data = input_female_fnlcp) #error
    p6_fem_fnlcp_time <- occu(~                     effort + time ~1, data = input_female_fnlcp)
    p7_fem_fnlcp_time <- occu(~dist + site +        effort + time ~1, data = input_female_fnlcp) #error
    p8_fem_fnlcp_time <- occu(~dist +        nest + effort + time ~1, data = input_female_fnlcp) #error
    p9_fem_fnlcp_time <- occu(~                              time ~1, data = input_female_fnlcp)
    
    modSel(fitList(p1_fem_fnlcp_time, p2_fem_fnlcp_time, p3_fem_fnlcp_time, p4_fem_fnlcp_time, 
                   p6_fem_fnlcp_time, p9_fem_fnlcp_time))
    #top mod is p3; next is p4 but deltaAIC is 48        
      
    
  #MALE FNLC detections (constant psi) - time-varying p
    p1_mal_fnlcp_time <- occu(~1                                  ~1, data = input_male_fnlcp)
    p2_mal_fnlcp_time <- occu(~dist                        + time ~1, data = input_male_fnlcp)
    p3_mal_fnlcp_time <- occu(~dist + site                 + time ~1, data = input_male_fnlcp) #top model
    p4_mal_fnlcp_time <- occu(~dist +        nest          + time ~1, data = input_male_fnlcp) 
    p5_mal_fnlcp_time <- occu(~dist +               effort + time ~1, data = input_male_fnlcp) #error
    p6_mal_fnlcp_time <- occu(~                     effort + time ~1, data = input_male_fnlcp) #error
    p7_mal_fnlcp_time <- occu(~dist + site +        effort + time ~1, data = input_male_fnlcp) #error
    p8_mal_fnlcp_time <- occu(~dist +        nest + effort + time ~1, data = input_male_fnlcp) #error
    p9_mal_fnlcp_time <- occu(~                              time ~1, data = input_male_fnlcp)
    
    modSel(fitList(p1_mal_fnlcp_time, p2_mal_fnlcp_time, p3_mal_fnlcp_time, p4_mal_fnlcp_time, 
                   p9_mal_fnlcp_time))
    #top mod is p3; next is p2 but deltaAIC is 62       
    
    
   #Examine results [p(DIST + SITE + TIME) psi(.)]
    p3_any_time
    p3_fem_fnlcp_time
    p3_mal_fnlcp_time
    
    #Actually do without site first for simplicity [p(DIST + TIME) psi(.)]
    p2_any_time
    p2_fem_fnlcp_time
    p2_mal_fnlcp_time
    
    #Format covariates
    new_covar_time <- expand.grid('dist' = dist_grid, 'time' = as.character(unique(time)))
                                  # 'site' = site_grid)
    
    #Predict
    det_pred_p2any_time <- predict(p2_any_time, type = 'det', new_covar_time)
      det_pred_p2any_time_df <- cbind(new_covar_time, det_pred_p2any_time)
      head(det_pred_p2any_time_df)
     
    det_pred_p2fem_time <- predict(p2_fem_fnlcp_time, type = 'det', new_covar_time)
      det_pred_p2fem_time_df <- cbind(new_covar_time, det_pred_p2fem_time)
      head(det_pred_p2fem_time_df)
    
    det_pred_p2mal_time <- predict(p2_mal_fnlcp_time, type = 'det', new_covar_time)
      det_pred_p2mal_time_df <- cbind(new_covar_time, det_pred_p2mal_time)
      head(det_pred_p2mal_time_df)
      
    p_by_week_any <- det_pred_p2any_time_df %>% group_by(time) %>%
      summarise(p_weekly_mean = mean(Predicted),
                p_weekly_lci  = mean(lower),
                p_weekly_uci  = mean(upper),
                .groups = 'drop')
    
    p_by_week_fem <- det_pred_p2fem_time_df %>% group_by(time) %>%
      summarise(p_weekly_mean = mean(Predicted),
                p_weekly_lci  = mean(lower),
                p_weekly_uci  = mean(upper),
                .groups = 'drop')
    
    p_by_week_mal <- det_pred_p2mal_time_df %>% group_by(time) %>%
      summarise(p_weekly_mean = mean(Predicted),
                p_weekly_lci  = mean(lower),
                p_weekly_uci  = mean(upper),
                .groups = 'drop')
    
    #Any
    ggplot(p_by_week_any, aes(x = time, y = p_weekly_mean)) +
      geom_point(data = det_pred_p2any_time_df, aes(x = time, y = Predicted), 
                 color = 'lightgray', size = 0.5) +
      geom_pointrange(aes(ymin = p_weekly_lci, ymax = p_weekly_uci)) +
      xlab('Week') + ylab('Detection probability') +
      ggtitle('Detection probability (Any STOC)') +
      theme_bw()
    
    ggplot(det_pred_p2any_time_df, aes(x = time, y = Predicted)) +
      geom_boxplot(outlier.colour = 'black', outlier.shape = 16,
                   outlier.size = 2, notch = TRUE) +
      xlab('Week') + ylab('Detection probability') +
      ggtitle('Detection probability (Any STOC)') +
      theme_bw()

    #Female
    ggplot(p_by_week_fem, aes(x = time, y = p_weekly_mean)) +
      geom_point(data = det_pred_p2fem_time_df, aes(x = time, y = Predicted), 
                 color = 'lightgray', size = 0.5) +
      geom_pointrange(aes(ymin = p_weekly_lci, ymax = p_weekly_uci)) +
      xlab('Week') + ylab('Detection probability') +
      ggtitle('Detection probability (Female STOC)') +
      theme_bw()
    
    ggplot(det_pred_p2fem_time_df, aes(x = time, y = Predicted)) +
      geom_boxplot(outlier.colour = 'black', outlier.shape = 16,
                   outlier.size = 2, notch = TRUE) +
      xlab('Week') + ylab('Detection probability') +
      ggtitle('Detection probability (Female STOC)') +
      theme_bw()
    
    #Male
    ggplot(p_by_week_mal, aes(x = time, y = p_weekly_mean)) +
      geom_point(data = det_pred_p2mal_time_df, aes(x = time, y = Predicted), 
                 color = 'lightgray', size = 0.5) +
      geom_pointrange(aes(ymin = p_weekly_lci, ymax = p_weekly_uci)) +
      xlab('Week') + ylab('Detection probability') +
      ggtitle('Detection probability (Male STOC)') +
      theme_bw()
    
    ggplot(det_pred_p2mal_time_df, aes(x = time, y = Predicted)) +
      geom_boxplot(outlier.colour = 'black', outlier.shape = 16,
                   outlier.size = 2, notch = TRUE) +
      xlab('Week') + ylab('Detection probability') +
      ggtitle('Detection probability (Male STOC)') +
      theme_bw()
    
    
        
    #Plot
    plot(dist_grid, 
         det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_1",]$Predicted,
         type = "l", 
         lwd = 3, 
         xlab = "distance", 
         ylab = "estimated probability of detection",
         ylim = c(0,1))
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_2",]$Predicted,
          lwd = 3,
          col = "blue")
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_3",]$Predicted,
          lwd = 3,
          col = "pink")
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_4",]$Predicted,
          lwd = 3,
          col = "green")
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_5",]$Predicted,
          lwd = 3,
          col = "red")
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_6",]$Predicted,
          lwd = 3,
          col = "purple")
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_7",]$Predicted,
          lwd = 3,
          col = "gray")
    lines(dist_grid, 
          det_pred_p2any_time_df[det_pred_p2any_time_df$time == "visit_8",]$Predicted,
          lwd = 3,
          col = "brown")

    
    
    
    
    
    
    
################################################################################    
    
    
  #test 'p' covars....
    # p_mod_4 <- occu(~noise + effort + dist + nest ~1, data = input_any)
    # p_mod_3a <- occu(~noise + effort + dist ~1, data = input_any)
    # p_mod_3b <- occu(~noise + effort + nest ~1, data = input_any)
    # p_mod_3c <- occu(~noise + dist + nest ~1, data = input_any)
    # p_mod_3d <- occu(~effort + dist + nest ~1, data = input_any)
    # p_mod_2a <- occu(~noise + dist ~1, data = input_any)
    # p_mod_2b <- occu(~noise + nest ~1, data = input_any)
    # p_mod_2c <- occu(~effort + dist ~1, data = input_any)
    # p_mod_2d <- occu(~effort + nest ~1, data = input_any)
    # p_mod_2e <- occu(~dist + nest ~1, data = input_any)
    # p_mod_2f <- occu(~noise + effort ~1, data = input_any)
    # p_mod_1a <- occu(~noise ~1, data = input_any)
    # p_mod_1b <- occu(~effort ~1, data = input_any)
    # p_mod_1c <- occu(~dist ~1, data = input_any) 
    # p_mod_1d <- occu(~nest ~1, data = input_any) 
    # pmod_0   <- occu(~1 ~1, data = input_any)
    # pmod_1_site <- occu(~site ~1, data = input_any)
    
    #compare
    modSel(fitList(p_mod_4, p_mod_3a, p_mod_3b, p_mod_3c, p_mod_3d,
                   p_mod_2a, p_mod_2b, p_mod_2c, p_mod_2d, p_mod_2e, p_mod_2f, 
                   p_mod_1a, p_mod_1b, p_mod_1c, p_mod_1d, pmod_1_site, pmod_0))
      
      #mods within 2 delta AIC are:
        #p(noise + effort + dist)        psi(.)
        #p(noise + effort + dist + nest) psi (.)
        #... next highest are delta AIC = 34
        #null model is delta AIC = 181
  

    
  #constant parameters ---------------------------------------------------------
    mod1_any <- occu(~1 ~1, data = input_any)
    mod1_fem <- occu(~1 ~1, data = input_female)
    mod1_mal <- occu(~1 ~1, data = input_male)
    
      #save
      saveRDS(mod1_any, 'output/11_occ_models/2023-05-22/mod1_any.rds')
      saveRDS(mod1_fem, 'output/11_occ_models/2023-05-22/mod1_fem.rds')
      saveRDS(mod1_mal, 'output/11_occ_models/2023-05-22/mod1_mal.rds')
      
      #inspect results
      mod1_any; mod1_fem; mod1_mal
      
      #a couple ways to back-transform
      coef(mod1_any)
      (psi_mod1_any <- plogis(coef(mod1_any)[1]))
      (p_mod1_any   <- plogis(coef(mod1_any)[2]))
      
      backTransform(mod1_any, type = 'state') #occupancy with SE (0.349, 0.0282)
      backTransform(mod1_any, type = 'det')   #detection with SE (0.294, 0.0112)
      
      confint(backTransform(mod1_any, type = 'state')) #95% CI for estimates above
      confint(backTransform(mod1_any, type = 'det'))
      
      
  #p(distance) psi(distance) ---------------------------------------------------
    mod2_any <- occu(~dist_intended_std ~dist_intended_std, data = input_any)
    mod2_fem <- occu(~dist_intended_std ~dist_intended_std, data = input_female)
    mod2_mal <- occu(~dist_intended_std ~dist_intended_std, data = input_male)
      
      saveRDS(mod2_any, 'output/11_occ_models/2023-05-22/mod2_any.rds')    
      saveRDS(mod2_fem, 'output/11_occ_models/2023-05-22/mod2_fem.rds')    
      saveRDS(mod2_mal, 'output/11_occ_models/2023-05-22/mod2_mal.rds')    
      
      
        
  #p(duration) psi(duration) ---------------------------------------------------
    mod21_effort <- occu(~duration_std ~duration_std, data = input21)
      
      mod21_effort
      # backTransform(mod21_effort, type = 'state') #hmm can't do this directly now
      # backTransform(mod21_effort, type = 'det')
      
      ## plot
      #first create a grid of covariate values to predict across (these are still scaled)
      effort_grid <- seq(min(site_covars21$duration_std), max(site_covars21$duration_std), length = 50)
      
      #now predict using the parameter estimates. equation is "intercept + slope*value"
      det_pred <- plogis(coef(mod21_effort)[3] + coef(mod21_effort)[4] * effort_grid)
      occ_pred <- plogis(coef(mod21_effort)[1] + coef(mod21_effort)[2] * effort_grid)
      
      #or just use function 'predict'
      duration_std <- effort_grid #needs to have same name as in model
      det_pred_2 <- predict(mod21_effort, type = 'det', as.data.frame(duration_std))
      occ_pred_2 <- predict(mod21_effort, type = 'state', as.data.frame(duration_std))
      
      #convert covariate values back to their real scale
      effort_bt <- (effort_grid * sd(site_covars21$duration)) + mean(site_covars21$duration)
      
      #plot
      par(mfrow = c(1,2))
      plot(effort_bt, det_pred, type = 'l', lwd = 3, ylim = c(0,1),
           xlab = 'effort', ylab = 'estimated detection prob')
      plot(effort_bt, occ_pred, type = 'l', lwd = 3, ylim = c(0,1),
           xlab = 'effort', ylab = 'estimated occupancy prob')
      
      #ggplot
      plot_data <- data.frame('effort' = rep(effort_bt,2),
                              'parameter' = c(rep('occ',length(occ_pred)), rep('det',length(det_pred))),
                              'value' = c(occ_pred_2$Predicted, det_pred_2$Predicted),
                              'LCI' = c(occ_pred_2$lower, det_pred_2$lower),
                              'UCI' = c(occ_pred_2$upper, det_pred_2$upper))
      ggplot(plot_data, aes(x = effort, y = value)) +
        geom_line() +
        geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
        ylim(0,1) +
        geom_rug(data = site_covars21, aes(x = duration), inherit.aes = FALSE) +
        facet_grid(~parameter) + theme_bw()
      
        ## effort is NOT significant on either parameter... move on without it? or still include on 'p'
      
      
  #p(distance) psi(distance) ---------------------------------------------------
    mod21_distance <- occu(~dist_actual_std ~dist_actual_std, data = input21)
      
      mod21_distance #looks to be significant on both
      
    ## plot
      #first create a grid of covariate values to predict across (these are still scaled)
      dist_grid <- seq(min(site_covars21$dist_actual_std), max(site_covars21$dist_actual_std), length = 50)
      
      #predict
      dist_actual_std <- dist_grid #needs to have same name as in model
      det_pred <- predict(mod21_distance, type = 'det', as.data.frame(dist_actual_std))
      occ_pred <- predict(mod21_distance, type = 'state', as.data.frame(dist_actual_std))
      
      #convert covariate values back to their real scale
      dist_bt <- (dist_grid * sd(site_covars21$dist_actual)) + mean(site_covars21$dist_actual)
      
      #ggplot
      plot_data <- data.frame('distance' = rep(dist_bt,2),
                              'parameter' = c(rep('occ',nrow(occ_pred)), rep('det',nrow(det_pred))),
                              'value' = c(occ_pred$Predicted, det_pred$Predicted),
                              'LCI' = c(occ_pred$lower, det_pred$lower),
                              'UCI' = c(occ_pred$upper, det_pred$upper))
      ggplot(plot_data, aes(x = distance, y = value)) +
        geom_line() +
        geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
        ylim(0,1) +
        ylab('Predicted probability (\u00B1 95 CI)') +
        xlab('Distance (m)') +
        ggtitle('p(distance) psi(distance)') +
        geom_rug(data = site_covars21, aes(x = dist_intended), inherit.aes = FALSE) +
        facet_grid(~parameter) + theme_bw()
      
      ## strong negative relationship with increasing distance and both p and psi
     
      
   #p(distance) psi(distance + site) -------------------------------------------
   #p(distance + site) psi(distance + site)
    mod21_distance_site <- occu(~dist_actual_std ~dist_actual_std + site_name, data = input21)
    mod21_distance_site_both <- occu(~dist_actual_std + site_name ~dist_actual_std + site_name, data = input21)
    
      mod21_distance_site #sig diff between UG and DC (reference)
      mod21_distance_site_both
      
      ## plot
      #first create a grid of covariate values to predict across (these are still scaled)
      dist_grid <- seq(min(site_covars21$dist_actual_std), max(site_covars21$dist_actual_std), length = 50)
      
      #predict
      dist_actual_std <- dist_grid #needs to have same name as in model
      new_covar <- data.frame('dist_actual_std' = rep(dist_grid, each = 4),
                    'site_name' = rep(unique(site_covars21$site_name), length(dist_grid)))
      
      det_pred <- predict(mod21_distance_site, type = 'det', new_covar)
      occ_pred <- predict(mod21_distance_site, type = 'state', new_covar)
      
      det_pred_2 <- predict(mod21_distance_site_both, type = 'det', new_covar)
      occ_pred_2 <- predict(mod21_distance_site_both, type = 'state', new_covar)
      
      #convert covariate values back to their real scale
      dist_bt <- (dist_grid * sd(site_covars21$dist_actual)) + mean(site_covars21$dist_actual)
      
      #format for plotting
      pred_wide <- cbind(new_covar, 'det' = det_pred$Predicted, 'occ' = occ_pred$Predicted)
        pred_long <- melt(pred_wide, id.vars = c('dist_actual_std','site_name'), value.name = 'value')
      lci_wide <- cbind(new_covar, 'det' = det_pred$lower, 'occ' = occ_pred$lower)
        lci_long <- melt(lci_wide, id.vars = c('dist_actual_std','site_name'), value.name = 'LCI')
      uci_wide <- cbind(new_covar, 'det' = det_pred$upper, 'occ' = occ_pred$upper)
        uci_long <- melt(uci_wide, id.vars = c('dist_actual_std','site_name'), value.name = 'UCI')        
      
      plot_data <- cbind(pred_long, 'LCI' = lci_long$LCI, 'UCI' = uci_long$UCI)
      plot_data$dist_actual_bt <- (plot_data$dist_actual_std * sd(site_covars21$dist_actual)) + mean(site_covars21$dist_actual)
      
      pred_wide2 <- cbind(new_covar, 'det' = det_pred_2$Predicted, 'occ' = occ_pred_2$Predicted)
        pred_long2 <- melt(pred_wide2, id.vars = c('dist_actual_std','site_name'), value.name = 'value')
      lci_wide2 <- cbind(new_covar, 'det' = det_pred_2$lower, 'occ' = occ_pred_2$lower)
        lci_long2 <- melt(lci_wide2, id.vars = c('dist_actual_std','site_name'), value.name = 'LCI')
      uci_wide2 <- cbind(new_covar, 'det' = det_pred_2$upper, 'occ' = occ_pred_2$upper)
        uci_long2 <- melt(uci_wide2, id.vars = c('dist_actual_std','site_name'), value.name = 'UCI')        
      
      plot_data_2 <- cbind(pred_long2, 'LCI' = lci_long2$LCI, 'UCI' = uci_long2$UCI)
      plot_data_2$dist_actual_bt <- (plot_data_2$dist_actual_std * sd(site_covars21$dist_actual)) + mean(site_covars21$dist_actual)
      
    #Plot    
      #same 'p' for all sites  
      ggplot(plot_data, aes(x = dist_actual_bt/1000, y = value, color = variable)) +
        geom_line() +
        geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
        ylim(0,1) +
        ylab('Predicted probability (\u00B1 95 CI)') +
        xlab('Distance (km)') +
        ggtitle('p(distance) psi(distance + site)') +
        # geom_rug(data = site_covars21, aes(x = dist_intended), inherit.aes = FALSE) +
        facet_grid(~ site_name) + theme_bw()
      
      #varying 'p' by site
      ggplot(plot_data_2, aes(x = dist_actual_bt/1000, y = value, color = variable)) +
        geom_line() +
        geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
        ylim(0,1) +
        ylab('Predicted probability (\u00B1 95 CI)') +
        xlab('Distance (km)') +
        ggtitle('p(distance + site) psi(distance + site)') +
        # geom_rug(data = site_covars21, aes(x = dist_intended), inherit.aes = FALSE) +
        facet_grid(~ site_name) + theme_bw()
      
      
  #p(distance) psi(distance + nesting) -----------------------------------------
  #p(distance + nesting) psi(distance + nesting)
    mod21_distance_nest <- occu(~dist_actual_std ~dist_actual_std + nested, data = input21)
    mod21_distance_nest_both <- occu(~dist_actual_std + nested ~dist_actual_std + nested, data = input21)
      
    mod21_distance_nest      #looks like no effect of 'nested' on occupancy
    mod21_distance_nest_both #but positive effect of 'nested' on detection
      
      #predict
      new_covar <- data.frame('dist_actual_std' = rep(dist_grid, each = 4),
                              'nested' = rep(unique(site_covars21$nested), length(dist_grid)))
      
      det_pred <- predict(mod21_distance_nest, type = 'det', new_covar)
      occ_pred <- predict(mod21_distance_nest, type = 'state', new_covar)
      
      det_pred_2 <- predict(mod21_distance_nest_both, type = 'det', new_covar)
      occ_pred_2 <- predict(mod21_distance_nest_both, type = 'state', new_covar)
      
      #format for plotting
      pred_wide <- cbind(new_covar, 'det' = det_pred$Predicted, 'occ' = occ_pred$Predicted)
        pred_long <- melt(pred_wide, id.vars = c('dist_actual_std','nested'), value.name = 'value')
      lci_wide <- cbind(new_covar, 'det' = det_pred$lower, 'occ' = occ_pred$lower)
        lci_long <- melt(lci_wide, id.vars = c('dist_actual_std','nested'), value.name = 'LCI')
      uci_wide <- cbind(new_covar, 'det' = det_pred$upper, 'occ' = occ_pred$upper)
        uci_long <- melt(uci_wide, id.vars = c('dist_actual_std','nested'), value.name = 'UCI')        
      
      plot_data <- cbind(pred_long, 'LCI' = lci_long$LCI, 'UCI' = uci_long$UCI)
      plot_data$dist_actual_bt <- (plot_data$dist_actual_std * sd(site_covars21$dist_actual)) + mean(site_covars21$dist_actual)
      
      pred_wide2 <- cbind(new_covar, 'det' = det_pred_2$Predicted, 'occ' = occ_pred_2$Predicted)
        pred_long2 <- melt(pred_wide2, id.vars = c('dist_actual_std','nested'), value.name = 'value')
      lci_wide2 <- cbind(new_covar, 'det' = det_pred_2$lower, 'occ' = occ_pred_2$lower)
        lci_long2 <- melt(lci_wide2, id.vars = c('dist_actual_std','nested'), value.name = 'LCI')
      uci_wide2 <- cbind(new_covar, 'det' = det_pred_2$upper, 'occ' = occ_pred_2$upper)
        uci_long2 <- melt(uci_wide2, id.vars = c('dist_actual_std','nested'), value.name = 'UCI')        
      
      plot_data_2 <- cbind(pred_long2, 'LCI' = lci_long2$LCI, 'UCI' = uci_long2$UCI)
      plot_data_2$dist_actual_bt <- (plot_data_2$dist_actual_std * sd(site_covars21$dist_actual)) + mean(site_covars21$dist_actual)
      
      #Plot    
      #same 'p' for nesting and non-nesting 
      ggplot(plot_data, aes(x = dist_actual_bt/1000, y = value, color = variable)) +
        geom_line() +
        geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
        ylim(0,1) +
        ylab('Predicted probability (\u00B1 95 CI)') +
        xlab('Distance (km)') +
        ggtitle('p(distance) psi(distance + nested)') +
        # geom_rug(data = site_covars21, aes(x = dist_intended), inherit.aes = FALSE) +
        facet_grid(~ nested) + theme_bw()
      
      #varying 'p' by site
      ggplot(plot_data_2, aes(x = dist_actual_bt/1000, y = value, color = variable)) +
        geom_line() +
        geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3) +
        ylim(0,1) +
        ylab('Predicted probability (\u00B1 95 CI)') +
        xlab('Distance (km)') +
        ggtitle('p(distance + nested) psi(distance + nested)') +
        # geom_rug(data = site_covars21, aes(x = dist_intended), inherit.aes = FALSE) +
        facet_grid(~ nested) + theme_bw()      
 
#####   
  #Compare models
    modList <- fitList('{psi, p}' = mod21_dot,
                       '{psi(effort), p(effort)' = mod21_effort,
                       '{psi(dist), p(dist)' = mod21_distance,
                       '{psi(dist + site), p(dist)' = mod21_distance_site,
                       '{psi(dist + site), p(dist + site)' = mod21_distance_site_both,
                       '{psi(dist + nest), p(dist)' = mod21_distance_nest,
                       '{psi(dist + nest), p(dist + nest)' = mod21_distance_nest_both) 
    modSel(modList)
    #or just do this:
      modSel(fitList(mod21_dot, mod21_effort, mod21_distance, mod21_distance_site, 
                     mod21_distance_site_both, mod21_distance_nest, mod21_distance_nest_both))
      
    #looks like the effect of site is important
    
      
      
######      
      
    #site effect
    siteMod21 <- occu(formula = ~1 ~site_name, data = input21)
      siteMod21
      confint(siteMod21, type = 'det', method = 'normal')# what's the difference between 'normal' and 'profile'?
      confint(siteMod21, type = 'state', method = 'normal')
      
      # # estimate detection effect at obsvars=0.5
      # (lc <- linearComb(siteMod21['det'], c(1,0.5)))
      # 
      # linearComb(siteMod21['occ'], c('site_nameWC'))
      # 
      # # transform this to probability (0 to 1) scale and get confidence limits
      # (btlc <- backTransform(lc))
      # confint(btlc, level = 0.9)
      
      # Empirical Bayes estimates of proportion of sites occupied
      re <- ranef(siteMod21)
      sum(bup(re, stat="mode"))
        #compare with naive occupancy
        table(rowSums(dh_any_21, na.rm = T)>0)
          #naive occupancy 45/130... same as predicted
      
## Save model results ----------------------------------------------------------    
    modResults[nrow(modResults)+1,] <- c(strsplit(dotModel2$modname, '_')[[1]][2], 'any_2022', 'staggered', 
                                         dotModel2$real$psi$est[1], dotModel2$real$psi$se[1],
                                         dotModel2$real$p$est[1], dotModel2$real$p$se[1])
    modResults
    
    
    
## Figures ---------------------------------------------------------------------
    
  modResults$cov <- as.character(modResults$cov)
    
  modResults$name <- paste(modResults$type, modResults$cov, sep = '_')
    
  psi_plot <- ggplot(data = modResults, aes(x = as.factor(name), y = as.numeric(psi_est), 
                                            label=sprintf("%0.2f", round(as.numeric(psi_est), digits = 2)))) + 
      geom_pointrange(aes(x = as.factor(name), y = as.numeric(psi_est), ymin = (as.numeric(psi_est) - as.numeric (psi_se)),
                          ymax = (as.numeric(psi_est) + as.numeric (psi_se)), color = name), size = 1.5) +
      ylab('Occupancy (psi) \u00B1 SE') +
      # scale_y_continuous(breaks = seq(0, 1, by = 0.2), lim = c(0, 1.09)) +
      # scale_color_manual(values = c('#0084A8', '#FF5000')) +
      theme(panel.background = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.grid = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            axis.line.x = element_line(),
            axis.line.y = element_line(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text = element_text(size = 24, color = '#000000'),
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.position = 'none')
    psi_plot
    

    p_plot <- ggplot(data = modResults, aes(x = as.factor(name), y = as.numeric(p_est), 
                                            label=sprintf("%0.2f", round(as.numeric(p_est), digits = 2)))) +
      geom_pointrange(aes(x = as.factor(name), y = as.numeric(p_est), ymin = (as.numeric(p_est) - as.numeric (p_se)),
                          ymax = (as.numeric(p_est) + as.numeric (p_se)), color = name), size = 1.5, shape = 17) +
      ylab('Weekly detection probability (p) \u00B1 SE') +
      #labs(title = paste(speciesName, sep = '')) +
      # scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), lim = c(0, 0.4)) +
      # scale_color_manual(values = c('#000000','#0084A8', '#FF5000')) +
      # scale_color_manual(values = c('#0084A8', '#FF5000')) +
      theme(panel.background = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.grid = element_blank(),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            axis.line.x = element_line(),
            axis.line.y = element_line(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.text = element_text(size = 24, color = '#000000'),
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.position = 'none')
    p_plot
    
    
    