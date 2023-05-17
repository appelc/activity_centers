## Occupancy models with activity center ("big grid") data

library(data.table)
library(unmarked)
library(ggplot2)
library(reshape2)


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


## Load and format detection histories -----------------------------------------
  # dh_any_21_left <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocAny_left.csv'); dh_any_21_left <- dh_any_21_left[,-'V1']
  dh_any_21_stag <- fread('output/08_weekly_dethist_staggered/08_dh_ac_2021_stocAny_staggered.csv')
  dh_any_22_stag <- fread('output/08_weekly_dethist_staggered/08_dh_ac_2022_stocAny_staggered.csv')
  
  ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
  # dh_any_21 <- dh_any_21_left[order(dh_any_21_left$SITESTN),]; dh_any_21 <- data.frame(dh_any_21)
  dh_any_21 <- dh_any_21_stag[order(dh_any_21_stag$SITESTN),]; dh_any_21 <- data.frame(dh_any_21)
  rownames(dh_any_21) <- dh_any_21$SITESTN; dh_any_21 <- dh_any_21[,-c(1:3, 24:25)]
  #no diff btwn left and staggered for 'any STOC' dot models, so just pick one or do some more checking?
  
  dh_any_22 <- dh_any_22_stag[order(dh_any_22_stag$SITE_STN),]; dh_any_22 <- data.frame(dh_any_22)
  rownames(dh_any_22) <- dh_any_22$SITE_STN; dh_any_22 <- dh_any_22[,-c(1:3, 25:26)]
  
  ## Convert to binary
  dh_any_21[dh_any_21 > 0] <- 1
  dh_any_22[dh_any_22 > 0] <- 1
  
  
## Load covariates -------------------------------------------------------------
  
  #site-level
  site_covars <- readRDS('output/10_occ_covariates/10_site_covariates.rds')
    site_covars21 <- site_covars$`2021`
    site_covars22 <- site_covars$`2022`
    
  #survey-level
  
  
## Create input files ----------------------------------------------------------    
  
  dim(dh_any_21)
  dim(dh_any_22)
  
  input21 <- unmarkedFrameOccu(y = dh_any_21, 
                               siteCovs = site_covars21[,c('duration_std','dist_actual_std','site_name','nested')])
    str(input21)
    plot(input21) #optionally panels = 4 e.g., to split up y-axis
    summary(input21) #this is useful; gives naive occupancy, mean number of obs per site, etc.
    
  input22 <- unmarkedFrameOccu(y = dh_any_22)
    str(input22)
  
    
## Run models ------------------------------------------------------------------
 
  #constant parameters ---------------------------------------------------------
    mod21_dot <- occu(~1 ~1, data = input21)
    
      #inspect results
      mod21_dot
      
      #a couple ways to back-transform
      coef(mod21_dot)
      (psi21_dot <- plogis(coef(mod21_dot)[1]))
      (p21_dot   <- plogis(coef(mod21_dot)[2]))
      
      backTransform(mod21_dot, type = 'state') #occupancy with SE (0.315, 0.039)
      backTransform(mod21_dot, type = 'det') #detection with SE (0.279, 0.0167)
      
      confint(backTransform(mod21_dot, type = 'state')) #95% CI for estimates above
      confint(backTransform(mod21_dot, type = 'det'))
        
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
    
    
    