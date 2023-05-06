## exploring detections by date and call type

library(data.table)
library(ggplot2)



## load data ####
ac21_agg <- fread('output/06_ac_21_by_station_night.csv')
ac21_agg$siteID <- sapply(strsplit(as.character(ac21_agg$SITESTN), '\\_'), '[', 1)
ac21_agg$siteID <- factor(ac21_agg$siteID, levels = c('WC','MC','DC','UG'))

  #format NIGHT field
  ac21_agg$NIGHT <- as.POSIXct(strptime(ac21_agg$NIGHT, '%Y-%m-%d'), tz = 'America/Los_Angeles')

  #add site nicknames for presentations
  ac21_agg$nname <- ifelse(ac21_agg$siteID %in% 'WC', 'P1',
                            ifelse(ac21_agg$siteID %in% 'MC', 'F2',
                                   ifelse(ac21_agg$siteID %in% 'DC','F1','N1')))
  ac21_agg$nname <- factor(ac21_agg$nname, levels = c('P1','N1','F1','F2'))


## Look at number of calls throughout the season ####
  
  ggplot(ac21_agg, aes(x = NIGHT)) +
    # geom_point(aes(y = STOC_ANY_N, color = 'Any'), size = 2, alpha = 0.8) +
    geom_point(aes(y = MALE_N, color = 'Male'), size = 2, alpha = 0.8) +
    geom_point(aes(y = FEMALE_N, color = 'Female'), size = 2, alpha = 0.8) +
    facet_wrap(vars(nname), nrow = 2) +
    labs(y = 'number of detections', color = 'Sex') +
    scale_color_manual(values = c('Female' = 'tomato','Male' = 'dodgerblue1')) +
                                    # ,'Any' = 'gray')) +
    theme(panel.background = element_rect(fill = 'transparent'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          # legend.position = 'none',
          # legend.background = element_rect(fill='transparent'))
    )


  ## Need to correct this for effort, though.
  ## Maybe do a rolling window (1-week?) for proportion of nights with detections?
  ## Or the proportion of units with detections in a given week?

  
## Proportion of units with detections each week (corrected for effort) ####
  
ac21_fem_dh <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocFemale.csv')
ac21_male_dh <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocMale.csv')

  #format 'site' column
  ac21_fem_dh$site <- sapply(strsplit(as.character(ac21_fem_dh$SITESTN), '\\_'), '[', 1)
  ac21_male_dh$site <- sapply(strsplit(as.character(ac21_male_dh$SITESTN), '\\_'), '[', 1)

  #format 'sex' column
  ac21_fem_dh$sex <- 'female'
  ac21_male_dh$sex <- 'male'

  ## If we need the *number* of detections per week...
    #convert back to long
    ac21_fem_long <- melt(ac21_fem_dh[,c(4:23,25:26)], id.vars = c('site','sex'))
    ac21_fem_long

    ac21_male_long <- melt(ac21_male_dh[,c(4:23,25:26)], id.vars = c('site','sex'))
    ac21_male_long

    #combine and plot
    ac21_long <- rbind(ac21_fem_long, ac21_male_long)
    colnames(ac21_long) <- c('site','sex','week','n')

    ggplot(data = ac21_long, aes(x = week, y = n)) +
      geom_point() +
      facet_grid(~sex)
    
  #get the number of units with detections and number of sites surveyed each week/site
  sitesFemale <- NULL  
  for(ss in unique(ac21_fem_dh$site)){
    nSites <- colSums(ac21_fem_dh[,c(4:23)][ac21_fem_dh$site %in% ss,] > 0, na.rm = TRUE) #number of units with detections in each week
    nSurv  <- colSums(!is.na(ac21_fem_dh[,c(4:23)][ac21_fem_dh$site %in% ss,]), na.rm = TRUE) #number of units that were surveyed each week (i.e., not NA)
    ssDF <- data.frame('site' = ss, 'week' = seq(1,20), 'nSitesDet' = nSites, 'nSitesSurv' = nSurv)  
    sitesFemale <- rbind(sitesFemale, ssDF)  
  }
  sitesFemale$sex <- 'Female'

  sitesMale <- NULL  
  for(ss in unique(ac21_male_dh$site)){
    nSites <- colSums(ac21_male_dh[,c(4:23)][ac21_male_dh$site %in% ss,] > 0, na.rm = TRUE) #number of sites with detections in each week
    nSurv  <- colSums(!is.na(ac21_male_dh[,c(4:23)][ac21_male_dh$site %in% ss,]), na.rm = TRUE) #number of sites that were surveyed each week (i.e., not NA)
    ssDF <- data.frame('site' = ss, 'week' = seq(1,20), 'nSitesDet' = nSites, 'nSitesSurv' = nSurv)  
    sitesMale <- rbind(sitesMale, ssDF)  
  }
  sitesMale$sex <- 'Male'

  #combine 
  sitesDet <- rbind(sitesFemale, sitesMale)

  #plot (proportion)
  ggplot(sitesDet, aes(x = week, y = nSitesDet / nSitesSurv, color = sex)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    # facet_grid(~site+sex) +
    facet_grid(rows = vars(sex), cols = vars(site)) +
    ylab('Proportion of stations with detection') + 
    theme_bw()
  
  #plot (raw number with effort line)
  ggplot(sitesDet, aes(x = week, y = nSitesDet, color = sex)) +
    geom_point(size = 2) +
    geom_line() +
    geom_line(aes(y = nSitesSurv), color = 'black') +
    facet_grid(rows = vars(sex), cols = vars(site)) +
    ylab('Proportion of stations with detection') + 
    scale_y_continuous(name = 'sites with detections', #first axis
                       sec.axis = sec_axis(~., name='sites surveyed')) + #second axis
    theme_bw()
  
  
  ## OK, beyond just removing the sites at the end after units were picked up, we may not need to 
  ## correct for effort too much ... it's mostly constant throughout the season until it goes to 0

  ## There's not much of a seasonal trend in the *number* of units that get detections throughout the season
  ## But what about the number of calls (somehow corrected for effort? or not important?)
    ## or the proportion of nights with calls
  
  
## Proportion of nights with detections each week (corrected for effort) ####

  #need long dh
  dh21_long <- fread('output/07_ac_21_dethist_long.csv')
  dh21_long
  
  #add 'site' field
  dh21_long$site <- sapply(strsplit(as.character(dh21_long$SITESTN), '\\_'), '[', 1)
  
  #calculate proportion of nights with detection on a weekly basis (fixed weeks)
  dfWWSS <- NULL
  for (ss in unique(dh21_long$site)){
    dhSS <- dh21_long[dh21_long$site %in% ss,]
    
    for (ww in unique(dh21_long$week)){
      dhWW <- dhSS[dhSS$week %in% ww,]
      nightsFdet <- nrow(dhWW[dhWW$FEMALE_N > 0])
      nightsMdet <- nrow(dhWW[dhWW$MALE_N > 0])
      nightsANYdet <- nrow(dhWW[dhWW$STOC_ANY_N > 0])
      nightsSurv <- nrow(dhWW)
      dfWW <- data.frame('site' = ss, 'week' = ww, 'nightsSurv' = nightsSurv,
                         'nightsF' = nightsFdet, 'nightsM' = nightsMdet, 'nightsANY' = nightsANYdet)
      dfWWSS <- rbind(dfWWSS, dfWW)
    }
  }
  dfWWSS <- dfWWSS[dfWWSS$week != '-',]
  
  #plot
  ggplot(dfWWSS, aes(x = week)) +
    geom_point(aes(y = nightsF / nightsSurv), color = 'tomato') +
    geom_point(aes(y = nightsM / nightsSurv), color = 'cyan4') +
    # geom_smooth(aes(y = nightsF / nightsSurv), color = 'tomato') +
    # geom_line() +
    facet_grid(~site) +
    theme_bw()
  
  
## Proportion of nights with detections each week -- rolling window (corrected for effort) ####  
  
  #calculate proportion of nights with detection on a weekly basis (fixed weeks)
  dfRolling <- NULL
  for (tt in unique(dh21_long$SITESTN)){
    dhTT <- dh21_long[dh21_long$SITESTN %in% tt,]
    dhTT <- dhTT[dhTT$surv == 1,]
    # dhTT[,c('site','SITESTN','date','duration','surv','week','MALE_N','FEMALE_N')]
    
    propF <- runner(x = dhTT, k = 7, f = function(x) {x$FEMALE_N[1] / length(x)})
   
    #deal with NAs...
    
  }
  dfWWSS <- dfWWSS[dfWWSS$week != '-',]
  
  
  #rolling window
  for (ss in unique(dh21_long$site)){
    dh21_long[dh21_long$site %in% ss,]
    
    prop <- runner(
      x = df,
      k = 7,
      f = function(x) {
        x$a[1] / sum(x$b)
      }
    )
    
  }
  