---
title: "12_glms_combined"
author: "Cara"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_float: true
    toc_depth: 4
    keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r load-packages, include=FALSE}
library(data.table)
# library(broom)
library(tidyverse)
library(effects)
library(ggplot2)
library(lme4)
# library(plyr)
# library(kableExtra)
# library(MASS)
```


--------------------------------------------------------------------------------
#### Load data
```{r echo=FALSE}

  #import station data with covariates
    ac_data <- fread('output/10_occ_covariates/10_site_level_2021-2022.csv')
    ac_data$year_site <- paste(ac_data$year, ac_data$site_stn, sep = '_')

    #also need Barred owl and NR habitat covariates
  
    head(ac_data[,c('year_site','site_name','repro_state','nested','duration','dist_actual')])


  #import detection histories (using FLNC/pair)
    dh_any    <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocAny_left.csv')
    dh_female <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocFemaleFNLCorPair_left.csv')
    dh_male   <- fread('output/08_weekly_dethist_left/08_dh_ac_combined_stocMaleFNLCorPair_left.csv')
    
  #calculate # weeks surveyed
    dh_any$survWeeks    <- rowSums(!is.na(dh_any[,-1])) #only need to do once; they're the same
    
  #calculate # weeks with detections
    dh_any$detWeeks <- rowSums(dh_any[,c(2:22)], na.rm = TRUE)
    dh_female$detWeeks <- rowSums(dh_female[,c(2:22)], na.rm = TRUE)
    dh_male$detWeeks <- rowSums(dh_male[,c(2:22)], na.rm = TRUE)
    
  #merge
    ac_data$survWeeks   <- dh_any$survWeeks[match(ac_data$year_site, dh_any$V1)] #only do once
    ac_data$weeksAny    <- dh_any$detWeeks[match(ac_data$year_site, dh_any$V1)]
    ac_data$weeksFemale <- dh_female$detWeeks[match(ac_data$year_site, dh_female$V1)]
    ac_data$weeksMale   <- dh_male$detWeeks[match(ac_data$year_site, dh_male$V1)]

  #add site nicknames for presentations
    #do this later. need to come up with new names now that we're combining years

```

### -- Models using the proportion of survey nights with any spotted owl detection as the response variable

### 1. Quasibinomial: weeks with detections ~ distance

```{r echo = FALSE}

  mod0 <- glm(cbind(weeksAny, survWeeks - weeksAny) ~ dist_actual, 
              data = ac_data, family = binomial(link = 'logit'))
    summary(mod0)
      #check binomial. deviance >> degrees of freedom, so use quasibinomial
  
  mod1_any <- glm(cbind(weeksAny, survWeeks - weeksAny) ~ dist_actual, 
                  data = ac_data, family = quasibinomial(link = 'logit'))
    summary(mod1_any)
  
  mod1_female <- glm(cbind(weeksFemale, survWeeks - weeksFemale) ~ dist_actual, 
                  data = ac_data, family = quasibinomial(link = 'logit'))
    summary(mod1_female)
  
  mod1_male <- glm(cbind(weeksMale, survWeeks - weeksMale) ~ dist_actual, 
                  data = ac_data, family = quasibinomial(link = 'logit'))
    summary(mod1_male)
  
    #not great but to double check geom_smooth below **actually, why aren't these right?
    # tmp <- allEffects(mod1_any)
    # tmp$dist_actual$fit <- plogis(tmp$dist_actual$fit)
    # tmp$dist_actual$lower <- plogis(tmp$dist_actual$lower)
    # tmp$dist_actual$upper <- plogis(tmp$dist_actual$upper)
    # 
    # plot(tmp, ylim = c(0,1))
    # # plot(allEffects(mod1_any))
    # 
    # #not great but to double check geom_smooth below
    # tmpF <- allEffects(mod1_female)
    # tmpF$dist_actual$fit <- plogis(tmpF$dist_actual$fit)
    # tmpF$dist_actual$lower <- plogis(tmpF$dist_actual$lower)
    # tmpF$dist_actual$upper <- plogis(tmpF$dist_actual$upper)
    # 
    # plot(tmpF, ylim = c(0,1))
    # # plot(allEffects(mod1_any))
    
  ## Diagnose
    plot(mod1_any$fitted.values, residuals(mod1_any), pch = 19, las = 1, cex = 1.4)
    abline(0, 0, lwd = 1.5)
      ## check for no pattern
    
    plot(mod1_any$fitted.values, residuals(mod1_female), pch = 19, las = 1, cex = 1.4)
    abline(0, 0, lwd = 1.5)
    
    plot(mod1_any$fitted.values, residuals(mod1_male), pch = 19, las = 1, cex = 1.4)
    abline(0, 0, lwd = 1.5)
    
```

#### Plot:
  
```{r echo=FALSE}

  plot_mod1_any <- ggplot(ac_data, aes(x = dist_actual/1000, y = weeksAny/survWeeks,
                                    weeks = weeksAny, total = survWeeks)) +
      geom_point(data = ac_data[ac_data$weeksAny > 0,], #we don't want to plot 0s
                 aes(x = dist_actual/1000, y = weeksAny / survWeeks),
                 color = 'gray70', size = 1.5) +
      geom_smooth(formula = cbind(weeks, total-weeks) ~ x, 
                  method = 'glm', method.args = list(family = quasibinomial(link = 'logit')),
                  color = 'aquamarine4', fill = 'aquamarine3') +
      # ylim(0,0.65) +
      ylim(0,1) +
      ggtitle('All') +
      ylab('Proportion of weeks with detection') + xlab('Distance from center (km)') +
      # theme_bw() +
      theme(panel.background = element_rect(fill = 'transparent'),
            panel.grid = element_line(color = 'gray90'),
            axis.line = element_line(),
            axis.title = element_text(size = 18),
            # axis.title.x = element_blank(),
            strip.text = element_text(size = 16),
            axis.text = element_text(size = 16),
            plot.title = element_text(hjust = 0.5),
            legend.text = element_text(size = 16),
            legend.title = element_blank(),
            legend.position = 'none',
            legend.background = element_rect(fill='transparent'))
  
  plot_mod1_any
  
  plot_mod1_fem <- ggplot(ac_data, aes(x = dist_actual/1000, y = weeksFemale/survWeeks,
                                       weeks = weeksFemale, total = survWeeks)) +
    geom_point(data = ac_data[ac_data$weeksFemale > 0,], #we don't want to plot 0s
               aes(x = dist_actual/1000, y = weeksFemale / survWeeks),
               color = 'gray70', size = 1.5) +
    geom_smooth(formula = cbind(weeks, total-weeks) ~ x, 
                method = 'glm', method.args = list(family = quasibinomial(link = 'logit')),
                color = 'tomato3', fill = 'tomato1') +
    ylim(0,1) +
    ggtitle('Female') +
    ylab('Proportion of weeks with detection') + xlab('Distance from center (km)') +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = 'gray90'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          # axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))
  plot_mod1_fem
  
  plot_mod1_male <- ggplot(ac_data, aes(x = dist_actual/1000, y = weeksMale/survWeeks,
                                       weeks = weeksMale, total = survWeeks)) +
    geom_point(data = ac_data[ac_data$weeksMale > 0,], #we don't want to plot 0s
               aes(x = dist_actual/1000, y = weeksMale / survWeeks),
               color = 'gray70', size = 1.5) +
    geom_smooth(formula = cbind(weeks, total-weeks) ~ x, 
                method = 'glm', method.args = list(family = quasibinomial(link = 'logit')),
                color = 'dodgerblue4', fill = 'dodgerblue1') +
    ylim(0,1) +
    ggtitle('Male') +
    ylab('Proportion of weeks with detection') + xlab('Distance from center (km)') +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = 'gray90'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          # axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))
  plot_mod1_male

```

#### Plot another way (predictions from GLM rather than geom_smooth in ggplot)
#### ** we get the same results **

```{r echo = FALSE}

## create grid of covariates
  dist_grid <- sort(c(mean(ac_data$dist_actual), seq(min(ac_data$dist_actual),
                                                     max(ac_data$dist_actual), length = 50)))
  site_grid <- unique(ac_data$site_name)
  nest_grid <- unique(ac_data$nested)

  covars <- expand.grid('dist_actual' = dist_grid, 'site' = site_grid, 'nested' = nest_grid)

  
## predict
  preds1_any <- predict(mod1_any, type = 'link', newdata = covars, se.fit = TRUE)
  preds1_any_df <- data.frame(covars, 'fit' = mod1_any$family$linkinv(preds1_any$fit), 
                    'lci' = mod1_any$family$linkinv(preds1_any$fit - (1.96 * preds1_any$se.fit)),
                    'uci' = mod1_any$family$linkinv(preds1_any$fit + (1.96 * preds1_any$se.fit)))
  
  ggplot(preds1_any_df, aes(x = dist_actual/1000, y = fit)) +
    geom_point(data = ac_data[ac_data$weeksAny > 0,], #we don't want to plot 0s
               aes(x = dist_actual/1000, y = weeksAny / survWeeks),
               color = 'gray70', size = 1.5) +
    geom_line(color = 'aquamarine4', linewidth = 1) +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.3, color = NA, fill = 'aquamarine3') +
    ylim(0,1) + xlab('Distance (km)') + ylab('Predicted % weeks with detection') +
    ggtitle('All') +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = 'gray90'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))    
  



```

### Add nesting status

```{r echo = FALSE}

  mod2_any <- glm(cbind(weeksAny, survWeeks - weeksAny) ~ dist_actual + nested, 
                  data = ac_data, family = quasibinomial(link = 'logit'))
  summary(mod2_any)
    #no diff in 'nested'
  
  mod2_female <- glm(cbind(weeksFemale, survWeeks - weeksFemale) ~ dist_actual + nested, 
                     data = ac_data, family = quasibinomial(link = 'logit'))
  summary(mod2_female)
    #yes, difference in nested
  
  mod2_male <- glm(cbind(weeksMale, survWeeks - weeksMale) ~ dist_actual + nested, 
                   data = ac_data, family = quasibinomial(link = 'logit'))
  summary(mod2_male)
    #no difference in male

```

#### Plot

```{r echo = FALSE}

  plot_mod2_any <- ggplot(ac_data, aes(x = dist_actual/1000, y = weeksAny/survWeeks,
                                       weeks = weeksAny, total = survWeeks)) +
    geom_point(data = ac_data[ac_data$weeksAny > 0,], #we don't want to plot 0s
               aes(x = dist_actual/1000, y = weeksAny / survWeeks),
               color = 'gray70', size = 1.5) +
    geom_smooth(formula = cbind(weeks, total-weeks) ~ x, 
                method = 'glm', method.args = list(family = quasibinomial(link = 'logit')),
                color = 'aquamarine4', fill = 'aquamarine3') +
    ylim(0,1) +
    ggtitle('All') +
    ylab('Proportion of weeks with detection') + xlab('Distance from center (km)') +
    facet_grid(~nested) +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = 'gray90'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          # axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))
  
  plot_mod2_any

  plot_mod2_fem <- ggplot(ac_data, aes(x = dist_actual/1000, y = weeksFemale/survWeeks,
                                       weeks = weeksFemale, total = survWeeks)) +
    geom_point(data = ac_data[ac_data$weeksFemale > 0,], #we don't want to plot 0s
               aes(x = dist_actual/1000, y = weeksFemale / survWeeks),
               color = 'gray70', size = 1.5) +
    geom_smooth(formula = cbind(weeks, total-weeks) ~ x, 
                method = 'glm', method.args = list(family = quasibinomial(link = 'logit')),
                color = 'tomato3', fill = 'tomato1') +
    ylim(0,1) +
    ggtitle('Female') +
    ylab('Proportion of weeks with detection') + xlab('Distance from center (km)') +
    facet_grid(~nested) +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = 'gray90'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          # axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))
  
  plot_mod2_fem
  
  
  plot_mod2_male <- ggplot(ac_data, aes(x = dist_actual/1000, y = weeksMale/survWeeks,
                                       weeks = weeksMale, total = survWeeks)) +
    geom_point(data = ac_data[ac_data$weeksMale > 0,], #we don't want to plot 0s
               aes(x = dist_actual/1000, y = weeksMale / survWeeks),
               color = 'gray70', size = 1.5) +
    geom_smooth(formula = cbind(weeks, total-weeks) ~ x, 
                method = 'glm', method.args = list(family = quasibinomial(link = 'logit')),
                color = 'dodgerblue4', fill = 'dodgerblue1') +
    ylim(0,1) +
    ggtitle('Male') +
    ylab('Proportion of weeks with detection') + xlab('Distance from center (km)') +
    facet_grid(~nested) +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_line(color = 'gray90'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          # axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))
  
  plot_mod2_male
  
```
