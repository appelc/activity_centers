## testing models with counts (see also 03_mkdown_models_counts.Rmd)

library(data.table)
library(broom)
library(tidyverse)
library(effects)
library(ggplot2)
library(plyr)
library(kableExtra)
library(MASS) #for negative binomial

## load data ####

  acData <- fread('output/05_ac_2021_merged.csv')
  acData

  anySTOC <- acData[,c('STOC_ANY_N','duration','Distance','dist_intended','reproState')]
  anySTOC  
  
  #convert NAs to 0s and order levels
  anySTOC$stoc_any_count <- ifelse(is.na(anySTOC$STOC_ANY_N), 0, anySTOC$STOC_ANY_N)
  anySTOC$reproState <- factor(anySTOC$reproState, levels = c('pair','nest','fledged'))

    summary(anySTOC$stoc_any_count)
    hist(anySTOC$stoc_any_count)  

    summary(log(anySTOC$stoc_any_count))
    hist(log(anySTOC$stoc_any_count))
    
    plot(anySTOC$stoc_any_count ~ anySTOC$Distance)
    
    
## Poisson GLM (take 1) ####
  
  mod1 <- glm(stoc_any_count ~ Distance, family = 'poisson', data = anySTOC)
  
  summary(mod1)  #overdispersed (deviance >>> degrees of freedom)
  
  #plot response anyway -- is the model specified correctly?
    y_up <- exp(augment(mod1, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] +
                  2*augment(mod1, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    y_low <- exp(augment(mod1, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] -
                  2*augment(mod1, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    
    plot(anySTOC$stoc_any_count ~ anySTOC$Distance, type = 'n',
         ylab = 'STOC counts', cex.lab = 1.5, xlab = 'Distance', main = 'model1')
    polygon(c(rev(anySTOC$Distance[order(anySTOC$Distance)]), 
              anySTOC$Distance[order(anySTOC$Distance)]),
            (c(rev(y_up), y_low)), col = 'grey80', border = NA)
    lines(anySTOC$Distance[order(anySTOC$Distance)], exp(augment(mod1)$.fitted)[order(anySTOC$Distance)],
          lwd = 2, lty = 2, col = 'blue')
    points(anySTOC$stoc_any_count ~ anySTOC$Distance, pch = 19)
      #confidence interval is really narrow! otherwise okay
  
  #plot predicted vs. actual counts
    y_up1 = exp(augment(mod1, se_fit = TRUE)$.fitted +
                  2*augment(mod1, se_fit = TRUE)$.se.fit)
    y_low1 = exp(augment(mod1, se_fit = TRUE)$.fitted -
                   2*augment(mod1, se_fit = TRUE)$.se.fit)
    
    plot(anySTOC$Distance, anySTOC$stoc_any_count, lwd = 2, type = 'n', 
         pch = 19, cex = 0.25, ylab = 'STOC count', xlab = '', cex.lab = 1.5)
    polygon(c(rev(anySTOC$Distance), anySTOC$Distance), (c(rev(y_up1), y_low1)),
            col = 'grey80', border = NA)
    points(anySTOC$Distance, anySTOC$stoc_any_count, lwd = 2, type = 'b', pch = 19, cex = 0.25)
    lines(anySTOC$Distance, exp(augment(mod1)$.fitted), lwd = 2, lty = 2, col = 'blue')
  

  
## Poisson GLM (take 2) -- log(distance) ####
  
  #any zeroes?
  sort(anySTOC$Distance) #yes, 4 (by design -- these are the activity centers)
  anySTOC[anySTOC$Distance == 0,]$Distance <- 1  ##replace with small number (1 meter)
  
  mod2 <- glm(stoc_any_count ~ log(Distance), family = 'poisson', data = anySTOC)

    summary(mod2)  #still over-dispersed
  
  #plot response anyway -- is the model specified correctly?
    plot(anySTOC$stoc_any_count ~ anySTOC$Distance, type = 'n',
         ylab = 'STOC counts', cex.lab = 1.5, xlab = 'Distance', main = 'mod2')
    y_up2 <- exp(augment(mod2, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] +
                   2*augment(mod2, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    y_low2 <- exp(augment(mod2, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] -
                    2*augment(mod2, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    polygon(c(rev(anySTOC$Distance[order(anySTOC$Distance)]), 
              anySTOC$Distance[order(anySTOC$Distance)]),
            (c(rev(y_up2), y_low2)), col = 'grey80', border = NA)
    lines(anySTOC$Distance[order(anySTOC$Distance)], exp(augment(mod2)$.fitted)[order(anySTOC$Distance)],
          lwd = 2, lty = 2, col = 'blue')
    points(anySTOC$stoc_any_count ~ anySTOC$Distance, pch = 19)
      #looks the same ... don't need to use log???
  
  
## Quasipoisson GLM ####
  
  mod3 <- glm(stoc_any_count ~ Distance, family = quasipoisson(link = 'log'), data = anySTOC)

    summary(mod3)  #still over-dispersed. 
                   #but don't look at deviance:d.o.f. now. look at Dispersion parameter (should be < 4 ideally)
    
  #compare with null model
    nullmod <- glm(stoc_any_count ~ 1, family = 'quasipoisson', data = anySTOC)
    anova(nullmod, mod3, test = 'Chisq')
    
  #plot response 
    plot(anySTOC$Distance, anySTOC$stoc_any_count, type = 'n', main = 'Quasipoisson')
    y_up3 <- exp(augment(mod3, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] +
                  2*augment(mod3, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    y_low3 <- exp(augment(mod3, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] -
                   2*augment(mod3, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    polygon(c(rev(anySTOC$Distance[order(anySTOC$Distance)]), anySTOC$Distance[order(anySTOC$Distance)]),
            (c(rev(y_up3), y_low3)), col = 'grey80', border = NA)
    points(anySTOC$Distance, anySTOC$stoc_any_count, pch = 19)
    lines(anySTOC$Distance[order(anySTOC$Distance)],
          exp(augment(mod3, se_fit = TRUE)$.fitted)[order(anySTOC$Distance)], lwd = 2, lty = 2, col = 'blue')
    
  #plot predicted vs. actual
    y_up31 = exp(augment(mod3, se_fit = TRUE)$.fitted +
                  2*augment(mod3, se_fit = TRUE)$.se.fit)
    y_low31 = exp(augment(mod3, se_fit = TRUE)$.fitted -
                   2*augment(mod3, se_fit = TRUE)$.se.fit)
    plot(anySTOC$Distance, anySTOC$stoc_any_count, lwd = 2, type = 'n', 
         pch = 19, cex = 0.25, ylab = 'STOC count', xlab = '', cex.lab = 1.5)
    polygon(c(rev(anySTOC$Distance), anySTOC$Distance), (c(rev(y_up31), y_low31)),
            col = 'grey80', border = NA)
    points(anySTOC$Distance, anySTOC$stoc_any_count, lwd = 2, type = 'b', pch = 19, cex = 0.25)
    lines(anySTOC$Distance, exp(augment(mod3)$.fitted), lwd = 2, lty = 2, col = 'blue')
      #no clue
    
    
## Negative binomial GLM ####
    
  mod4 <- glm.nb(stoc_any_count ~ Distance, data = anySTOC)

    summary(mod4)  #much better!
    
  #plot response
    plot(anySTOC$Distance, anySTOC$stoc_any_count, type = 'n', main = 'Neg Binomial', ylab = 'STOC count')
    y_up4 <- exp(augment(mod4, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] +
                   2*augment(mod4, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    y_low4 <- exp(augment(mod4, se_fit = TRUE)$.fitted[order(anySTOC$Distance)] -
                    2*augment(mod4, se_fit = TRUE)$.se.fit[order(anySTOC$Distance)])
    polygon(c(rev(anySTOC$Distance[order(anySTOC$Distance)]), anySTOC$Distance[order(anySTOC$Distance)]),
            (c(rev(y_up4), y_low4)), col = 'grey80', border = NA)
    points(anySTOC$Distance, anySTOC$stoc_any_count, pch = 19)
    lines(anySTOC$Distance[order(anySTOC$Distance)],
          exp(augment(mod4, se_fit = TRUE)$.fitted)[order(anySTOC$Distance)], lwd = 2, lty = 2, col = 'blue')

  #plot predicted vs. actual  
    y_up41 = exp(augment(mod4, se_fit = TRUE)$.fitted +
                   2*augment(mod4, se_fit = TRUE)$.se.fit)
    y_low41 = exp(augment(mod4, se_fit = TRUE)$.fitted -
                    2*augment(mod4, se_fit = TRUE)$.se.fit)
    plot(anySTOC$Distance, anySTOC$stoc_any_count, lwd = 2, type = 'n', 
         pch = 19, cex = 0.25, ylab = 'STOC count', xlab = '', cex.lab = 1.5)
    polygon(c(rev(anySTOC$Distance), anySTOC$Distance), (c(rev(y_up41), y_low41)),
            col = 'grey80', border = NA)
    points(anySTOC$Distance, anySTOC$stoc_any_count, lwd = 2, type = 'b', pch = 19, cex = 0.25)
    lines(anySTOC$Distance, exp(augment(mod4)$.fitted), lwd = 2, lty = 2, col = 'blue')
      #no clue -- seems low?
    
    
  #plot another way
    summary(mod4)$coefficients[1, 1] #intercept
    summary(mod4)$coefficients[2, 1] #slope
    
    summary(mod4)$coefficients[1, 2] #intercept SE
    summary(mod4)$coefficients[2, 2] #slope SE

    # Make model predicitions
    pp <- predict(mod4,
                  newdata = data.frame(Distance = 1:3500),
                  se.fit = TRUE)
    linkinv <- family(mod4)$linkinv # inverse-link function
    
    # Prepare to plot model results
    pframe <- as.data.frame(pp$fit)
    names(pframe) <- "pred0"
    pframe$pred <- linkinv(pp$fit)
    sc <- abs(qnorm((1-0.95)/2))  # Normal approx. to likelihood
    pframe <- transform(pframe, lwr = linkinv(pred0-sc*pp$se.fit), upr = linkinv(pred0+sc*pp$se.fit))
    
    # Plot!
    plot(anySTOC$Distance, anySTOC$stoc_any_count,
         ylab = 'Count of STOC', xlab = 'Distance (km)')
    lines(pframe$pred, lwd = 2) # predicted values
    lines(pframe$upr, col = 2, lty = 3, lwd = 2) # show error bounds
    lines(pframe$lwr, col = 2, lty = 3, lwd = 2)
    
    
  #yet another way
    ggplot(anySTOC, aes(x = Distance/1000, y =stoc_any_count)) +
      geom_point(color = 'gray60') +
      geom_smooth(method = 'glm.nb', formula = y ~ x) +
      geom_rug(sides = 'b', lwd = 1) +
      xlab('Distance from center (km)') + ylab('STOC count') +
      ggtitle('Negative binomial') +
      theme(
        panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray90'),
        axis.line = element_line(),
        axis.title = element_text(size = 18),
        # axis.title.x = element_blank(),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = 'none',
        legend.background = element_rect(fill='transparent'))
    
    #
    
## Check residuals ####
  
  #does variance look constant?
    plot(stoc_any_count ~ Distance, data = anySTOC, pch = 19, ylab = 'Count', xlab = 'Distance') 
    
  #try log
    plot(log(stoc_any_count) ~ log(Distance), data = anySTOC, pch = 19, ylab = 'log(Count)', xlab = 'log(Distance)')
      
  #look at distribution of counts
    mean(anySTOC$stoc_any_count)
    var(anySTOC$stoc_any_count)  #huge variance!
    summary(anySTOC$stoc_any_count)
    
  