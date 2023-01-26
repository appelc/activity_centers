## models

library(data.table)
library(broom)
library(tidyverse)
library(effects)
library(ggplot2)
library(plyr)

## read in data
  acData <- fread('output/05_ac_2021_merged.csv')
  head(acData)  
  
## format for a binomial model (each row is an ARU site)
  anySTOC <- acData[,c('STOC_ANY_N','nDaysSampled','dist_m','reproState')]
  head(anySTOC)  
    table(anySTOC$STOC_ANY_N, useNA = 'always') 
  
  #convert to binary (and convert NAs to 0s)
  anySTOC$STOC <- ifelse(is.na(anySTOC$STOC_ANY_N) | anySTOC$STOC_ANY_N == 0, 0, 1)
    head(anySTOC)
    table(anySTOC$STOC)
  
    
## fit a linear model
  stoc.lm <- lm(STOC ~ reproState, data = anySTOC)  
    hist(resid(stoc.lm), xlab = 'residuals', main = '', col = 'gray')    #the residuals are not normal
    summary(stoc.lm)
  
    
## generalized linear models (GLMs) -- Bernoulli/Binomial distribution (logistic regression) 
    
  #how many sites had each reproState
    tapply(anySTOC$STOC, anySTOC$reproState, length)
    
  #how many sites had STOC detections by each reproState
    tapply(anySTOC$STOC, anySTOC$reproState, sum)
    
  #What proportion STOC detections were from activity centers in each reproState?
    tapply(anySTOC$STOC, anySTOC$reproState, mean)
    
  #arrange STOC detections according to reproState
    summarise(group_by(anySTOC, reproState, STOC), count = n())
    
    #Can also be written like this
    anySTOC %>%
      group_by(reproState, STOC) %>%
      summarise(count = n())
    
    table(anySTOC$reproState, anySTOC$STOC)
    
    #or like this
    anySTOC %>%
      count(reproState, STOC, sort = TRUE)
    
    #or graphically
    anySTOC %>%
      mutate(STOC = as_factor(STOC)) %>%
      dplyr::count(reproState, STOC) %>%
      ggplot(aes(x = reproState, y = n, fill = STOC)) +
      geom_col()
      #n are number of sites (ARU stations)
    
#########
## BINOMIAL MODELS ####    
    
## Did STOC detections differ by reproductive state?    
## fit binomial GLM with reproState
    
  #re-order the state factor  
  anySTOC$reproState <- factor(anySTOC$reproState, levels = c('pair','nest','fledged'))    
    
  stoc.glm <- glm(STOC ~ reproState, data = anySTOC, family = binomial)
    summary(stoc.glm)  
    tidy(stoc.glm)
  
  #How do you interpret this? What if you want to see which groups different from each other?
  TukeyHSD(aov(stoc.glm))
    
    #Not these estimates are on the logit scale!
    ## Interpreting logistic regression outputs 
    coef(stoc.glm)
    
    #We need to back-transform using the inverse logit function:  
    plogis(coef(stoc.glm)[1]) #probability of STOC detections at sites with reproState = pair
    
    #Looking at the data, the proportion of 'pair' sites with STOC detections (should match above)
    sum(anySTOC$STOC[anySTOC$reproState == 'pair']) / nrow(anySTOC[anySTOC$reproState == 'pair',])
    
    #Probability of STOC detection at 'nest' sites? 
    plogis(coef(stoc.glm)[1] + coef(stoc.glm)[2])  #add intercept (baseline) to the parameter estimate
    sum(anySTOC$STOC[anySTOC$reproState == 'nest']) / nrow(anySTOC[anySTOC$reproState == 'nest',])  #should match the data
      
    #Probability of STOC detection at 'fledged' sites
    plogis(coef(stoc.glm)[1] + coef(stoc.glm)[3])
    sum(anySTOC$STOC[anySTOC$reproState == 'fledged']) / nrow(anySTOC[anySTOC$reproState == 'fledged',])
    
    #Model interpretation using `effects` package to back transform all parameter estimates
    allEffects(stoc.glm)
    
    ## Effects plot
    plot(allEffects(stoc.glm))
    
    
## Did distance from center affect STOC detections?
## fit binomial GLM with distance   **DISTANCE AS CONTINUOUS**
    
  #plot
  barplot(table(anySTOC$STOC, anySTOC$dist_m))

  #did STOC detections vary by distance?
    stoc.dist <- glm(STOC ~ dist_m, data = anySTOC, family = binomial)
    tidy(stoc.dist)
    
    allEffects(stoc.dist)
    
    plot(allEffects(stoc.dist))
    
    
## Did STOC detections vary by distance *and* reproductive state?  
## fit binomial GLM with reproState * distance
    
  #look at the data
  tapply(anySTOC$STOC, list(anySTOC$reproState, anySTOC$dist_m), sum)  

  #fit model with both covariates and interactions
  stoc.state.dist <- glm(STOC ~ reproState * dist_m, data = anySTOC, family = binomial)
  tidy(stoc.state.dist)  
    
  #effects
  allEffects(stoc.state.dist)
  plot(allEffects(stoc.state.dist))  
  
  #Tukey's honest significant difference
  # TukeyHSD(aov(stoc.state.dist))          #for categorical?
  
  
## Compare all the models  
  AIC(glm(STOC ~ 1, data = anySTOC, family = binomial)) #null model
  AIC(stoc.glm)         #reproState effect
  AIC(stoc.dist)        #distance effect (continuous)             **but v. similar AIC and simpler** -- effect of distance
  AIC(stoc.state.dist)  #interaction of reproState and distance   **lowest AIC**

  
  
  
############################################################    
  
## model as above, but with data organized as Bernoulli ####
  
  #re-format dataframe as Bernoulli (each row is a combo of groups)
  head(anySTOC)
  stocBern <- dcast(anySTOC[,-c(1:2)], dist_m + reproState ~ STOC, fun.aggregate = length, value.var = 'STOC')
    colnames(stocBern)[c(3:4)] <- c('No','Yes')
    head(stocBern)
    levels(stocBern$reproState)  #make sure levels match above
    nrow(stocBern) #3 repro states * 6 distances = 18 groups 
    

## Did STOC detections differ by reproductive state?     
## fit binomial GLM with reproState (Bernoulli data)  
  bern.repro.glm <- glm(cbind(Yes, No) ~ reproState, data = stocBern, family = binomial)
    summary(bern.repro.glm)
    tidy(bern.repro.glm)
    # tidy(stoc.glm) #Compare to repro binomail GLM (**it's the same**)should be the same
  
  #effects
  allEffects(bern.repro.glm)
  plot(allEffects(bern.repro.glm))  
  
  ## alternative plot
  df.repro.glm <- allEffects(bern.repro.glm)   #save the estimates for your target effect
  df.repro.glm <- data.frame(df.repro.glm$reproState)
  df.repro.glm$reproState.reproState <- factor(df.repro.glm$reproState.reproState,
                                               levels = c('pair','nest','fledged'))
  
  # d2 <- ddply(stocBern, .(reproState), summarise, mean_variable = prop(Yes,na.rm=TRUE))
  (df.repro.2 <- data.frame('reproState' = c('pair','nest','fledged'),
                            'raw' = c(sum(stocBern[stocBern$reproState %in% 'pair',]$Yes) / (sum(stocBern[stocBern$reproState %in% 'pair',]$Yes) + sum(stocBern[stocBern$reproState %in% 'pair',]$No)),
                                      sum(stocBern[stocBern$reproState %in% 'nest',]$Yes) / (sum(stocBern[stocBern$reproState %in% 'nest',]$Yes) + sum(stocBern[stocBern$reproState %in% 'nest',]$No)),
                                      sum(stocBern[stocBern$reproState %in% 'fledged',]$Yes) / (sum(stocBern[stocBern$reproState %in% 'fledged',]$Yes) + sum(stocBern[stocBern$reproState %in% 'fledged',]$No)))))
  
  plot.repro.glm <- ggplot(df.repro.glm, aes(x = reproState.reproState, y = reproState.fit)) + 
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = reproState.lower, ymax = reproState.upper)) +
    # geom_point(data = df.repro.2, aes(x = reproState, y = raw), size = 4, color = 'red')+
    ylim(c(0,1)) +
    ylab('probability of spotted owl detection') +
    ggtitle('Spotted owl detections by reproductive state') +
    theme(panel.background = element_rect(fill = 'transparent'),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(hjust = 0.5),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = 'none',
          legend.background = element_rect(fill='transparent'))
  plot.repro.glm
  ##
  
  #coefficients are on the logit scale 
  coef(bern.repro.glm)
  
  #back-transform using the inverse logit function
  plogis(coef(bern.repro.glm)[1]) #probability of STOC detections at sites with reproState = pair
  
  #Looking at the data, the proportion of 'pair' sites with STOC detections (should match above)
  sum(stocBern$Yes[stocBern$reproState == 'pair']) / 
    (sum(stocBern$Yes[stocBern$reproState == 'pair']) + sum(stocBern$No[stocBern$reproState == 'pair']))
  
  #Probability of STOC detection at 'nest' sites? 
  plogis(coef(bern.repro.glm)[1] + coef(bern.repro.glm)[2])  #add intercept (baseline) to the parameter estimate
  sum(stocBern$Yes[stocBern$reproState == 'nest']) / 
    (sum(stocBern$Yes[stocBern$reproState == 'nest']) + sum(stocBern$No[stocBern$reproState == 'nest']))
  
  #Probability of STOC detection at 'pair' sites
  plogis(coef(bern.repro.glm)[1] + coef(stoc.glm)[3])
  sum(stocBern$Yes[stocBern$reproState == 'fledged']) / 
    (sum(stocBern$Yes[stocBern$reproState == 'fledged']) + sum(stocBern$No[stocBern$reproState == 'fledged']))
  
  
## Did distance from center affect STOC detections?
## fit binomial GLM with distance (Bernoulli data)  **DISTANCE AS CONTINUOUS**
  bern.dist.glm <- glm(cbind(Yes, No) ~ dist_m, data = stocBern, family = binomial)
    summary(bern.dist.glm)
    tidy(bern.dist.glm)
    # tidy(stoc.dist)  #Compare to distance binomial GLM (should be the same)    
    
  #effects
  allEffects(bern.dist.glm)
  plot(allEffects(bern.dist.glm))  
    
  
## Did STOC detections vary by distance *and* reproductive state?  
## fit binomial GLM with reproState * distance (Bernoulli data)
  
  #look at the data
  tapply(stocBern$Yes, list(stocBern$reproState, stocBern$dist_m), sum)
  
  #fit model with both covariates and interactions
  bern.repro.dist.glm <- glm(cbind(Yes, No) ~ reproState * dist_m, data = stocBern, family = binomial)
    summary(bern.repro.dist.glm)
    tidy(bern.repro.dist.glm)
    # tidy(stoc.state.dist) #Compare to repro*distance binomial GLM (should be the same)
    
  #effects
  allEffects(bern.repro.dist.glm)
  plot(allEffects(bern.repro.dist.glm))    
  
    
## Compare models
  AIC(glm(cbind(Yes, No) ~ 1, data = stocBern, family = binomial)) #null model
  AIC(bern.repro.glm)       #reproState effect
  AIC(bern.dist.glm)        #distance effect (continuous)             **but v. similar AIC and simpler** -- effect of distance
  AIC(bern.repro.dist.glm)  #interaction of reproState and distance   **lowest AIC**
  
  
  
#############  
## Try modeling sex separately as response variable
  
  #create new dataframes
  head(acData)
  sexSTOC <- acData[,c('FEMALE_N','MALE_N','nDaysSampled','dist_m','reproState')]
    head(sexSTOC)
    table(sexSTOC$FEMALE_N, useNA = 'always')
    table(sexSTOC$MALE_N, useNA = 'always')
    
  #convert to binary (and convert NAs to 0s)
  sexSTOC$female <- ifelse(is.na(sexSTOC$FEMALE_N) | sexSTOC$FEMALE_N == 0, 0, 1)  
  sexSTOC$male   <- ifelse(is.na(sexSTOC$MALE_N) | sexSTOC$MALE_N == 0, 0, 1)  
    table(sexSTOC$female, useNA = 'always')
    table(sexSTOC$male, useNA = 'always')
  
  #re-format dataframe as Bernoulli (each row is a combo of groups)
  head(sexSTOC)
  femaleSTOC <- dcast(sexSTOC[,c(4:6)], dist_m + reproState ~ female, fun.aggregate = length, value.var = 'female')
    colnames(femaleSTOC)[c(3:4)] <- c('No','Yes')
    femaleSTOC$reproState <- factor(femaleSTOC$reproState, levels = c('pair','nest','fledged')) #order levels
    nrow(femaleSTOC) #3 repro states * 6 distances = 18 groups 
  maleSTOC <- dcast(sexSTOC[,c(4:5,7)], dist_m + reproState ~ male, fun.aggregate = length, value.var = 'male')
    colnames(maleSTOC)[c(3:4)] <- c('No','Yes')
    maleSTOC$reproState <- factor(maleSTOC$reproState, levels = c('pair','nest','fledged')) #order levels
    nrow(maleSTOC) #3 repro states * 6 distances = 18 groups   
  
    
## Effect of reproState
  fem.repro.glm <- glm(cbind(Yes, No) ~ reproState, data = femaleSTOC, family = binomial)
    tidy(fem.repro.glm)
  mal.repro.glm <- glm(cbind(Yes, No) ~ reproState, data = maleSTOC, family = binomial)  
    tidy(mal.repro.glm)
    
    #effects
    allEffects(fem.repro.glm)
    allEffects(mal.repro.glm) 
    
    par(mfrow = c(2,1))
    plot(allEffects(fem.repro.glm), main = 'reproState effect plot (females)')      
    plot(allEffects(mal.repro.glm), main = 'reproState effect plot (males)')      
      
    
## Effect of distance
  fem.dist.glm <- glm(cbind(Yes, No) ~ dist_m, data = femaleSTOC, family = binomial)
    tidy(fem.dist.glm)
  mal.dist.glm <- glm(cbind(Yes, No) ~ dist_m, data = maleSTOC, family = binomial)  
    tidy(mal.dist.glm)
    
    #effects
    allEffects(fem.dist.glm)
    allEffects(mal.dist.glm) 
    
    par(mfrow = c(2,1))
    plot(allEffects(fem.dist.glm), main = 'distance effect plot (females)')      
    plot(allEffects(mal.dist.glm), main = 'distance effect plot (males)')          
    
    
## Effect of reproState * distance
  fem.repro.dist.glm <- glm(cbind(Yes, No) ~ reproState * dist_m, data = femaleSTOC, family = binomial)
    tidy(fem.repro.dist.glm)
  mal.repro.dist.glm <- glm(cbind(Yes, No) ~ reproState * dist_m, data = maleSTOC, family = binomial)  
    tidy(mal.repro.dist.glm)
    
    #effects
    allEffects(fem.repro.dist.glm)
    allEffects(mal.repro.dist.glm) 
    
    par(mfrow = c(2,1))
    plot(allEffects(fem.repro.dist.glm), main = 'reproState*distance (females)')      
    plot(allEffects(mal.repro.dist.glm), main = 'reproState*distance (males)') 
    
    
## Compare models
  
  #Females
  AIC(glm(cbind(Yes, No) ~ 1, data = femaleSTOC, family = binomial))  #null model
  AIC(fem.repro.glm)
  AIC(fem.dist.glm)        #top model
  AIC(fem.repro.dist.glm)
        
  #Males
  AIC(glm(cbind(Yes, No) ~ 1, data = maleSTOC, family = binomial))  #null model
  AIC(mal.repro.glm)
  AIC(mal.dist.glm)        #very close
  AIC(mal.repro.dist.glm)  #top model
  
  
    
#############      
## trying to format data w/ sex as covariate in bernoulli model
    
    ## ---
    
    #re-format dataframe as Bernoulli (each row is a combo of groups) -- 3 repro states * 6 distances * 2 sexes = 36 groups
    head(acData)  
    sexSTOC <- acData[,c('FEMALE_N','MALE_N','dist_m','reproState')] #go back and get F/M columns (also want UNK and JUV?)
    head(sexSTOC)
    # sexSTOC$FEMALE_N <- ifelse(is.na(sexSTOC$FEMALE_N), 0, sexSTOC$FEMALE_N)
    # sexSTOC$MALE_N <- ifelse(is.na(sexSTOC$MALE_N), 0, sexSTOC$MALE_N)
    
    #convert to binary (and convert NAs to 0s)
    sexSTOC$f_STOC <- ifelse(is.na(sexSTOC$FEMALE_N) | sexSTOC$FEMALE_N == 0, 0, 1)
    sexSTOC$m_STOC <- ifelse(is.na(sexSTOC$MALE_N) | sexSTOC$MALE_N == 0, 0, 1)
    
    # #aggregate female/male detections by the other variables
    # sexSTOCagg <- aggregate(data = sexSTOC, cbind(sexSTOC$FEMALE_N, sexSTOC$MALE_N) ~ 
    #                           sexSTOC$reproState + sexSTOC$dist_m, FUN = length, drop = FALSE)
    # head(sexSTOCagg)
    # nrow(sexSTOCagg)
    # colnames(sexSTOCagg) <- c('reproState','dist_m','female','male')
    
    #now convert to long so that sex is a variable and there is a new 'count' column
    sexSTOClong <- data.table(melt(sexSTOC[,-c(1:2)], id.vars = c('dist_m','reproState'), 
                                   variable.name = 'sex', value.name = 'count'))
    head(sexSTOClong)
    nrow(sexSTOClong)
    sexSTOClong$count
    
    #and convert 'count' to 'No' and 'Yes' columns
    sexBern <- dcast(sexSTOClong, dist_m + reproState + sex ~ count, fun.aggregate = length, value.var = 'count')
    head(sexBern)
    nrow(sexBern)
    levels(sexBern$reproState)
    levels(sexBern$sex)
    colnames(sexBern)[c(4:5)] <- c('No','Yes')
    
    nrow(sexBern[sexBern$sex %in% 'f_STOC' & sexBern$Yes > 0,])
    nrow(sexBern[sexBern$sex %in% 'm_STOC' & sexBern$Yes > 0,])
    
    
    dcast(anySTOC[,-c(1:2)], dist_m + reproState ~ STOC, fun.aggregate = length, value.var = 'STOC')
    
    colnames(stocBern)[c(3:4)] <- c('No','Yes')
    head(stocBern)
    levels(stocBern$reproState)  #make sure levels match above
    nrow(stocBern) #3 repro states * 6 distances = 18 groups 
    
    
    
    # #cast
    # tmp <- dcast(sexSTOC[-c(1:2)], dist_m + reproState ~ f_STOC, fun.aggregate = length, value.var = c('f_STOC'))
    # head(tmp)
    # 
    
    #convert to long
    sexSTOClong <- melt(sexSTOC[,-c(1:2)], id.vars = c('dist_m','reproState'), variable.name = 'sex', value.name = 'count')
    head(sexSTOClong)
    
    #re-organize into groups
    sexBern <- dcast(sexSTOClong, dist_m + reproState + sex ~ count, value.var = 'count', fun.aggregate = length)
    head(sexBern)
    colnames(sexBern)[c(4:5)] <- c('No','Yes')
    sexBern$reproState <- factor(sexBern$reproState, levels = c('pair','nest','fledged'))  
    levels(sexBern$reproState)
    class(sexBern$sex)
    levels(sexBern$sex)
    
#############      
    
    
