
## GLMMTB practice

Owls <- transform(Owls,
                  Nest=reorder(Nest,NegPerChick),
                  logBroodSize=log(BroodSize),
                  NCalls=SiblingNegotiation)

head(Owls)

fit_zipoiss <- glmmadmb(NCalls~(FoodTreatment+ArrivalTime)*SexParent+
                            offset(logBroodSize)+(1|Nest),
                          data=Owls,
                          zeroInflation=TRUE,
                          family="poisson")


====
  
  mod3.repro <- glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY) 
                      ~ reproState + (1|SITESTN), 
                      family = binomial, data = daysMerge)
  summary(mod3.repro)

=======
    
  install.packages("glmmTMB", repos="https://glmmTMB.github.io/glmmTMB/repos")
    library(glmmTMB)

  #example data
  Owls <- transform(Owls,
                    Nest=reorder(Nest,NegPerChick),
                    NCalls=SiblingNegotiation,
                    FT=FoodTreatment)
  
  fit_zipoisson <- glmmTMB(NCalls~(FT+ArrivalTime)*SexParent+
                             offset(log(BroodSize))+(1|Nest),
                           data=Owls,
                           ziformula=~1,
                           family=poisson)
  summary(fit_zipoisson)
  
  #my data
  head(daysMerge)
  
  tmp1 <- glmmTMB(cbind(nights_STOC_ANY, duration - nights_STOC_ANY)  #new way
                  ~ reproState + (1|SITESTN), data = daysMerge, 
                  family = binomial, REML = TRUE)

  tmp2 <- glmer(cbind(nights_STOC_ANY, duration - nights_STOC_ANY)    #old way
                ~ reproState + (1|SITESTN),
                family = binomial, data = daysMerge)

  summary(tmp1) 
  summary(tmp2) #compare
  
  #now try with Distance
  distnew <- glmmTMB(cbind(nights_STOC_ANY, duration - nights_STOC_ANY)  #new way
                     ~ Distance + (1|SITESTN), data = daysMerge, 
                     family = binomial)
  
  distnewREML <- glmmTMB(cbind(nights_STOC_ANY, duration - nights_STOC_ANY)  #new way
                         ~ Distance + (1|SITESTN), data = daysMerge, 
                         family = binomial, REML = TRUE)   
  
  summary(distnew)  
  summary(distnewREML)    
  
  