
## Function to make predictions from 'occu' object and new covariate data

occPred <- function(modname, new_data, nickname){ #nickname argument not working
  
  nickname <- as.character(nickname)
  
  #predict
  detPreds <- predict(modname, type = 'det', new_data)
  occPreds <- predict(modname, type = 'state', new_data)
  
  #combine covariates with predictions
  predWide <- cbind(new_data, 'det' = detPreds$Predicted, 'occ' = occPreds$Predicted)
  lciWide  <- cbind(new_data, 'det' = detPreds$lower, 'occ' = occPreds$lower)
  uciWide  <- cbind(new_data, 'det' = detPreds$upper, 'occ' = occPreds$upper)
  
  predLong <- melt(predWide, id.vars = names(new_data), value.name = 'value')
  lciLong  <- melt(lciWide, id.vars = names(new_data), value.name = 'LCI')
  uciLong  <- melt(uciWide, id.vars = names(new_data), value.name = 'UCI')
  
  predData <- cbind(predLong, 'LCI' = lciLong$LCI, 'UCI' = uciLong$UCI)
  predData$group <- nickname
  
  return(predData)
  
}

#   #test
#   
#     preds12any <- occPred(p12_any, new_covar, 'any')
#     preds12fem <- occPred(p12_fem_fnlcp, new_covar, 'female')
#     preds12mal <- occPred(p12_mal_fnlcp, new_covar, 'male')
# 
#     predsAll <- do.call('rbind', list(preds12any, preds12fem, preds12mal))
#     
#  
#     #All together (y ~ dist)
#     plotTest <- ggplot(predsAll[predsAll$effort == effort_mean 
#                                          & predsAll$noise == noise_mean
#                                          & predsAll$variable %in% 'det',], 
#                             aes(x = dist/1000, y = value, color = group, fill = group)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) + #color=NA to remove outlines
#       ylim(0,1) +
#       ylab('Weekly detection probability (\u00B1 95 CI)') +
#       xlab('Distance (km)') +
#       ggtitle('p(distance + noise + effort + site) psi(.)') +
#       geom_rug(data = site_covars, aes(x = dist_actual/1000), inherit.aes = FALSE) +
#       facet_grid(~ site) + theme_bw()
#     
#     plotTest
#     
#     
# ## Function to combine and plot several
#     
#     #got pretty far with this but don't think it's worth it... 
#     #too complicated with needing to set each covariate to its mean, 
#     #and there aren't always the same number of covars
#     #(would be easier if they were already scaled, actually)
#     
# #test
#   predsDF <- predsAll
#   rugCovars <- site_covars[,c('dist_actual','site','nested')]
#     colnames(rugCovars) <- c('dist','site','nest')
#     rugCovars <- 
#   factorCovar <- 'site'
#       
# predPlot <- function(predsDF, rugCovars, factorCovar){
#   
#     covars <- names(predsDF)[!names(predsDF) %in% c('variable','value','LCI','UCI','group')]
#     covarsCont <- covars[!covars %in% factorCovar]
#     rugCovars <- data.frame(rugCovars)
#     
#     l <- length(covarsCont)
#     
#     #Plot each covar in turn
#     
#     # for (ll in 1:l)
#     
#     plotData <- predsDF[predsDF[covarsCont[1]] == mean(rugCovars[covarsCont[1]][,1], na.rm = TRUE)
#                         & predsDF[covarsCont[2]] == mean(rugCovars[covarsCont[2]][,1], na.rm = TRUE)
#                         & predsDF$variable %in% 'det',]
#     
#     plotCovar <- ggplot(predsDF[predsDF$effort == effort_mean 
#                                 & predsAll$noise == noise_mean
#                                 & predsAll$variable %in% 'det',], 
#                         
#                        aes(x = dist/1000, y = value, color = group, fill = group)) +
#       geom_line() +
#       geom_ribbon(aes(ymin = LCI, ymax = UCI), alpha = 0.3, color = NA) + #color=NA to remove outlines
#       ylim(0,1) +
#       ylab('Weekly detection probability (\u00B1 95 CI)') +
#       xlab('Distance (km)') +
#       ggtitle('p(distance + noise + effort + site) psi(.)') +
#       geom_rug(data = rugCovars, aes(x = dist_actual/1000), inherit.aes = FALSE) +
#       facet_grid(~ factorCovar) + theme_bw()
#     
#     plotTest
#     
#   
# }
#     
# 
# 
# 
# 
#     