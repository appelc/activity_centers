data(frogs)

  head(pfer.bin) #detection history
  dim(pfer.bin)  #130 sites, 3 occasions

  # create input object
  pferUMF <- unmarkedFrameOccu(pfer.bin)
  str(pferUMF)
  plot(pferUMF, panels=4) #view detections visually

  # add some fake covariates for illustration
  siteCovs(pferUMF) <- data.frame(sitevar1 = rnorm(numSites(pferUMF)))
  obsCovs(pferUMF) <- data.frame(obsvar1 = rnorm(numSites(pferUMF) * obsNum(pferUMF)))
    #observation covariates are in site-major, observation-minor order
  
  str(pferUMF)
    head(pferUMF@siteCovs); nrow(pferUMF@siteCovs) #sites
    head(pferUMF@obsCovs); nrow(pferUMF@obsCovs)   #sites x occasions
  
  # run model
  (fm <- occu(~ obsvar1 ~ 1, pferUMF))
    confint(fm, type='det', method = 'normal')
    confint(fm, type='det', method = 'profile')

  
  # estimate detection effect at obsvars=0.5
  (lc <- linearComb(fm['det'], c(1,0.5)))

    # transform this to probability (0 to 1) scale and get confidence limits
    (btlc <- backTransform(lc))
    confint(btlc, level = 0.9)

  # Empirical Bayes estimates of proportion of sites occupied
  re <- ranef(fm)
  sum(bup(re, stat="mode"))
    #the model estimates that all sites were occupied...?
   
      #but naive occupancy was only 43 out of 130 (33%)
      #well, 'p' was quite low