## Combine and prep detection histories for activity center occupancy models

library(data.table)
library(reshape2)
library(dplyr)

## Any STOC (all calls ) -------------------------------------------------------

  ## Use left-justified detection histories for combined years
    # dh_any_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocAny_left.csv')
    # dh_any_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocAny_left.csv')

  ## Use staggered-entry detection histories where calendar dates match both years
    dh_any_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocAny_staggered.csv') #2021 weeks are good
    dh_any_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocAny_stag_both.csv') #I updated 2022 weeks to match
  
  ## Make sure sites are sorted alphabetically by site name, then delete that column  
    dh_any_21 <- dh_any_21[order(dh_any_21$SITE_STN),]; dh_any_21 <- data.frame(dh_any_21)
      rownames(dh_any_21) <- paste('2021', dh_any_21$SITE_STN, sep = '_')
      dh_any_21 <- dh_any_21[,-c(1:3, 24:25)]
    dh_any_22 <- dh_any_22[order(dh_any_22$SITE_STN),]; dh_any_22 <- data.frame(dh_any_22)
      rownames(dh_any_22) <- paste('2022', dh_any_22$SITE_STN, sep = '_')
      dh_any_22 <- dh_any_22[,-c(1:3, 26:27)]
  
  ## Combine years
   dh_any <- bind_rows(dh_any_21, dh_any_22)
   
  ## Convert to binary
   dh_any[dh_any > 0] <- 1
  
  ## Save
    # write.csv(dh_any, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocAny_left.csv')
   write.csv(dh_any, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocAny_stag.csv')
  
   
## Any STOC (FNLC only) --------------------------------------------------------
   
   ## Use staggered-entry detection histories where calendar dates match both years
   dh_anyFNLC_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stoc4n_staggered.csv') #2021 weeks are good
   dh_anyFNLC_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stoc4n_stag_both.csv') #I updated 2022 weeks to match
   
   ## Make sure sites are sorted alphabetically by site name, then delete that column  
   dh_anyFNLC_21 <- dh_anyFNLC_21[order(dh_anyFNLC_21$SITE_STN),]; dh_anyFNLC_21 <- data.frame(dh_anyFNLC_21)
     rownames(dh_anyFNLC_21) <- paste('2021', dh_anyFNLC_21$SITE_STN, sep = '_')
     dh_anyFNLC_21 <- dh_anyFNLC_21[,-c(1:3, 24:25)]
   dh_anyFNLC_22 <- dh_anyFNLC_22[order(dh_anyFNLC_22$SITE_STN),]; dh_anyFNLC_22 <- data.frame(dh_anyFNLC_22)
     rownames(dh_anyFNLC_22) <- paste('2022', dh_anyFNLC_22$SITE_STN, sep = '_')
     dh_anyFNLC_22 <- dh_anyFNLC_22[,-c(1:3, 26:27)]
   
   ## Combine years
   dh_anyFNLC <- bind_rows(dh_anyFNLC_21, dh_anyFNLC_22)
   
   ## Convert to binary
   dh_anyFNLC[dh_anyFNLC > 0] <- 1
   
   ## Save
   # write.csv(dh_any, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocAny_left.csv')
   write.csv(dh_any, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocAnyFNLC_stag.csv')
   
   
## Female ----------------------------------------------------------------------
    
  ## Use left-justified detection histories for combined years    
    # dh_fem_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocFemale_left.csv')
    # dh_fem_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocFemale_left.csv')

  ## Use staggered-entry detection histories where calendar dates match both years
   dh_fem_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocFemale_staggered.csv') #2021 weeks are good
   dh_fem_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocFemale_stag_both.csv') #I updated 2022 weeks to match
   
  ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_fem_21 <- dh_fem_21[order(dh_fem_21$SITE_STN),]; dh_fem_21 <- data.frame(dh_fem_21)
      rownames(dh_fem_21) <- paste('2021', dh_fem_21$SITE_STN, sep = '_')
      dh_fem_21 <- dh_fem_21[,-c(1:3, 24:25)]
    dh_fem_22 <- dh_fem_22[order(dh_fem_22$SITE_STN),]; dh_fem_22 <- data.frame(dh_fem_22)
      rownames(dh_fem_22) <- paste('2022', dh_fem_22$SITE_STN, sep = '_')
      dh_fem_22 <- dh_fem_22[,-c(1:3, 26:27)]

  ## Combine years
    dh_fem <- bind_rows(dh_fem_21, dh_fem_22)
  
  ## Convert to binary
    dh_fem[dh_fem > 0] <- 1
      
  ## Save
    # write.csv(dh_fem, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocFemale_left.csv')
    write.csv(dh_fem, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocFemale_stag.csv')
    
## Female FNLC -----------------------------------------------------------------
    
  ## Use left-justified detection histories for combined years    
    # dh_fem_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocFemaleFNLC_left.csv')
    # dh_fem_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocFemaleFNLC_left.csv')
    
  ## Use staggered-entry detection histories where calendar dates match both years
    dh_fem_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocFemaleFNLC_staggered.csv') #2021 weeks are good
    dh_fem_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocFemaleFNLC_stag_both.csv') #I updated 2022 weeks to match
    
    ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_fem_21 <- dh_fem_21[order(dh_fem_21$SITE_STN),]; dh_fem_21 <- data.frame(dh_fem_21)
      rownames(dh_fem_21) <- paste('2021', dh_fem_21$SITE_STN, sep = '_')
      dh_fem_21 <- dh_fem_21[,-c(1:3, 24:25)]
    dh_fem_22 <- dh_fem_22[order(dh_fem_22$SITE_STN),]; dh_fem_22 <- data.frame(dh_fem_22)
      rownames(dh_fem_22) <- paste('2022', dh_fem_22$SITE_STN, sep = '_')
      dh_fem_22 <- dh_fem_22[,-c(1:3, 26:27)]
    
    ## Combine years
    dh_fem_fnlc <- bind_rows(dh_fem_21, dh_fem_22)
    
    ## Convert to binary
    dh_fem_fnlc[dh_fem_fnlc > 0] <- 1
    
    ## Save
    # write.csv(dh_fem_fnlc, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocFemaleFNLC_left.csv')
    write.csv(dh_fem_fnlc, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocFemaleFNLC_stag.csv')

    
## Female FNLC or Pair ---------------------------------------------------------
    
  ## Use left-justified detection histories for combined years    
    # dh_fem_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocFemaleFNLCorPair_left.csv')
    # dh_fem_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocFemaleFNLCorPair_left.csv')
    
  ## Use staggered-entry detection histories where calendar dates match both years
    dh_fem_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocFemaleFNLCorPair_staggered.csv') #2021 weeks are good
    dh_fem_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocFemaleFNLCorPair_stag_both.csv') #I updated 2022 weeks to match
    
  ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_fem_21 <- dh_fem_21[order(dh_fem_21$SITE_STN),]; dh_fem_21 <- data.frame(dh_fem_21)
      rownames(dh_fem_21) <- paste('2021', dh_fem_21$SITE_STN, sep = '_')
      dh_fem_21 <- dh_fem_21[,-c(1:3, 24:25)]
    dh_fem_22 <- dh_fem_22[order(dh_fem_22$SITE_STN),]; dh_fem_22 <- data.frame(dh_fem_22)
      rownames(dh_fem_22) <- paste('2022', dh_fem_22$SITE_STN, sep = '_')
      dh_fem_22 <- dh_fem_22[,-c(1:3, 26:27)]
    
    ## Combine years
    dh_fem_fnlc_or_pair <- bind_rows(dh_fem_21, dh_fem_22)
    
    ## Convert to binary
    dh_fem_fnlc_or_pair[dh_fem_fnlc_or_pair > 0] <- 1
    
    ## Save
    # write.csv(dh_fem_fnlc_or_pair, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocFemaleFNLCorPair_left.csv')        
    write.csv(dh_fem_fnlc_or_pair, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocFemaleFNLCorPair_stag.csv')
        
    
## Male ------------------------------------------------------------------------
    
  ## Use left-justified detection histories for combined years
    # dh_mal_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocMale_left.csv')
    # dh_mal_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocMale_left.csv')

  ## Use staggered-entry detection histories where calendar dates match both years
    dh_mal_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocMale_staggered.csv') #2021 weeks are good
    dh_mal_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocMale_stag_both.csv') #I updated 2022 weeks to match
    
  ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_mal_21 <- dh_mal_21[order(dh_mal_21$SITE_STN),]; dh_mal_21 <- data.frame(dh_mal_21)
      rownames(dh_mal_21) <- paste('2021', dh_mal_21$SITE_STN, sep = '_')
      dh_mal_21 <- dh_mal_21[,-c(1:3, 24:25)]
    dh_mal_22 <- dh_mal_22[order(dh_mal_22$SITE_STN),]; dh_mal_22 <- data.frame(dh_mal_22)
      rownames(dh_mal_22) <- paste('2022', dh_mal_22$SITE_STN, sep = '_')
      dh_mal_22 <- dh_mal_22[,-c(1:3, 26:27)]
    
  ## Combine years
    dh_mal <- bind_rows(dh_mal_21, dh_mal_22)
    
  ## Convert to binary
    dh_mal[dh_mal > 0] <- 1
    
  ## Save
    # write.csv(dh_mal, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocMale_left.csv')    
    write.csv(dh_mal, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocMale_stag.csv')

    
## Male FNLC -------------------------------------------------------------------
    
  ## Use left-justified detection histories for combined years
  #   dh_mal_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocMaleFNLC_left.csv')
  #   dh_mal_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocMaleFNLC_left.csv')
    
  ## Use staggered-entry detection histories where calendar dates match both years
    dh_mal_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocMaleFNLC_staggered.csv') #2021 weeks are good
    dh_mal_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocMaleFNLC_stag_both.csv') #I updated 2022 weeks to match
    
  ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_mal_21 <- dh_mal_21[order(dh_mal_21$SITE_STN),]; dh_mal_21 <- data.frame(dh_mal_21)
      rownames(dh_mal_21) <- paste('2021', dh_mal_21$SITE_STN, sep = '_')
      dh_mal_21 <- dh_mal_21[,-c(1:3, 24:25)]
    dh_mal_22 <- dh_mal_22[order(dh_mal_22$SITE_STN),]; dh_mal_22 <- data.frame(dh_mal_22)
      rownames(dh_mal_22) <- paste('2022', dh_mal_22$SITE_STN, sep = '_')
      dh_mal_22 <- dh_mal_22[,-c(1:3, 26:27)]
    
    ## Combine years
    dh_mal_fnlc <- bind_rows(dh_mal_21, dh_mal_22)
    
    ## Convert to binary
    dh_mal_fnlc[dh_mal_fnlc > 0] <- 1
    
    ## Save
    # write.csv(dh_mal_fnlc, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocMaleFNLC_left.csv')    
    write.csv(dh_mal_fnlc, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocMaleFNLC_stag.csv')

    
## Male FNLC or Pair------------------------------------------------------------------------
    
  ## Use left-justified detection histories for combined years
    # dh_mal_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocMaleFNLCorPair_left.csv')
    # dh_mal_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocMaleFNLCorPair_left.csv')
    
  ## Use staggered-entry detection histories where calendar dates match both years
    dh_mal_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocMaleFNLCorPair_staggered.csv') #2021 weeks are good
    dh_mal_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocMaleFNLCorPair_stag_both.csv') #I updated 2022 weeks to match
    
    ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_mal_21 <- dh_mal_21[order(dh_mal_21$SITE_STN),]; dh_mal_21 <- data.frame(dh_mal_21)
      rownames(dh_mal_21) <- paste('2021', dh_mal_21$SITE_STN, sep = '_')
      dh_mal_21 <- dh_mal_21[,-c(1:3, 24:25)]
    dh_mal_22 <- dh_mal_22[order(dh_mal_22$SITE_STN),]; dh_mal_22 <- data.frame(dh_mal_22)
      rownames(dh_mal_22) <- paste('2022', dh_mal_22$SITE_STN, sep = '_')
      dh_mal_22 <- dh_mal_22[,-c(1:3, 26:27)]
    
    ## Combine years
    dh_mal_fnlc_or_pair <- bind_rows(dh_mal_21, dh_mal_22)
    
    ## Convert to binary
    dh_mal_fnlc_or_pair[dh_mal_fnlc_or_pair > 0] <- 1
    
    ## Save
    # write.csv(dh_mal_fnlc_or_pair, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocMaleFNLCorPair_left.csv')    
    write.csv(dh_mal_fnlc_or_pair, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocMaleFNLCorPair_stag.csv')
      
    
## Pair ------------------------------------------------------------------------
    
  ## Use left-justified detection histories for combined years
    # dh_pair_21 <- fread('output/08_weekly_dethist_left/08_dh_ac_2021_stocPair_left.csv')
    # dh_pair_22 <- fread('output/08_weekly_dethist_left/08_dh_ac_2022_stocPair_left.csv')
    
  ## Use staggered-entry detection histories where calendar dates match both years
    dh_pair_21 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2021_stocPair_staggered.csv') #2021 weeks are good
    dh_pair_22 <- fread('output/08_weekly_dethist_stag_both/08_dh_ac_2022_stocPair_stag_both.csv') #I updated 2022 weeks to match
    
  ## Make sure sites are sorted alphabetically by hexagon, then delete that column  
    dh_pair_21 <- dh_pair_21[order(dh_pair_21$SITE_STN),]; dh_pair_21 <- data.frame(dh_pair_21)
      rownames(dh_pair_21) <- paste('2021', dh_pair_21$SITE_STN, sep = '_')
      dh_pair_21 <- dh_pair_21[,-c(1:3, 24:25)]
    dh_pair_22 <- dh_pair_22[order(dh_pair_22$SITE_STN),]; dh_pair_22 <- data.frame(dh_pair_22)
      rownames(dh_pair_22) <- paste('2022', dh_pair_22$SITE_STN, sep = '_')
      dh_pair_22 <- dh_pair_22[,-c(1:3, 26:27)]
    
  ## Combine years
    dh_pair <- bind_rows(dh_pair_21, dh_pair_22)
    
  ## Convert to binary
    dh_pair[dh_pair > 0] <- 1
    
  ## Save
    # write.csv(dh_pair, 'output/08_weekly_dethist_left/08_dh_ac_combined_stocPair_left.csv')    
    write.csv(dh_pair, 'output/08_weekly_dethist_stag_both/08_dh_ac_combined_stocPair_stag.csv')
    
