## data prep for 2021 AC "big grid" data

library(data.table)
library(ggplot2)

## read in data from Chris M. ####
  ac21 <- fread('COA_AC_Work/ac_STOC_2021.csv')
  head(ac21)  
  
  
## explore some of the fields ####
  
  #SPECIES
  table(ac21$SO, useNA = 'always')
    #s = SRV1 (2124)
    #u = STOC suspected but not positively ID'ed (e.g., '?' tags) (68)
    #x = authentic STOC (7340)

  #SURVEYS
  table(ac21$SRV, useNA = 'always')
    #whether or not SRV1 was tagged in this clip (2132 -- why not 2124?)
  table(ac21$SRV1KM, useNA = 'always')
  table(ac21$SRV2KM, useNA = 'always')  
    #whether surveys were done within this distance on this night

  #SEX        
  table(ac21$PR, useNA = 'always')
    #whether sex predictions were done? I think?
  
  table(ac21$M, useNA = 'always')  #male (from Chris...?)
  table(ac21$F, useNA = 'always')  #female (from Chris...?) 
  
  table(ac21$M_PI, useNA = 'always')  #male predictions from model (obsolete?)    *were these re-run?
  table(ac21$F_PI, useNA = 'always')  #female predictions from model (obsolete?)  *were these re-run?
  
  #TARGET RESIDENT
  table(ac21$NRES, useNA = 'always')
    #x = these are 'extra' owls detected within the grid
    
  #AC/STUDY AREA NAME
  table(ac21$SRC, useNA = 'always')
    #dc = Drift Creek (COA)
    #mc = Miller (COA)
    #ug = Upper Greenleaf (COA)
    #wc = Waite (COA)
    ## *why do they all have 1 and 2?
  table(ac21$SITE, useNA = 'always')
    #not sure I need the SRC field then
  
  #CALL TYPE
  table(ac21$WHIS_SP, useNA = 'always')
  table(ac21$BEG_SP, useNA = 'always')  
  # table(ac21$MANUAL_ID, useNA = 'always')  
    #whistle and beg tags are here too
    #but I need to pull out more like BARK, STOC_IRREG, and STOC_X2 (pair)
  
    table(ac21$MANUAL_ID == ac21$FULL_ID) #what is the difference between these two fields?
    ac21[ac21$MANUAL_ID != ac21$FULL_ID]
      #this one entry just has an extra (SO_U) tag in the FULL_ID field
      #go with the FULL_ID field
    
      
    
    
    
## do some cleaning ####

  ## extract all call types
    tags <- unlist(sapply(strsplit(as.character(ac21$FULL_ID), '\\+'), '['))
    sort(unique(tags))      ## view all the tags used
    
    ac21$STOC_BARK <- ifelse(grepl('BARK', ac21$FULL_ID), 'Y','N')
    ac21$STOC_WHIS <- ifelse(grepl('WHIS_SO', ac21$FULL_ID), 'Y','N')  #should match the WHIS_SP column
    ac21$STOC_BEG  <- ifelse(grepl('BEG_SO', ac21$FULL_ID), 'Y','N')   #should match the BEG_SP column
    # ac21$STOC_PAIR  <- ifelse(grepl('STOC_X2|ST0C_X2', ac21$FULL_ID), 'Y','N')
    ac21$STOC_PAIR <- ifelse(grepl('(SO_MF)', ac21$FULL_ID), 'Y', 'N')  #go with Chris's tags instead of original STOC_X2
    ac21$STOC_IRREG  <- ifelse(grepl('STOC_IRREG', ac21$FULL_ID), 'Y','N') #should match the PR column
    ac21$STOC_4N <- ifelse(grepl('STOC\\+', ac21$FULL_ID), 'Y','N')
  
    ac21$STVA_IRREG <- ifelse(grepl('STVA_IRREG', ac21$FULL_ID), 'Y','N')
    ac21$STVA_PAIR <- ifelse(grepl('STVA_X2', ac21$FULL_ID), 'Y','N')
    ac21$STVA_BEG <- ifelse(grepl('BEG_SV', ac21$FULL_ID), 'Y','N')
    ac21$STVA_INSP <- ifelse(grepl('INSP', ac21$FULL_ID), 'Y', 'N')
    ac21$STVA_8N <- ifelse(grepl('STVA', ac21$FULL_ID),
                           ifelse((ac21$STVA_IRREG %in% 'N' & ac21$STVA_PAIR %in% 'N' & ac21$STVA_BEG %in% 'N' & ac21$STVA_INSP %in% 'N'),
                           'Y', 'N'), 'N')
    

  ## format datetime; get exact time from offset
    ac21$datetime <- paste(ac21$DATE, 
                           sapply(strsplit(as.character(sapply(strsplit(as.character(ac21$IN_FILE), '\\.'), '[', 1)), '\\_'), '[', 4), 
                           sep = ' ')
    ac21$datetime <- as.POSIXct(strptime(ac21$datetime, '%m/%d/%Y %H%M%S'), tz = 'America/Los_Angeles')
    
    ac21$PART_N <- as.integer(sapply(strsplit(as.character(ac21$PART), '\\_'), '[', 2))
    ac21$timestamp <- ac21$datetime + (12 * (ac21$PART_N - 1)) #could also work with column 'MM_SS' and add that to datetime
    
    
  ## keep only authentic STOC
    table(ac21$SO, useNA = 'always')
    table(ac21$SRV, useNA = 'always') #8 clips had both SRV1 and authentic STOC. keep them
    
    ac21$REMOVE <- ifelse(ac21$SO %in% c('u','s'), 'y','n') #don't keep 'unknown' or 'survey'
    
      #look at tags in clips we are removing
      sort(unique(unlist(sapply(strsplit(as.character(ac21[ac21$REMOVE %in% 'y']$FULL_ID), '\\+'), '['))))
      #note that we are removing some with STVA, STVA_IRREG, or INSP. these were predicted to be STOC by the CNN but were not
      #all we have here are clips predicted to be STOC, and some also have STVA, but we need the full list of STVA predictions
      #  and validated tags to get those detections

    ac21stoc <- ac21[ac21$REMOVE %in% 'n',]  #keep only authentic STOC
    
    
  ## add 'any STOC' column  
    ac21stoc$STOC_ANY <- ifelse() #finish this!!!
    
    
  ## save
    write.csv(ac21stoc, 'output/ac_STOC_2021_cleaned.csv') #latest save 01/17/23



## collapse by station ####
  head(ac21stoc[,c('SITE','STN','timestamp','PR','M','F','U','JV','SV','NRES','SRV1MILE',
                   'STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR','STOC_IRREG','STOC_4N',
                   'STVA_IRREG','STVA_PAIR','STVA_BEG','STVA_8N')])

  ## remove calls by non-residents
    table(ac21stoc$SITE, ac21stoc$NRES)  #I thought there were extra ones in Upper Greenleaf?
    
    ac21stocRes <- ac21stoc[ac21stoc$NRES %in% '',] #keep target owl calls only
    
  ## aggregate by site/stn
    table(ac21stocRes$SITE, ac21stocRes$STN)
    table(ac21stocRes$SITE)    
    table(ac21stocRes$SRC) #what's the difference between dc1 and dc2, etc.?
    
    ac21stocRes$SITE_STN <- paste(ac21stocRes$SRC, ac21stocRes$STN, sep = '_')
      table(ac21stocRes$SITE_STN, useNA = 'always')
    
    # tmp <- aggregate(ac21stocRes, by = list('SITE_STN'), FUN = 'sum')
    
    # tmp2 <- ac21stocRes[,c('SRV1MILE','STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR','STOC_IRREG','STOC_4N',
    #                    'STVA_IRREG','STVA_PAIR','STVA_BEG','STVA_8N','SITE_STN')]
    # head(tmp2)    
    
    #convert Y/N columns to numeric
    ac21stocRes$STOC_BARK_N <- ifelse(ac21stocRes$STOC_BARK %in% 'Y',1,0)
    ac21stocRes$STOC_WHIS_N <- ifelse(ac21stocRes$STOC_WHIS %in% 'Y',1,0)
    ac21stocRes$STOC_BEG_N <- ifelse(ac21stocRes$STOC_BEG %in% 'Y',1,0)
    ac21stocRes$STOC_PAIR_N <- ifelse(ac21stocRes$STOC_PAIR %in% 'Y',1,0)
    ac21stocRes$STOC_IRREG_N <- ifelse(ac21stocRes$STOC_IRREG %in% 'Y',1,0)
    ac21stocRes$STOC_4N_N <- ifelse(ac21stocRes$STOC_4N %in% 'Y',1,0)

    ac21stocRes$STVA_IRREG_N <- ifelse(ac21stocRes$STVA_IRREG %in% 'Y',1,0)
    ac21stocRes$STVA_PAIR_N <- ifelse(ac21stocRes$STVA_PAIR %in% 'Y',1,0)    
    ac21stocRes$STVA_BEG_N <- ifelse(ac21stocRes$STVA_BEG %in% 'Y',1,0)
    ac21stocRes$STVA_INSP_N <- ifelse(ac21stocRes$STVA_INSP %in% 'Y',1,0)
    ac21stocRes$STVA_8N_N <- ifelse(ac21stocRes$STVA_8N %in% 'Y',1,0)
    
    ac21stocRes$MALE_N <- ifelse(ac21stocRes$M %in% 'x', 1,0)
    #add F, etc.
    
    #aggregate
      ac21agg <- aggregate(ac21stocRes[,c('STOC_BARK_N','STOC_WHIS_N','STOC_BEG_N','STOC_PAIR_N','STOC_IRREG_N','STOC_4N_N',
                                          'STVA_IRREG_N','STVA_PAIR_N','STVA_BEG_N','STVA_INSP_N','STVA_8N_N')],
                           by = list(as.factor(ac21stocRes$SITE_STN)), 
                           FUN = sum)
      colnames(ac21agg)[1] <- 'SITE_STN'
      head(ac21agg)

      ## keep in mind these can sum to greater than the number of clips... e.g., a clip can have STOC_BEG and STOC_4N
      ## so can't add these up to get STOC_ANY for a station
      ## go back and add 'STOC_ANY' field separately
      
      