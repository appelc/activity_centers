## data prep for 2021 AC "big grid" data

library(data.table)
library(ggplot2)

## read in data from Chris
  ac21 <- fread('COA_AC_Work/ac_STOC_2021.csv')
  head(ac21)  
  
## explore some of the fields
  
  #SPECIES
  table(ac21$SO, useNA = 'always')
    #s = SRV1
    #u = STOC suspected but not positively ID'ed (e.g., '?' tags)
    #x = authentic STOC

  #SURVEYS
  table(ac21$SRV, useNA = 'always')
    #whether or not SRV1 was tagged in this clip
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
    
      
## do some cleaning  

      
  ## format datetime; get exact time from offset
  
  
  ## extract all call types
    
    tags <- unlist(sapply(strsplit(as.character(ac21$FULL_ID), '\\+'), '['))
    sort(unique(tags))      ## view all the tags used
    
  # ac21$CALLTYPE <- ifelse(grepl('WHIS_SO', ac21$FULL_ID), 'WHIS', NA)
  # ac21$CALLTYPE <- ifelse(grepl('BARK', ac21$FULL_ID), 'BARK', ac21$CALLTYPE)
    #this isn't going to work bc each clip can have multiple call types (e.g., STOC + BEG)
    #and I'll be right back here with combined tags (e.g., 'STOC+BEG')
    #so go back to individual columns after all. then we can convert wide to long later?

  ac21$STOC_BARK <- ifelse(grepl('BARK', ac21$FULL_ID), 'Y','N')
  ac21$STOC_WHIS <- ifelse(grepl('WHIS_SO', ac21$FULL_ID), 'Y','N')  #should match the WHIS_SP column
  ac21$STOC_BEG  <- ifelse(grepl('BEG_SO', ac21$FULL_ID), 'Y','N')   #should match the BEG_SP column
  ac21$STOC_PAIR  <- ifelse(grepl('STOC_X2|ST0C_X2', ac21$FULL_ID), 'Y','N')
  ac21$STOC_IRREG  <- ifelse(grepl('STOC_IRREG', ac21$FULL_ID), 'Y','N')
  # ac21$STOC_TERR <-  ##how to do the 4-note hoots? can't just grep for "STOC" ...
  
  ac21$STVA_IRREG <- ifelse(grepl('STVA_IRREG', ac21$FULL_ID), 'Y','N')
  ac21$STVA_PAIR <- ifelse(grepl('STVA_X2', ac21$FULL_ID), 'Y','N')
  # ac21$STVA_TERR <- ##how to do the 8-note hoots? can't just grep for "STOC" ...
  ac21$STVA_BEG <- ifelse(grepl('BEG_SV', ac21$FULL_ID), 'Y','N')

  
  
  
  ## keep only authentic STOC (but we also want STVA too, actually, so maybe just get rid of surveys here?)
      # ac21stoc <- ac21[ac21$SO %in% 'x',]
      # table(ac21stoc$SRV, useNA = 'always')
      # #hmm, 8 of these did have SRV1 tagged
      # 
      # ac21stoc[ac21stoc$SRV %in% 'x',]
      # #ok, these clips had both surveys AND authentic STOC. I'll keep them