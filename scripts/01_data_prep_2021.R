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
      #note that we are removing some with STVA, STVA_IRREG, or INSP. these were predicted to be STOC by the CNN but were not.
      #all we have here are clips predicted to be STOC, and some also have STVA, but we need the full list of STVA predictions
      #  and validated tags to get those detections

    ac21stoc <- ac21[ac21$REMOVE %in% 'n',]  #keep only authentic STOC
    
    
  ## add 'any STOC' column  
    ac21stoc$STOC_ANY <- 'Y'
      table(ac21stoc$SO, useNA = 'always')  #should be all of them at this point; double check
      
    ac21stoc$STVA_ANY <- ifelse((ac21stoc$STVA_8N %in% 'Y' | ac21stoc$STVA_IRREG %in% 'Y' | ac21stoc$STVA_BEG %in% 'Y' |
                                  ac21stoc$STVA_PAIR %in% 'Y' | ac21stoc$STVA_INSP %in% 'Y'), 'Y', 'N')
    
  ## save
    # write.csv(ac21stoc, 'output/01_ac_STOC_2021_cleaned.csv') #latest save 01/19/23



## clean up, remove non-residents, remove clips with nearby surveys ####
    
  # ac21stoc <- fread('output/ac_STOC_2021_cleaned.csv')  #if needed
    
  head(ac21stoc[,c('SITE','STN','timestamp','PR','M','F','U','JV','SV','NRES','SRV1MILE',
                   'STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR','STOC_IRREG','STOC_4N',
                   'STVA_IRREG','STVA_PAIR','STVA_BEG','STVA_8N', 'STOC_ANY','STVA_ANY')])

  ## remove calls by non-residents
    table(ac21stoc$SITE, ac21stoc$NRES)  
    ac21stocRes <- ac21stoc[ac21stoc$NRES %in% '',] #keep target owl calls only
    
  ## clean up fields
    table(ac21stocRes$SITE, ac21stocRes$STN)
    table(ac21stocRes$SITE)    
    table(ac21stocRes$SRC) #what's the difference between dc1 and dc2, etc.?
    
    #add site_stn column
      ac21stocRes$SITE_STN <- paste(ac21stocRes$SRC, formatC(ac21stocRes$STN, width = '2', format = 'd', flag = '0'),  #formatC to pad with leading zeroes
                                    sep = '_')
        table(ac21stocRes$SITE_STN, useNA = 'always')
    
    #convert Y/N columns to numeric (1/0)
    ac21stocRes$STOC_BARK_N <- ifelse(ac21stocRes$STOC_BARK %in% 'Y',1,0)
    ac21stocRes$STOC_WHIS_N <- ifelse(ac21stocRes$STOC_WHIS %in% 'Y',1,0)
    ac21stocRes$STOC_BEG_N <- ifelse(ac21stocRes$STOC_BEG %in% 'Y',1,0)
    ac21stocRes$STOC_PAIR_N <- ifelse(ac21stocRes$STOC_PAIR %in% 'Y',1,0)
    ac21stocRes$STOC_IRREG_N <- ifelse(ac21stocRes$STOC_IRREG %in% 'Y',1,0)
    ac21stocRes$STOC_4N_N <- ifelse(ac21stocRes$STOC_4N %in% 'Y',1,0)
    ac21stocRes$STOC_ANY_N <- ifelse(ac21stocRes$STOC_ANY %in% 'Y',1,0)

    ac21stocRes$STVA_IRREG_N <- ifelse(ac21stocRes$STVA_IRREG %in% 'Y',1,0)
    ac21stocRes$STVA_PAIR_N <- ifelse(ac21stocRes$STVA_PAIR %in% 'Y',1,0)    
    ac21stocRes$STVA_BEG_N <- ifelse(ac21stocRes$STVA_BEG %in% 'Y',1,0)
    ac21stocRes$STVA_INSP_N <- ifelse(ac21stocRes$STVA_INSP %in% 'Y',1,0)
    ac21stocRes$STVA_8N_N <- ifelse(ac21stocRes$STVA_8N %in% 'Y',1,0)
    ac21stocRes$STVA_ANY_N <- ifelse(ac21stocRes$STVA_ANY %in% 'Y',1,0)
    
    ac21stocRes$MALE_N <- ifelse(ac21stocRes$M %in% 'x', 1,0)
    ac21stocRes$FEMALE_N <- ifelse(ac21stocRes$F %in% 'x', 1,0)
    ac21stocRes$UNK_N <- ifelse(ac21stocRes$U %in% 'x', 1,0)
    ac21stocRes$JUV_N <- ifelse(ac21stocRes$JV %in% 'x', 1,0)
    
    #save
    # write.csv(ac21stocRes, 'output/02_ac_STOC_2021_residents.csv')  #last output 1/13/23
    
  ## remove detections within 1 mile of surveys  ***RETURN HERE TO MODIFY REMOVING DETECTIONS WITH SURVEYS WITHIN X DISTANCE**
    table(ac21stocRes$SRV1MILE, useNA = 'always')
    ac21stocResNosurv1m <- ac21stocRes[ac21stocRes$SRV1MILE %in% 'N',]  #removed 652 rows
    
    #save
    # write.csv(ac21stocResNosurv1m, 'output/03_ac_STOC_2021_noSurveys1mile.csv')
  

  
## aggregate by site/stn ####
    
  # ac21stocResNosurv1m <- fread('output/03_ac_STOC_2021_noSurveys1mile.csv') #if needed
    
  ac21agg <- aggregate(ac21stocResNosurv1m[,c('STOC_BARK_N','STOC_WHIS_N','STOC_BEG_N','STOC_PAIR_N','STOC_IRREG_N','STOC_4N_N','STOC_ANY_N',
                                      'STVA_IRREG_N','STVA_PAIR_N','STVA_BEG_N','STVA_INSP_N','STVA_8N_N','STVA_ANY_N',
                                      'MALE_N','FEMALE_N','UNK_N','JUV_N')],
                       by = list(as.factor(ac21stocResNosurv1m$SITE_STN)), 
                       FUN = sum)
  colnames(ac21agg)[1] <- 'SITE_STN'
  ac21agg  #45 rows = 45 stations with STOC detections

    ## keep in mind these can sum to greater than the number of clips... e.g., a clip can have STOC_BEG and STOC_4N
    ## but the 'STOC_ANY_N' and 'STVA_ANY_N' columns are the total number of clips with any of these species
    ## (but all STOC columns do not add up to 'STOC_ANY', e.g.)
    
  head(ac21agg[,c('SITE_STN','STOC_4N_N','STOC_ANY_N','STVA_8N_N','STVA_ANY_N')])
      
  #save
    # write.csv(ac21agg, 'output/04_ac_STOC_2021_aggregated.csv')
    
    
## integrate stn-level data with Julie's outputs ####
    
  # ac21agg <- fread('output/ac_STOC_2021_aggregated.csv')
  
  #read in cleaned file from Julie  
  ac21jj <- fread('COA_AC_Work/data_output/COAAC_21_strixSeason_wide.csv')
  head(ac21jj)    
    #this dataframe has all the stations (not just ones with STOC detected) so we can get STVA totals, days surveyed, etc.
  
  #reformat columns to merge
  ac21jj$SITESTN <- paste(substr(ac21jj$site, 5,6), sapply(strsplit(as.character(ac21jj$site), '\\-'),'[',2), sep = '_')
  unique(ac21jj$SITESTN)    
    
  ac21agg$SITESTN <- paste(toupper(substr(ac21agg$SITE_STN, 1, 2)),
                     substr(ac21agg$SITE_STN, 5, 6), sep = '_')
    
  #merge
  ac21merge <- merge(ac21agg, ac21jj, by = c('SITESTN'), all = TRUE)
  head(ac21merge)    
  
  nrow(ac21agg)  
  nrow(ac21jj)
  nrow(ac21merge) #good, should be 145 rows (to match ac21jj) ... 37 stations * 4 sites (minus 3 at WC)
  
  #keep STVA columns from Julie's; STOC columns from mine (bc it has Chris's final M/F determinations)
  ac21merge_trim <- ac21merge[,c('SITESTN','minDate','maxDate','nDaysSampled','nDays',
                                 'STOC_ANY_N','STOC_4N_N','STOC_IRREG_N','STOC_BARK_N','STOC_WHIS_N','STOC_BEG_N','STOC_PAIR_N',
                                 'FEMALE_N','MALE_N','UNK_N','JUV_N','STVA_all','INSP','WHIS_bo','BEG__bo')]
  head(ac21merge_trim)
  
  #add repro status of each site
  ac21merge_trim$reproState <- ifelse(grepl('DC', ac21merge_trim$SITESTN), 'fledged',
                                      ifelse(grepl('MC', ac21merge_trim$SITESTN), 'nest',
                                             ifelse(grepl('UG', ac21merge_trim$SITESTN), 'fledged', 'pair')))
  
  
## add distances as measured in GIS to stn-level outputs ####
  
  #read in attribute tables from ArcGIS ('distance' field is the distance in m from center for each point)
  dist_dc <- fread('COA_AC_Work/distances/drift_creek_2021_distances.csv', 
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_dc$site <- 'DC'
  dist_mc <- fread('COA_AC_Work/distances/miller_creek_2021_distances.csv',
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_mc$site <- 'MC'
  dist_ug <- fread('COA_AC_Work/distances/upper_greenleaf_2021_distances.csv',
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_ug$site <- 'UG'
  dist_wc <- fread('COA_AC_Work/distances/waite_creek_2021_distances.csv',
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_wc$site <- 'WC'
  
  #combine and add column for matching
  distances <- rbind(dist_dc, dist_mc); distances <- rbind(distances, dist_ug); distances <- rbind(distances, dist_wc)
  distances$SITESTN <- paste(distances$site, formatC(distances$STATION, width = '2', format = 'd', flag = '0'), sep = '_')
  
  #merge with rest of the data
  ac21merge_trim_dist <- merge(ac21merge_trim, distances[,c('SITESTN','Distance','XNAD83FP','YNAD83FP')], 
                               by = c('SITESTN'), all = TRUE)
  
  #add intended distance from center for each station (**but we probably want the actual distances above for models**)
  ac21merge_trim_dist$dist_intended <- ifelse(grepl('_12|_13|_18|_20|_25|_26', ac21merge_trim_dist$SITESTN), as.numeric(1000), NA)
  ac21merge_trim_dist$dist_intended <- ifelse(grepl('_07|_11|_14|_24|_27|_31',  ac21merge_trim_dist$SITESTN), as.numeric(1732), ac21merge_trim_dist$dist_intended)
  ac21merge_trim_dist$dist_intended <- ifelse(grepl('_06|_08|_17|_21|_30|_32', ac21merge_trim_dist$SITESTN), as.numeric(2000), ac21merge_trim_dist$dist_intended)
  ac21merge_trim_dist$dist_intended <- ifelse(grepl('_02|_03|_05|_09|_10|_15|_23|_28|_29|_33|_35|_36', ac21merge_trim_dist$SITESTN), as.numeric(2646), ac21merge_trim_dist$dist_intended)
  ac21merge_trim_dist$dist_intended <- ifelse(grepl('_01|_04|_16|_22|_34|_37', ac21merge_trim_dist$SITESTN), as.numeric(3000), ac21merge_trim_dist$dist_intended)
  ac21merge_trim_dist$dist_intended <- ifelse(grepl('19', ac21merge_trim_dist$SITESTN), as.numeric(0), ac21merge_trim_dist$dist_intended)

  table(ac21merge_trim_dist$dist_intended, useNA = 'always')
  
  plot(ac21merge_trim_dist$STOC_ANY_N ~ ac21merge_trim_dist$Distance) #any STOC
  plot(ac21merge_trim_dist$STOC_4N_N ~ ac21merge_trim_dist$Distance)  #all STOC
  
  plot(ac21merge_trim_dist$STVA_all ~ ac21merge_trim_dist$Distance)   #any STVA
  
  
  #save
    # write.csv(ac21merge_trim, 'output/05_ac_2021_merged.csv')  #output 1/25/23 with actual distances

    

  
  