## Data prep for 2022 AC "big grid" data

library(data.table)
library(ggplot2)


## PART 1: DETECTION DATA ######################################################

## Read in validated clip data from Chris M. -----------------------------------
  ac22 <- fread('2022/ac_stoc_2022.csv')
  head(ac22)  
  

## Explore some of the fields --------------------------------------------------
  
  #SPECIES
  table(ac22$SO, useNA = 'always')
    #u (STOC suspected but not positively ID'ed, e.g., '?' tags) = 71
    #x (authentic STOC) = 9683

  #SURVEYS
  table(ac22$SRV, useNA = 'always')
    #whether or not SRV1 was tagged in this clip
    #Chris said there were no surveys to be worried about in 2022 -- good

  #SEX        
  table(ac22$PR, useNA = 'always')
    #whether sex predictions were done? I think?
  
    #sex as tagged by Chris (not model predictions):
    table(ac22$M, useNA = 'always')  #male = 3863
    table(ac22$F, useNA = 'always')  #female = 5124
    table(ac22$FF, useNA = 'always') #2 females = 89
    table(ac22$U, useNA = 'always')  #unknown = 1327
    table(ac22$PR, useNA = 'always') #pair = 631
    table(ac22$JV, useNA = 'always') #juv = 0?
    table(ac22$SV, useNA = 'always') #barred owl = 65

  #TARGET RESIDENT OR NOT
  table(ac22$RES, useNA = 'always')  
    #x (clip contains detections of the focal owl/pair) = 7578
    
  table(ac22$NRES, useNA = 'always')
    #x (clip contains detections of 'extra' owls, not the focal pair) = 2194
  
  table(ac22$NRES2, useNA = 'always')
    #P (clip has detections of primary owls) = 7489
    #S (clip has detections of secondary owls) = 2105
    #B (clip has detectoins of both) = 89
  
    nrow(ac22[ac22$RES %in% 'x' & ac22$NRES2 %in% 'P',]) + #we want these ones
     nrow(ac22[ac22$RES %in% 'x' & ac22$NRES2 %in% 'B',])  #and these ones
    
  #AC/STUDY AREA NAME
  table(ac22$SITE, useNA = 'always')
    #bc = Baker Creek (CLE)
    #cc = Cummins Creek (COA)
    #dc = Drift Creek (COA)
    #lm = Limpy Mtn (UMPQUA)
  table(ac22$SRC, useNA = 'always')
    #I think 1 and 2 refer to the hard drives?
  
  #CALL TYPE
  table(ac22$WHIS_SP, useNA = 'always') #592 WHIS were spotted owl, 0 barred owl
  table(ac22$BEG_SP, useNA = 'always')  #0 BEG were spotted owl, 3 barred owl
  # table(ac22$MANUAL_ID, useNA = 'always')
    #whistle and beg tags are here too
    #but I will also pull out more like BARK, STOC_IRREG, and STOC_X2 (pair)
  
  #this sheet doesn't have 'FULL_ID' field like 2021, so use 'MANUAL ID' instead


## Clean up call types----------------------------------------------------------

  #extract all tags
  tags <- unlist(sapply(strsplit(as.character(ac22$MANUAL_ID), '\\+'), '['))
    sort(unique(tags))      ## view all the tags used
    
  #create individual columns for the call types
  ac22$STOC_BARK <- ifelse(grepl('BARK', ac22$MANUAL_ID), 1, 0)
  ac22$STOC_WHIS <- ifelse(ac22$WHIS_SP %in% 'SO', 1, 0) #don't use grepl bc would get both WHIS_SO and WHIS_SV (although none this year)
  ac22$STOC_BEG  <- ifelse(ac22$BEG_SP %in% 'SO', 1, 0)  #see above re: grepl
  ac22$STOC_PAIR <- ifelse(grepl('(SO_MF)', ac22$MANUAL_ID), 1, 0)  
  ac22$STOC_PAIR_MFF <- ifelse(grepl('STOC_X3', ac22$MANUAL_ID), 1, 0) #only 1 -- same as (SO_MFF)
  ac22$STOC_IRREG  <- ifelse(grepl('STOC_IRREG', ac22$MANUAL_ID), 1, 0)
  ac22$STOC_4N <- ifelse(grepl('STOC\\+', ac22$MANUAL_ID), 1, 0)
    
    #use sex columns 'F','M','U' or extract tags as below? why don't they match?
    # ac22$female <- ifelse(grepl('(SO_F)', ac22$MANUAL_ID), 'Y','N')
      #manual tags are more specific; columns F, M, U are more comprehensive
      #e.g., clip with manual tag '(SO_MF)' will have both columns 'M' and 'F' checked
      #which to use? 
        #go with the more comprehensive ones for now ('F','M','U')
    
    #don't pull out STVA, BUVI, etc. here -- this dataset has only clips with predicted STOC
    
  #convert male/female columns to numeric (1/0)
  ac22$MALE <- ifelse(ac22$M %in% 'x', 1,0)
  ac22$FEMALE <- ifelse(ac22$F %in% 'x', 1,0)
  ac22$FEMALE2 <- ifelse(ac22$FF %in% 'x',1,0)
  ac22$UNK <- ifelse(ac22$U %in% 'x', 1,0)
  ac22$JUV <- ifelse(ac22$JV %in% 'x', 1,0)  
  

## Format datetimes (get exact time from offset) -------------------------------
  ac22$datetime <- paste(ac22$DATE, 
                         sapply(strsplit(as.character(sapply(strsplit(as.character(ac22$IN_FILE), '\\.'), '[', 1)), '\\_'), '[', 4), 
                         sep = ' ')
  ac22$datetime <- as.POSIXct(strptime(ac22$datetime, '%m/%d/%Y %H%M%S'), tz = 'America/Los_Angeles')
  
  ac22$PART_N <- as.integer(sapply(strsplit(as.character(ac22$PART), '\\_'), '[', 2))
  ac22$timestamp <- ac22$datetime + (12 * (ac22$PART_N - 1)) #could also work with column 'MM_SS' and add that to datetime
    

## Add SITE_STN column ---------------------------------------------------------
  table(ac22$SITE, ac22$STN)
  table(ac22$SITE, useNA = 'always')    
  
  ac22$SITE_STN <- paste(toupper(substr(ac22$SRC, 1,2)), 
                         formatC(ac22$STN, width = '2', format = 'd', flag = '0'), #use formatC to pad with leading zeroes
                         sep = '_') 
  sort(unique(ac22$SITE_STN, useNA = 'always'))
  
    
## Keep only authentic STOC ----------------------------------------------------
  table(ac22$SO, useNA = 'always')  #71 'uncertain' ones to remove
  table(ac22$SRV, useNA = 'always') #no surveys to remove from 2022
    
  ac22$REMOVE <- ifelse(ac22$SO %in% c('u'), 'y','n') #don't keep 'unknown'
    table(ac22$REMOVE, useNA = 'always')
    
    #look at tags in clips we are removing just to be sure:
    sort(unique(unlist(sapply(strsplit(as.character(ac22[ac22$REMOVE %in% 'y']$MANUAL_ID), '\\+'), '['))))
    
  #keep only authentic STOC    
  ac22stoc <- ac22[ac22$REMOVE %in% 'n',] 

  #add 'any STOC' column  
  ac22stoc$STOC_ANY <- 1

  
## Keep only focal pair detections ---------------------------------------------
  table(ac22stoc$SITE, ac22stoc$RES, useNA = 'always')
  table(ac22stoc$SITE, ac22stoc$NRES2, useNA = 'always')
    
  ac22stocRes <- ac22stoc[ac22stoc$RES %in% 'x',]  #or could keep rows where NRES2 %in% c('P','B')  
    
 
## Aggregate by site/station ---------------------------------------------------
  ac22agg <- aggregate(ac22stocRes[,c('STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR','STOC_PAIR_MFF','STOC_IRREG',
                                      'STOC_4N','MALE','FEMALE','FEMALE2','UNK','JUV','STOC_ANY')],
                       by = list(as.factor(ac22stocRes$SITE_STN)), FUN = sum)
  colnames(ac22agg)[1] <- 'SITE_STN'
  ac22agg  #55 rows = 55 stations with STOC detections

    ## keep in mind these can sum to greater than the number of clips... e.g., a clip can have STOC_BEG and STOC_4N
    ## but the 'STOC_ANY_N' column is the total number of clips with any of these species
    ## (but all STOC columns do not add up to 'STOC_ANY', e.g.)
    
  head(ac22agg[,c('SITE_STN','STOC_4N','STOC_ANY')])
      

## Save ------------------------------------------------------------------------
  write.csv(ac22stoc, 'output/01_ac_STOC_2022_cleaned.csv') #latest save 04/07/23
  write.csv(ac22stocRes, 'output/02_ac_STOC_2022_residents.csv') #latest save 04/07/23
  write.csv(ac22agg, 'output/03_ac_STOC_2022_aggregated.csv')   #latest save 04/07/23
  
  #2022 has no surveys so I won't have an '03_..._noSurveys' dataframe
  
  
################################################################################  

## PART 2: EFFORT AND DISTANCE DATA ############################################    
  
  # ac22agg <- fread('output/ac_STOC_2022_aggregated.csv') #if necessary  
  
  
## Read in Station_info files --------------------------------------------------
  infoFiles <- list.files('2022/AC_2022_STATION_INFO/', full.names = TRUE)
  mylist <- lapply(infoFiles, fread, header = TRUE)
  stnInfo <- rbindlist(mylist, fill = TRUE)
    head(stnInfo)
    sort(unique(stnInfo$`Station ID`, useNA = 'always'))
    
    
## Format SITE_STN column ------------------------------------------------------
  stnInfo$SITE <- ifelse(grepl('Baker', stnInfo$`Station ID`), 'BC',
                         ifelse(grepl('COA_CC', stnInfo$`Station ID`), 'CC',
                                ifelse(grepl('COA_DC', stnInfo$`Station ID`), 'DC',
                                       ifelse(grepl('UMP_LM', stnInfo$`Station ID`), 'LM', NA))))
  table(stnInfo$SITE, useNA = 'always')

  stnInfo$STN <- sapply(strsplit(as.character(stnInfo$`Station ID`), '\\-'), '[', 2)
  stnInfo$SITE_STN <- as.factor(paste(stnInfo$SITE, stnInfo$STN, sep = '_'))


## Format dates and add duration -----------------------------------------------
  stnInfo$Earliest <- as.POSIXct(strptime(stnInfo$Earliest, '%m/%d/%y'), tz = 'America/Los_Angeles')
  stnInfo$Latest <- as.POSIXct(strptime(stnInfo$Latest, '%m/%d/%y'), tz = 'America/Los_Angeles')
  
  stnInfo$duration <- difftime(stnInfo$Latest, stnInfo$Earliest)
  stnInfo$nDays <- interval(stnInfo$Earliest, stnInfo$Latest) / days(1) 
    #looks like they are the same (did we do this a different way in 2021?)
    #could also look at number of wav files?
  
  
## Merge -----------------------------------------------------------------------
  head(ac22agg)
  head(stnInfo)

  ac22merge <- merge(ac22agg, stnInfo[,c('SITE_STN','Earliest','Latest','duration')], by = c('SITE_STN'), all = TRUE)
      
    nrow(ac22agg)  
    nrow(stnInfo)  
    nrow(ac22merge) #good, should be 146 rows: 4 sites * 37 stations (minus 2 in CC bc it's at the coast)
    
    
## Add repro status of each site -----------------------------------------------
  ac22merge$reproState <- ifelse(grepl('DC', ac22merge$SITE_STN), 'pair',
                                 ifelse(grepl('CC', ac22merge$SITE_STN), 'female',
                                        ifelse(grepl('BC', ac22merge$SITE_STN), 'pair', 'pair')))
    table(ac22merge$reproState, useNA = 'always')
    
    ## **DOUBLE CHECK WITH CHRIS ON THESE; THEY ARE TRICKIER FOR 2022**
  
    
## Add distances as measured in GIS --------------------------------------------
  
  #read in attribute tables from ArcGIS ('distance' field is the distance from center for each point)
  dist_dc <- fread('2022/distances/drift_creek_2022_distances.csv',
                   select = c('ac_st','utmx_new','utmy_new','Distance')); dist_dc$site <- 'dc'
  dist_cc <- fread('2022/distances/cummins_creek_2022_distances.csv',
                   select = c('ac_st','utmx_new','utmy_new','Distance')); dist_cc$site <- 'cc'
  dist_bc <- fread('2022/distances/baker_creek_2022_distances.csv',
                   select = c('ac_st','utmx_new','utmy_new','Distance')); dist_bc$site <- 'bc'
  dist_lm <- fread('2022/distances/limpy_mtn_2022_distances.csv',
                   select = c('ac_st','utmx_new','utmy_new','Distance')); dist_lm$site <- 'lm'
  
  #combine and add column for matching
  distances <- rbind(dist_dc, dist_cc); distances <- rbind(distances, dist_bc); distances <- rbind(distances, dist_lm)
  distances$stn <- sapply(strsplit(as.character(distances$ac_st), '\\-'), '[', 2)
  distances$SITE_STN <- paste(toupper(distances$site), 
                             formatC(as.numeric(distances$stn), width = '2', format = 'd', flag = '0'), sep = '_')
  
  #merge with rest of the data
  ac22merge_dist <- merge(ac22merge, distances[,c('SITE_STN','Distance','utmx_new','utmy_new')], 
                          by = c('SITE_STN'), all = TRUE)
  
    nrow(ac22merge) ; length(unique(ac22merge$SITE_STN)) #146 rows, 146 unique sites
    nrow(distances) ; length(unique(distances$SITE_STN)) #145 rows, 146 unique sites 
    nrow(ac22merge_dist); length(unique(ac22merge_dist$SITE_STN)) #146 rows, 146 unique sites
    
      ## I made some changes manually to the shapefiles to make them match above:
        # - Changed name of LM_03 to LM_06 (there were 2 named LM_03 accidentally)
        # - Added CC_38 to shapefile using deployment/retrieval form and switched names of 20 and 38
            ## CC_38 was the original placement at CC_20; wasn't great but was left out the whole season (5/10/22 - 9/1/22)
            ## CC_20 was the redo; was also left out the whole season (5/14/22 - 9/1/23)
            ## **Do we want to use data from both stations? We'll keep it in for now**
      
  
## Add intended distances ------------------------------------------------------
  ac22merge_dist$dist_intended <- ifelse(grepl('_12|_13|_18|_20|_25|_26|_38', ac22merge_dist$SITE_STN), as.numeric(1000), NA)
  ac22merge_dist$dist_intended <- ifelse(grepl('_07|_11|_14|_24|_27|_31',  ac22merge_dist$SITE_STN), as.numeric(1732), ac22merge_dist$dist_intended)
  ac22merge_dist$dist_intended <- ifelse(grepl('_06|_08|_17|_21|_30|_32', ac22merge_dist$SITE_STN), as.numeric(2000), ac22merge_dist$dist_intended)
  ac22merge_dist$dist_intended <- ifelse(grepl('_02|_03|_05|_09|_10|_15|_23|_28|_29|_33|_35|_36', ac22merge_dist$SITE_STN), as.numeric(2646), ac22merge_dist$dist_intended)
  ac22merge_dist$dist_intended <- ifelse(grepl('_01|_04|_16|_22|_34|_37', ac22merge_dist$SITE_STN), as.numeric(3000), ac22merge_dist$dist_intended)
  ac22merge_dist$dist_intended <- ifelse(grepl('19', ac22merge_dist$SITE_STN), as.numeric(0), ac22merge_dist$dist_intended)

    table(ac22merge_dist$dist_intended, useNA = 'always')
    plot(ac22merge_dist$dist_intended, ac22merge_dist$Distance) #did I match up the intended distances correctly? yes
    
  plot(ac22merge_dist$STOC_ANY ~ ac22merge_dist$Distance) #any STOC
  plot(ac22merge_dist$STOC_4N ~ ac22merge_dist$Distance)  #4NLC STOC
  
  
  #save
    write.csv(ac22merge_dist, 'output/05_ac_2022_merged.csv')  #output 04/07/23
    
    