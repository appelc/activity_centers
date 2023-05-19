## Data prep for 2021 AC "big grid" data

library(data.table)
library(lubridate)
library(ggplot2)


## PART 1: DETECTION DATA ######################################################

## Read in validation clip data from Chris M. ----------------------------------
  ac21 <- fread('2021/COA_AC_Work/ac_STOC_2021.csv')
  head(ac21)  
  
  
## Explore some of the fields --------------------------------------------------
  
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
    
    
## Clean up call types----------------------------------------------------------

  #extract all call types
  tags <- unlist(sapply(strsplit(as.character(ac21$FULL_ID), '\\+'), '['))
    sort(unique(tags))      ## view all the tags used
    
  #create individual columns for the call types
    ac21$STOC_BARK <- ifelse(grepl('BARK', ac21$FULL_ID), 1, 0)
    ac21$STOC_WHIS <- ifelse(grepl('WHIS_SO', ac21$FULL_ID), 1, 0)  #should match the WHIS_SP column
    ac21$STOC_BEG  <- ifelse(grepl('BEG_SO', ac21$FULL_ID), 1, 0)   #should match the BEG_SP column
    ac21$STOC_PAIR <- ifelse(grepl('(SO_MF)', ac21$FULL_ID), 1, 0)  #go with Chris's tags instead of original STOC_X2
    ac21$STOC_IRREG  <- ifelse(grepl('STOC_IRREG', ac21$FULL_ID), 1, 0) #should match the PR column
    ac21$STOC_4N <- ifelse(grepl('STOC\\+', ac21$FULL_ID), 1, 0)

  #convert male/female columns to numeric (1/0)
    ac21$MALE <- ifelse(ac21$M %in% 'x', 1,0)
    ac21$FEMALE <- ifelse(ac21$F %in% 'x', 1,0)
    ac21$UNK <- ifelse(ac21$U %in% 'x', 1,0)
    ac21$JUV <- ifelse(ac21$JV %in% 'x', 1,0)  
    
  #differentiate 4-note location call (FNLC) and irregular for female, male
    ac21$FEMALE_FNLC <- ifelse(ac21$FEMALE == 1 & ac21$STOC_4N == 1, 1, 0)
    ac21$MALE_FNLC   <- ifelse(ac21$MALE == 1 & ac21$STOC_4N == 1, 1, 0)
    
  #also include detections of pairs
    ac21$FEMALE_FNLC_OR_PAIR <- ifelse(ac21$FEMALE_FNLC == 1 | ac21$STOC_PAIR == 1, 1, 0)
    ac21$MALE_FNLC_OR_PAIR <- ifelse(ac21$MALE_FNLC == 1 | ac21$STOC_PAIR == 1, 1, 0)
    
      #how do these look?
      table(ac21$FEMALE)              #1531 clips with any F
      table(ac21$FEMALE_FNLC_OR_PAIR) #1141 clips with F FNLC or pair
      table(ac21$FEMALE_FNLC)         #683 clips with F FNLC
      
      table(ac21$MALE)              #5184 clips with any F
      table(ac21$MALE_FNLC_OR_PAIR) #3292 clips with F FNLC or pair
      table(ac21$MALE_FNLC)         #2834 clips with F FNLC
      
          
## Format datetime; get exact time from offset ---------------------------------
    ac21$datetime <- paste(ac21$DATE, 
                           sapply(strsplit(as.character(sapply(strsplit(as.character(ac21$IN_FILE), '\\.'), '[', 1)), '\\_'), '[', 4), 
                           sep = ' ')
    ac21$datetime <- as.POSIXct(strptime(ac21$datetime, '%m/%d/%Y %H%M%S'), tz = 'America/Los_Angeles')
    
    ac21$PART_N <- as.integer(sapply(strsplit(as.character(ac21$PART), '\\_'), '[', 2))
    ac21$timestamp <- ac21$datetime + (12 * (ac21$PART_N - 1)) #could also work with column 'MM_SS' and add that to datetime
    

## Add SITE_STN column ---------------------------------------------------------
    table(ac21$SRC, ac21$STN)
    
    ac21$SITE_STN <- paste(toupper(substr(ac21$SRC, 1, 2)),
                           formatC(ac21$STN, width = '2', format = 'd', flag = '0'), sep = '_')
    
    
## Keep only authentic STOC ----------------------------------------------------
    table(ac21$SO, useNA = 'always')  #68 'uncertain' ones to remove
    table(ac21$SRV, useNA = 'always') #8 clips had both SRV1 and authentic STOC. keep them
    
    ac21$REMOVE <- ifelse(ac21$SO %in% c('u','s'), 'y','n') #don't keep 'unknown' or 'survey'
    ac21stoc <- ac21[ac21$REMOVE %in% 'n',]  #keep only authentic STOC    
    
      #look at tags in clips we are removing
      sort(unique(unlist(sapply(strsplit(as.character(ac21[ac21$REMOVE %in% 'y']$FULL_ID), '\\+'), '['))))
    
  ## add 'any STOC' column  
    ac21stoc$STOC_ANY <- 1
      table(ac21stoc$SO, useNA = 'always')  #should be all of them at this point; double check
      
    
## Keep only focal pair detections ---------------------------------------------
    table(ac21stoc$SITE, ac21stoc$NRES)  
    ac21stocRes <- ac21stoc[ac21stoc$NRES %in% '',] #keep target owl calls only
    
  
## Remove detections within X distance of surveys ------------------------------

  ## WITHIN 1 MILE (CHANGE HERE IF NEEEDED)
    table(ac21stocRes$SRV1MILE, useNA = 'always')
    ac21stocResNosurv1m <- ac21stocRes[ac21stocRes$SRV1MILE %in% 'N',]  #removed 652 rows
    

## Aggregate by site/station ---------------------------------------------------
  ac21agg <- aggregate(ac21stocResNosurv1m[,c('STOC_BARK','STOC_WHIS','STOC_BEG','STOC_PAIR',
                                              'STOC_IRREG','STOC_4N','STOC_ANY','MALE','FEMALE',
                                              'UNK','JUV','FEMALE_FNLC','MALE_FNLC',
                                              'FEMALE_FNLC_OR_PAIR','MALE_FNLC_OR_PAIR')],
                       by = list(as.factor(ac21stocResNosurv1m$SITE_STN)), FUN = sum)
  colnames(ac21agg)[1] <- 'SITE_STN'
  dim(ac21agg)  #45 rows = 45 stations with STOC detections

    ## keep in mind these can sum to greater than the number of clips... e.g., a clip can have STOC_BEG and STOC_4N
    ## but the 'STOC_ANY_N' and 'STVA_ANY_N' columns are the total number of clips with any of these species
    ## (but all STOC columns do not add up to 'STOC_ANY', e.g.)
    
  head(ac21agg[,c('SITE_STN','STOC_4N','STOC_ANY')])
      
    
## Save ------------------------------------------------------------------------
  
  write.csv(ac21stoc, 'output/01_ac_STOC_2021_cleaned.csv') #latest save 05/19/23 wtih FNLC separated 
  write.csv(ac21stocRes, 'output/02_ac_STOC_2021_residents.csv')  #last output 1/13/23, 05/19/23 with FNLC separated
  write.csv(ac21stocResNosurv1m, 'output/03_ac_STOC_2021_noSurveys1mile.csv') #latest output 05/19/23
  write.csv(ac21agg, 'output/04_ac_STOC_2021_aggregated.csv')
    

################################################################################  

## PART 2: EFFORT AND DISTANCE DATA ############################################
    
  # ac21agg <- fread('output/ac_STOC_2021_aggregated.csv') #if necessary
  
  
## Read in cleaned files from Julie --------------------------------------------  
  ac21jj <- fread('2021/COA_AC_Work/from_Julie/data_output/COAAC_2021_SampleSummary_fromNoise.csv')
  head(ac21jj)    

  
## Add SITE_STN column ---------------------------------------------------------
  ac21jj$SITE_STN <- paste(substr(ac21jj$site, 5,6), sapply(strsplit(as.character(ac21jj$site), '\\-'),'[',2), sep = '_')
    unique(ac21jj$SITE_STN)    

    
## Format dates and add duration -----------------------------------------------
  ac21jj$start <- as.POSIXct(strptime(ac21jj$min_date, format = '%Y-%m-%d'), tz = 'America/Los_Angeles')
  ac21jj$stop <- as.POSIXct(strptime(ac21jj$max_date, format = '%Y-%m-%d'), tz = 'America/Los_Angeles')
  ac21jj$duration <- difftime(ac21jj$stop, ac21jj$start) 
    #nDays is unique days while duration is the difference
  
  
## Merge -----------------------------------------------------------------------
  ac21merge <- merge(ac21agg, ac21jj[,c('SITE_STN','start','stop','duration')], 
                     by = c('SITE_STN'), all = TRUE)
  head(ac21merge)    
  
  nrow(ac21agg)  
  nrow(ac21jj)
  nrow(ac21merge) #good, should be 145 rows (to match ac21jj): 37 stations * 4 sites (minus 3 at WC)
  
  
## Add repro status of each site -----------------------------------------------
  ac21merge$reproState <- ifelse(grepl('DC', ac21merge$SITE_STN), 'fledged',
                                 ifelse(grepl('MC', ac21merge$SITE_STN), 'nest',
                                        ifelse(grepl('UG', ac21merge$SITE_STN), 'fledged', 'pair')))
  
  
## Add distances as measured in GIS --------------------------------------------
  
  #read in attribute tables from ArcGIS ('distance' field is the distance in m from center for each point)
  dist_dc <- fread('2021/COA_AC_Work/distances/drift_creek_2021_distances.csv', 
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_dc$site <- 'DC'
  dist_mc <- fread('2021/COA_AC_Work/distances/miller_creek_2021_distances.csv',
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_mc$site <- 'MC'
  dist_ug <- fread('2021/COA_AC_Work/distances/upper_greenleaf_2021_distances.csv',
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_ug$site <- 'UG'
  dist_wc <- fread('2021/COA_AC_Work/distances/waite_creek_2021_distances.csv',
                   select = c('LOCNAME','YR','XNAD83FP','YNAD83FP','STATION','Distance')); dist_wc$site <- 'WC'
  
  #combine and add 'SITE_STN'column for matching
  distances <- rbind(dist_dc, dist_mc); distances <- rbind(distances, dist_ug); distances <- rbind(distances, dist_wc)
  distances$SITE_STN <- paste(distances$site, 
                              formatC(distances$STATION, width = '2', format = 'd', flag = '0'), sep = '_')
  
  #merge with rest of the data
  ac21merge_dist <- merge(ac21merge, distances[,c('SITE_STN','Distance','XNAD83FP','YNAD83FP')], 
                          by = c('SITE_STN'), all = TRUE)
  
    nrow(ac21merge) ; length(unique(ac21merge$SITE_STN)) #145 rows, 145 unique sites
    nrow(distances) ; length(unique(distances$SITE_STN)) #145 rows, 145 unique sites
    nrow(ac21merge_dist) ; length(unique(ac21merge_dist$SITE_STN)) #145 rows, 145 unique sites

    
## Add intended distance from center for each station --------------------------
  ac21merge_dist$dist_intended <- ifelse(grepl('_12|_13|_18|_20|_25|_26', ac21merge_dist$SITE_STN), as.numeric(1000), NA)
  ac21merge_dist$dist_intended <- ifelse(grepl('_07|_11|_14|_24|_27|_31',  ac21merge_dist$SITE_STN), as.numeric(1732), ac21merge_dist$dist_intended)
  ac21merge_dist$dist_intended <- ifelse(grepl('_06|_08|_17|_21|_30|_32', ac21merge_dist$SITE_STN), as.numeric(2000), ac21merge_dist$dist_intended)
  ac21merge_dist$dist_intended <- ifelse(grepl('_02|_03|_05|_09|_10|_15|_23|_28|_29|_33|_35|_36', ac21merge_dist$SITESTN), as.numeric(2646), ac21merge_dist$dist_intended)
  ac21merge_dist$dist_intended <- ifelse(grepl('_01|_04|_16|_22|_34|_37', ac21merge_dist$SITE_STN), as.numeric(3000), ac21merge_dist$dist_intended)
  ac21merge_dist$dist_intended <- ifelse(grepl('19', ac21merge_dist$SITE_STN), as.numeric(0), ac21merge_dist$dist_intended)

    table(ac21merge_dist$dist_intended, useNA = 'always')
    plot(ac21merge_dist$dist_intended, ac21merge_dist$Distance) #did I match up the intended distances correctly? yes
  
  plot(ac21merge_dist$STOC_ANY ~ ac21merge_dist$Distance) #any STOC
  plot(ac21merge_dist$STOC_4N ~ ac21merge_dist$Distance)  #all STOC
  
  
  #save
    write.csv(ac21merge_dist, 'output/05_ac_2021_merged.csv')  #output 051923 with FNLC separated
    
    
    

  
  