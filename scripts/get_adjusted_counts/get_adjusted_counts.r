## Script from Zack Ruff, modified by Cara (June 2023)

library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)

# This document contains our class-specific precision estimates for PNW-Cnet v4.
# Doesn't have all of them but for STVA, STVA_IRREG, and INSP it's fine
metrics <- read_xlsx("scripts/get_adjusted_counts/QC_Stats_Combined_22Jul21.xlsx")
metrics_stva <- metrics[metrics$Class %in% c('STVA','STVA_IRREG','INSP'),]


## DEFINE FUNCTIONS (Zack) -----------------------------------------------------

# Reads in a CNN_Predictions file, groups rows by station and date, and returns 
# a table of counts of apparent detections by stn-date (i.e. the number of 12-s
# segments where the class score exceeded the threshold for each class).

makeDailyDetTable <- function(pred_file_path, threshold = 0.95) {
  
  predictions <- read_csv(pred_file_path, show_col_types = FALSE) 
  
  daily_detection_table <- predictions %>% 
    separate(Filename, into = c("Area", "Hex", "Stn", "StrDate", "StrTime", "p",
                                "Part", "ext")) %>% 
    select(-p, -ext) %>%
    mutate(Date = ymd(StrDate),
           Day1 = min(Date),
           Rec_Day = as.integer(difftime(Date, Day1, units = "days")) + 1,
           Rec_Week = floor((Rec_Day - 1) / 7) + 1,
           .before = AEAC) %>% 
    group_by(Area, Hex, Stn, Date, Rec_Day, Rec_Week) %>% 
    summarize(across(AEAC:ZEMA, function(label) length(which(label >= threshold))))
  
  return(daily_detection_table)
}

# Takes output from makeDailyDetTable() and returns detections by stn-week. Can
# modify to get detections by station-deployment, by hex-date, hex-week, etc.
makeWeeklyDetTable <- function(det_table) {
  weekly_detection_table <- det_table %>% 
    group_by(Area, Hex, Stn, Rec_Week) %>% 
    summarize(Week_Start = min(Date), 
              across(AEAC:ZEMA, sum))
  return(weekly_detection_table)
}

# Takes a table of apparent detections and and a table of performance metrics by
# class and adjusts the number of detections of each class based on precision.
# Probably best to run this on the daily detections, then group and summarize
# the output in various ways as needed. N.b. because precision is always < 1 and 
# the counts are rounded down, single detections will be discarded.
adjustDetections <- function(apparent_detections, metrics) {
  
  target_classes <- metrics %>%
    filter(Class != "Other") %>%
    arrange(Class) %>% 
    pull(Class)
  
  adjusted_detections <- apparent_detections %>% 
    select(Area:Rec_Week)
  
  for(i in target_classes) {
    prec <- metrics %>% 
      filter(Class == `i`) %>% 
      pull(v4_Precision)

    adjusted_detections[, i] <- ifelse( prec == "NaN", 
                                        apparent_detections[, i], 
                                        apparent_detections[, i] * as.double(prec) )
  }
  
  adjusted_detections <- adjusted_detections %>% 
    mutate(across(all_of(target_classes), replace_na, 0)) %>% 
    mutate(across(all_of(target_classes), as.integer))

  return(adjusted_detections)
}


## NOW RUN WITH PRED FILES (Cara) ----------------------------------------------

# preds <- list.files('2021/Predictions_COA_AC_2021/', pattern = 'predictions', full.names = TRUE)
preds <- list.files('2022/AC_2022_PREDICTIONS/', pattern = 'redictions', full.names = TRUE)

all_counts <- NULL
for (pp in preds){
  #generate daily detection table and keep only desired classes
    daily_det <- makeDailyDetTable(pp, 0.95) %>%
                    select(Area, Hex, Stn, Date, Rec_Day, Rec_Week, STVA, STVA_IRREG, INSP)

  #adjust daily counts by model metrics
    daily_adj <- adjustDetections(daily_det, metrics_stva)
    
  #save
  all_counts <- rbind(all_counts, daily_adj)
}

## Format output
all_counts$Hex <- ifelse(all_counts$Hex %in% 'Baker', 'BC', all_counts$Hex) #for 2022
all_counts$Site <- paste(substr(all_counts$Hex, 1, 2), all_counts$Stn, sep = '_')

## Save
# write.csv(all_counts, '2021/Predictions_COA_AC_2021/stva_daily_counts_adjusted_2021.csv')
write.csv(all_counts, '2022/AC_2022_PREDICTIONS/stva_daily_counts_adjusted_2022.csv')

