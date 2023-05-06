## summarizing detections from PNW monitoring hexagons overlapping activity center sites (2022)

library(data.table)


## read in data (hex-level)
  

## read in data (stn-level)
  coa_stn_f <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/COA/STOC_F_stnWK_COA2022.csv') #do I want weekly or season summaries?
  coa_stn_m <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/COA/STOC_M_stnWK_COA2022.csv') 
  coa_stn_irreg_u <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/COA/STOC_IRREG_U_stnWK_COA2022.csv') 
  coa_stn_u <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/COA/STOC_U_stnWK_COA2022.csv') 

  ump_stn_m <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/UMP/STOC_M_stnWK_UMP2022.csv')
  ump_stn_u <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/UMP/STOC_U_stnWK_UMP2022.csv') 

  cle_stn_f <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/CLE/STOC_F_stnWK_CLE2022.csv') 
  cle_stn_m <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/CLE/STOC_M_stnWK_CLE2022.csv') 
  cle_stn_u <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/d2022/CLE/STOC_U_stnWK_CLE2022.csv') 
  
  #combine study areas  
  stn_f <- rbind(coa_stn_f, cle_stn_f, fill = TRUE)
  stn_m <- rbind(coa_stn_m, ump_stn_m, fill = TRUE) ; stn_m <- rbind(stn_m, cle_stn_m, fill = TRUE)
  stn_irreg_u <- coa_stn_irreg_u
  stn_u <- rbind(coa_stn_u, ump_stn_u, fill = TRUE) ; stn_u <- rbind(stn_u, cle_stn_u, fill = TRUE)
  
  
## add 'sum' columns
  # hex_f$sum <- rowSums(hex_f[,c(4:20)], na.rm = TRUE)
  # hex_m$sum <- rowSums(hex_m[,c(4:20)], na.rm = TRUE)
  # hex_irreg$sum <- rowSums(hex_irreg[,c(4:20)], na.rm = TRUE)
  # hex_u$sum <- rowSums(hex_u[,c(4:20)], na.rm = TRUE)  
  
  stn_f$sum <- rowSums(stn_f[,c(5:25)], na.rm = TRUE)
  stn_m$sum <- rowSums(stn_m[,c(5:25)], na.rm = TRUE)
  stn_irreg_u$sum <- rowSums(stn_irreg_u[,c(5:15)], na.rm = TRUE)
  stn_u$sum <- rowSums(stn_u[,c(5:25)], na.rm = TRUE)
  
  
## pull out hexagons that are near activity center sites
  (ac_hexes <- data.table('study' = c(rep('DC',5),rep('CC',4),rep('LM',1),rep('BC',6)),
                          'hex' = c('23137','23042','23039','22848','23044',
                                    '22276','22371','22183','21989',
                                    '17509',
                                    '37439','37583','37731','38019','37588','37442'),
                          'year' = rep('2022')))
  
  ac_stn_f <- stn_f[stn_f$hex %in% ac_hexes$hex,]
  ac_stn_m <- stn_m[stn_m$hex %in% ac_hexes$hex,]
  ac_stn_irreg_u <- stn_irreg_u[stn_irreg_u$hex %in% ac_hexes$hex,]
  ac_stn_u <- stn_u[stn_u$hex %in% ac_hexes$hex,]
  
  
## save so I can plot in ArcMap
  write.csv(stn_f, '2022/PNW_monitoring_data/outputs/STOC_F_2022_stns.csv')
  write.csv(stn_m, '2022/PNW_monitoring_data/outputs/STOC_M_2022_stns.csv')
  write.csv(stn_irreg_u, '2022/PNW_monitoring_data/outputs/STOC_IRREG_2022_stns.csv')
  write.csv(stn_u, '2022/PNW_monitoring_data/outputs/STOC_U_2022_stns.csv')
  
##     
  