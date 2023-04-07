## summarizing detections from PNW monitoring hexagons overlapping activity center sites (2022)

library(data.table)


## read in data (hex-level)
  hex_f <- fread('2022/PNW_monitoring_data/Detections_byStnWeek/') #do I want weekly or season summaries?

  hex_f <- fread('PNW_monitoring_data/STOC_F_HexWK_COA2021.csv') ; hex_f$site <- paste(hex_f$study, hex_f$hex, sep = '_')
  hex_m <- fread('PNW_monitoring_data/STOC_M_HexWK_COA2021.csv') ; hex_m$site <- paste(hex_m$study, hex_m$hex, sep = '_')
  hex_irreg <- fread('PNW_monitoring_data/STOC_IRREG_U_HexWK_COA2021.csv') ; hex_irreg$site <- paste(hex_irreg$study, hex_irreg$hex, sep = '_')
  hex_u <- fread('PNW_monitoring_data/STOC_U_HexWK_COA2021.csv') ; hex_u$site <- paste(hex_u$study, hex_u$hex, sep = '_')


## read in data (stn-level)
  stn_f <- fread('PNW_monitoring_data/STOC_F_stnWK_COA2021.csv')
  stn_m <- fread('PNW_monitoring_data/STOC_M_stnWK_COA2021.csv')
  stn_irreg <- fread('PNW_monitoring_data/STOC_IRREG_U_stnWK_COA2021.csv')
  stn_u <- fread('PNW_monitoring_data/STOC_U_stnWK_COA2021.csv')
  
      
## add 'sum' columns
  hex_f$sum <- rowSums(hex_f[,c(4:20)], na.rm = TRUE)
  hex_m$sum <- rowSums(hex_m[,c(4:20)], na.rm = TRUE)
  hex_irreg$sum <- rowSums(hex_irreg[,c(4:20)], na.rm = TRUE)
  hex_u$sum <- rowSums(hex_u[,c(4:20)], na.rm = TRUE)  
  
  stn_f$sum <- rowSums(stn_f[,c(5:21)], na.rm = TRUE)
  stn_m$sum <- rowSums(stn_m[,c(5:21)], na.rm = TRUE)
  stn_irreg$sum <- rowSums(stn_irreg[,c(5:21)], na.rm = TRUE)
  stn_u$sum <- rowSums(stn_u[,c(5:21)], na.rm = TRUE)

  
## pull out hexagons that are near activity center sites
  (ac_hexes <- data.table('study' = c(rep('DC',4),rep('UG',2),rep('WC',2),rep('MC',1)),
                          'hex' = c('23137','23042','23039','22848',
                                    '21811','21714',
                                    '20891','20785',
                                    '21308'),
                          'year' = rep('2021')))
  
  ac_hex_f <- hex_f[hex_f$hex %in% ac_hexes$hex,]
  ac_hex_m <- hex_m[hex_m$hex %in% ac_hexes$hex,]
  ac_hex_irreg <- hex_irreg[hex_irreg$hex %in% ac_hexes$hex,]
  ac_hex_u <- hex_u[hex_u$hex %in% ac_hexes$hex,]
  
  ac_stn_f <- stn_f[stn_f$hex %in% ac_hexes$hex,]
  ac_stn_m <- stn_m[stn_m$hex %in% ac_hexes$hex,]
  ac_stn_irreg <- stn_irreg[stn_irreg$hex %in% ac_hexes$hex,]
  ac_stn_u <- stn_u[stn_u$hex %in% ac_hexes$hex,]
  
  
## save so I can plot in ArcMap
  write.csv(hex_f, 'PNW_monitoring_data/outputs/STOC_F_2021_hexes.csv')
  write.csv(hex_m, 'PNW_monitoring_data/outputs/STOC_M_2021_hexes.csv')
  write.csv(hex_irreg, 'PNW_monitoring_data/outputs/STOC_IRREG_2021_hexes.csv')
  write.csv(hex_u, 'PNW_monitoring_data/outputs/STOC_U_2021_hexes.csv')

  write.csv(stn_f, 'PNW_monitoring_data/outputs/STOC_F_2021_stns.csv')
  write.csv(stn_m, 'PNW_monitoring_data/outputs/STOC_M_2021_stns.csv')
  write.csv(stn_irreg, 'PNW_monitoring_data/outputs/STOC_IRREG_2021_stns.csv')
  write.csv(stn_u, 'PNW_monitoring_data/outputs/STOC_U_2021_stns.csv')
  
##     