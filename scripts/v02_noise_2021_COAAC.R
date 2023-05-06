## Create noise and effort covariates
## Based on code from Julie (v02_noise_2021_COAAC.R)

library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)
library(reshape2)  

##########Before we make detection histories, we are going to figure out 
  #first which days should not have any samples (find our sampling windows and true nulls)
  ## for this we will use the Noise analysis files. 

#get all noise files (these list all the recorded) and append to 1 r file
#Julie's path:
# files2021 <- list.files(path = "C:\\Users\\juliannajenkins\\USDA\\PNW BIOACOUSTICS LAB - Tagging Help/2021_Noise_Analysis", pattern = 'noisebyfile.csv|splbyfile.csv', recursive = TRUE, full.names = TRUE)
#Cara's path:
files22 <- list.files(path = 'E:/Activity_centers/AC_2022_NOISE_ANALYSIS', pattern = 'noisebyfile.csv|splbyfile.csv', recursive = TRUE, full.names = TRUE)
  length(files22)
  mylist <- lapply(files22, fread)
  
# files21<-subset(files2021, grepl( "2021AC", files2021))
# paste('Julie found', length(files21),'files in the summer 2020 noise folders') #There were 409 noise
# mylist <- lapply(files21, fread) 

n <- rbindlist(mylist, fill = TRUE, idcol = 'origin')
n[, origin := factor(origin, labels = basename(files22))]     #adds a filename column (not useful bc they're all 'spbyfile')
n$origin <- as.character(n$origin)
n$area<-substr(n$File, 1,3)
  table(n$area, useNA = 'a')
# n<-n[area=='COA',]#get rid of any OLY sites
n$site <- paste(sapply(strsplit(as.character(n$File),
                                   '\\_'), '[', 1),
                   sapply(strsplit(as.character(n$File), 
                                   '\\_'), '[', 2), sep = '_')
n$msno<-substr(n$site, 5,10)
  sort(unique(n$site))
  sort(unique(n$msno))  
  
n<- n[!duplicated(n), ] #get rid of duplicate rows based on sloppy record keeping--removed 35227

head(n)

n$date <- paste(sapply(strsplit(as.character(n$File), '\\_'), '[', 3),
                   sapply(strsplit(as.character(n$File), '\\_'), '[', 4), sep = ' ')
n$date <- as.POSIXct(strptime(n$date, '%Y%m%d %H%M%S'), 
                        tz = 'America/Los_Angeles') 
n$DATE2<-as.Date(sapply(strsplit(as.character(n$File), '\\_'), '[', 3), '%Y%m%d') 

summary(n$DATE2) #make sure there are no 'NA' dates

weirds<-n[n$`250.0Hz 1/3 Octave Band (Mean dB)`==0,]$File #6 waves with all 0's

n2<-n[!n$File %in% weirds ,] #remove those 6 wavs

###change cha to num in str
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

n2<-n2 %>% 
  mutate_if(is_all_numeric,as.numeric) 

#n$nonNA<-ifelse(!is.na(n[,7:12]),7,0)
  n2$meanSPL <- (n2$`250.0Hz 1/3 Octave Band (Mean dB)` + n2$`315.0Hz 1/3 Octave Band (Mean dB)` +
                  n2$`396.9Hz 1/3 Octave Band (Mean dB)` + n2$`500.0Hz 1/3 Octave Band (Mean dB)` +
                  n2$`630.0Hz 1/3 Octave Band (Mean dB)` + n2$`793.7Hz 1/3 Octave Band (Mean dB)` +
                  n2$`1000.0Hz 1/3 Octave Band (Mean dB)`) / 7

### now aggregate by station and date (average over all the hours for each day)
#noise_daily <- reshape2::dcast(n, site ~ DATE2, fun.aggregate = mean, value.var = 'mean')

Daily_noise_22<- n2 %>% dplyr::group_by( site, DATE2) %>% 
              dplyr::summarize(min_time=min((format(as.POSIXct(date), format = "%H:%M")), na.rm=T),
                               max_time=max((format(as.POSIXct(date), format = "%H:%M")), na.rm=T),
                               nWAV=n(),
                               durS=sum(`Samples (s)`, na.rm=T),
                               mnSPL= mean(meanSPL, na.rm=T))


head(Daily_noise_22)
## now back to long format
# noise <- reshape2::melt(noise_daily_ch0, id.vars = c('site', 'Channel'), variable.name = 'DATE', value.name = 'mean_noise')
# noise$DATE<-as.Date(noise$DATE, '%Y-%m-%d', tz = 'America/Los_Angeles')

# saveRDS(Daily_noise,"data_work/noise_2021_COA_AC.rds")
# saveRDS(Daily_noise,"C:\\Users\\juliannajenkins\\USDA\\PNW BIOACOUSTICS LAB - Tagging Help/COA_AC_julieWork/noise_2021_COA_AC.rds")

  saveRDS(Daily_noise_22, 'output/10_ac_22_dailyNoise.rds')

  
## SUMMARIZE
    
## Explore effort (recording per day)
  
  #2022
  hist(Daily_noise_22$durS / 3600) #in hours
    summary(Daily_noise_22$durS)        #in seconds
    summary(Daily_noise_22$durS) /3600  #in hours
    length(Daily_noise_22[is.na(Daily_noise_22$durS),]$durS)  #good, no NAs
    
  #2021 (Julie made this already)
  Daily_noise_21 <- readRDS('2021/COA_AC_Work/from_Julie/Codes/noise_2021_COA_AC.rds')
  head(Daily_noise_21)        

  hist(Daily_noise_21$durS / 3600) #in hours    
    summary(Daily_noise_21$durS)        #in seconds
    summary(Daily_noise_21$durS) / 3600 #in hours
    length(Daily_noise_21[is.na(Daily_noise_21$durS),]$durS)  #good, no NAs
      
      
## Explore noise (mean SPL per day) **CHECK TO SEE IF THIS IS THE SAME AS HOW I DID IT FOR 2018 DATA
    
  #2022
  hist(Daily_noise_22$mnSPL)
    summary(Daily_noise_22$mnSPL)       
    length(Daily_noise_22[is.na(Daily_noise_22$mnSPL),]$mnSPL)  #good, no NAs
      
  #2021    
  hist(Daily_noise_21$mnSPL)
    summary(Daily_noise_21$mnSPL)
    length(Daily_noise_21[is.na(Daily_noise_21$mnSPL),]$mnSPL)  #good, no NAs      
      
    
    
### Now make into weekly covar
      
      
### And make effort covar      