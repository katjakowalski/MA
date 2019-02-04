library(lubridate)
library(ggplot2)

###########################################################################################
# load DWD meteorological data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$datum >= "2016-09-01"  & tmk$datum <= "2017-07-15" )    

tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

# remove all plots with NA
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 318)

colnames(tmk)[1] <- "stat_id"

# load PEP data
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/PEP/PEP725_Kowalski")
data_PEP <- read.csv(file="PEP725_Kowalski.csv", header=TRUE, sep=";")
PEP_stats <- read.csv(file="PEP_DWD_stat.csv", header=TRUE, sep=",")
PEP_SOS <- subset(data_PEP, data_PEP$phase_id==11)
colnames(PEP_SOS)[1] <- "PEP_ID"

PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by="PEP_ID", all=TRUE)
PEP <- PEP[!is.na(PEP$day), ]
PEP <- PEP[!is.na(PEP$DWD_ID), ]
PEP_SOS <- aggregate(day ~ DWD_ID, data=PEP, mean)

###########################################################################################

data_input <- merge(tmk, SOS_TT[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.y=TRUE)
data_LOG_NDVI <- data_input[!is.na(data_input$LOG_NDVI), ]

data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID", all.y=TRUE)

###########################################################################################

tt_CD_count <- function(statid, 
                   t_day, 
                   year,
                   doy,
                   datum,
                   t_base=5,
                   c_min = 0,
                   c_max = 5,
                   doy_crit) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    datum,
                    doy,
                    doy_crit)
  
  SOS_LSP <- NULL
  
  for( i in unique(data$statid)){   # loop through plots 
  
    # subset plot
    d = subset(data, data$statid == i)
    
    # SOS
    doy_crit <- d$doy_crit[1]
    
    # subset chilling
    date_crit <- as.Date(doy_crit, origin ="2017-01-01") -1
    d_c = subset(data, data$statid == i & data$datum >= "2016-09-01" & data$datum <= date_crit )
    
    chilling <- 0 
    out <- NULL
    
    # count chilling 
    for(c in d_c$doy){
      
      dat_c <- subset(d_c, d_c$doy == c)
      t <- dat_c$t_day

      if (t >= c_min &
          t <= c_max) {
        chilling = chilling + 1
      }
    }
   
    out <- data.frame("stat_id" = i,
                      "CD" = chilling)
    SOS_LSP <- rbind(SOS_LSP, out)
  }
  return(SOS_LSP)
}

###########################################################################################

CD_GAM_EVI <- tt_CD_count(statid=data_input$stat_id, 
                   t_day=data_input$WERT,
                   year = data_input$year,
                   doy= data_input$doy,
                   datum = data_input$datum,
                   doy_crit = data_input$GAM_EVI)
colnames(CD_GAM_EVI) <- c("stat_id","CD_GAM_EVI")

CD_LOG_EVI <- tt_CD_count(statid=data_input$stat_id, 
                      t_day=data_input$WERT,
                      year = data_input$year,
                      doy= data_input$doy,
                      datum = data_input$datum,
                      doy_crit = data_input$LOG_EVI)
colnames(CD_LOG_EVI) <- c("stat_id", "CD_LOG_EVI")

CD_LOG_NDVI <- tt_CD_count(statid=data_LOG_NDVI$stat_id, 
                      t_day=data_LOG_NDVI$WERT,
                      year = data_LOG_NDVI$year,
                      doy= data_LOG_NDVI$doy,
                      datum = data_LOG_NDVI$datum,
                      doy_crit = data_LOG_NDVI$LOG_NDVI)
colnames(CD_LOG_NDVI) <- c("stat_id", "CD_LOG_NDVI")

CD_GAM_NDVI <- tt_CD_count(statid=data_input$stat_id, 
                      t_day=data_input$WERT,
                      year = data_input$year,
                      doy= data_input$doy,
                      datum = data_input$datum,
                      doy_crit = data_input$GAM_NDVI)
colnames(CD_GAM_NDVI) <- c("stat_id", "CD_GAM_NDVI")


CD_PEP <- tt_CD_count(statid=data_PEP$stat_id, 
                      t_day=data_PEP$WERT,
                      year = data_PEP$year,
                      doy= data_PEP$doy,
                      datum = data_PEP$datum,
                      doy_crit = data_PEP$day)
colnames(CD_PEP) <- c("stat_id",  "CD_PEP")

###########################################################################################

CD_SOS <- merge(CD_GAM_EVI, CD_LOG_EVI, by="stat_id", all.x=TRUE)
CD_SOS <- merge(CD_SOS, CD_GAM_NDVI, by="stat_id", all.x=TRUE)
CD_SOS <- merge(CD_SOS, CD_LOG_NDVI, by="stat_id", all.x=TRUE)
CD_SOS <- merge(CD_SOS, CD_PEP, by="stat_id", all.x=TRUE)

# add SOS estimates 
GDD_SOS <- merge(GDD_SOS, CD_SOS , by="stat_id", all.x=TRUE)

# difference CD - SOS
GDD_SOS$diff_GAM_EVI_SQ <- GDD_SOS$SQ - GDD_SOS$GAM_EVI
GDD_SOS$diff_LOG_EVI_SQ <- GDD_SOS$SQ - GDD_SOS$LOG_EVI 

GDD_SOS$diff_GAM_NDVI_SQ <- GDD_SOS$SQ - GDD_SOS$GAM_NDVI
GDD_SOS$diff_LOG_NDVI_SQ <- GDD_SOS$SQ - GDD_SOS$LOG_NDVI 

# write results
write.csv(GDD_SOS, file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190204_GDD_SOS.csv",row.names = FALSE )

GDD_PEP <- GDD_SOS[!is.na(GDD_SOS$GDD_PEP),]
write.csv(GDD_PEP, file="20190204_GDD_PEP.csv",row.names = FALSE )




 
