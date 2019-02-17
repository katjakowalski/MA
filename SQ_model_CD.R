
# load meteorological data
tmk <- read.csv(file=file.path(path_data, "TMK_MN004.txt"), header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

# add missing DOY 
tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$datum >= "2016-09-01"  & tmk$datum <= "2017-07-15" )    

# moving average
tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

# fill gaps with interpolated value 
tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

# remove plots with data gaps and shorter time series 
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 318)

colnames(tmk)[1] <- "stat_id"

# load and merge PEP data for 2017
data_PEP <- read.csv(file=file.path(path_data,"PEP725_Kowalski.csv"), header=TRUE, sep=";")
PEP_stats <- read.csv(file=file.path(path_data,"PEP_DWD_stat.csv"), header=TRUE, sep=",")

# select BBCH phase of leaf unfolding (11)
PEP_SOS <- subset(data_PEP, data_PEP$phase_id==11)
colnames(PEP_SOS)[1] <- "PEP_ID"

# select observations within 5000m distance of DWD plots
PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by="PEP_ID", all=TRUE)
PEP <- PEP[!is.na(PEP$day), ]
PEP <- PEP[!is.na(PEP$DWD_ID), ]
PEP_SOS <- aggregate(day ~ DWD_ID, data=PEP, mean)

# input datasets 
data_input <- merge(tmk, SOS_TT[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.y=TRUE)
data_LOG_NDVI <- data_input[!is.na(data_input$LOG_NDVI), ]
data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID", all.y=TRUE)


CD_count <- function(statid, 
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
    
    # subset chilling period
    date_crit <- as.Date(doy_crit, origin ="2017-01-01") -1 # SOS estimate
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
    
    if( chilling == 0){
      print("0")
      out <- data.frame("stat_id" = i,
                        "CD" = NA)
      SOS_LSP <- rbind(SOS_LSP, out)
    }
   else{
    out <- data.frame("stat_id" = i,
                      "CD" = chilling)
    SOS_LSP <- rbind(SOS_LSP, out)
   }
  }
  return(SOS_LSP)
}
