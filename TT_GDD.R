
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$year == 2017 & tmk$doy <= 196)

# rolling mean 
tmk$gap_fill <- rollapply(
  data    = tmk$WERT,
  width   = 4,
  FUN     = function(x) mean(x, na.rm=TRUE), 
  fill    = NA,
  align   = "center",
  partial=TRUE
)

# fills gaps 
tmk <- tmk %>%
  mutate(WERT = coalesce(WERT, gap_fill))

# remove plots with data gaps and shorter time series 
tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT))) %>%
  filter(length(STATION_ID) == 196)

colnames(tmk)[1] <- "stat_id"

# load PEP data
setwd( "\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/PEP/PEP725_Kowalski")
data_PEP <- read.csv(file = "PEP725_Kowalski.csv", header = TRUE, sep = ";")
PEP_stats <- read.csv(file = "PEP_DWD_stat.csv", header = TRUE, sep = ",")
PEP_SOS <- subset(data_PEP, data_PEP$phase_id == 11)
colnames(PEP_SOS)[1] <- "PEP_ID"

PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by = "PEP_ID", all =TRUE)
PEP <- PEP[!is.na(PEP$day),]
PEP <- PEP[!is.na(PEP$DWD_ID),]
PEP_SOS <- aggregate(day ~ DWD_ID, data = PEP, mean)


data_input <- merge(tmk, SOS_TT[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.y=TRUE)
data_LOG_NDVI <- data_input[!is.na(data_input$LOG_NDVI), ]

data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID", all.y=TRUE)


tt_GDD <- function(statid, 
                   t_day, 
                   year,
                   doy,
                   datum,
                   t_base=5,
                   doy_crit) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    datum,
                    doy,
                    doy_crit)
  
  SOS_LSP <- NULL
  
  for( i in unique(data$statid)){   # loop through plots 
    
    # subset forcing
    d = subset(data, data$statid == i)
    
    
    # SOS 
    doy_crit <- d$doy_crit[1]
    d = subset(d, d$doy <= doy_crit)
    
    forcing <- 0
    out <- NULL
    
    if(length(d$t_day) < 10 ){
      out <- data.frame("stat_id" = i, 
                        "GDD" = NA)
      SOS_LSP <- rbind(SOS_LSP, out)
      next
    }
    
    # forcing
    for (x in d$doy) {                  # loop through days until SOS
      
      dat <- subset(d, d$doy == x)
      t <- dat$t_day
      
      if (t > t_base) {
        val = t - t_base
        forcing = forcing + val
      }
    }
    
    out <- data.frame("stat_id" = i, 
                      "GDD" = forcing)
    SOS_LSP <- rbind(SOS_LSP, out)
  }
  return(SOS_LSP)
}







