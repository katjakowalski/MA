
# load temperature data (daily mean)
tmk <- read.csv(file=file.path(path_data, "TMK_MN004.txt"), header=TRUE, sep=";")
tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

# add missing DOY
tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk, tmk$year == 2017 & tmk$doy <= 196)

# moving average
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

# load PEP data for 2017 
data_PEP <- read.csv(file = file.path(path_data, "PEP725_Kowalski.csv"), header = TRUE, sep = ";")
PEP_stats <- read.csv(file = file.path(path_data,"PEP_DWD_stat.csv"), header = TRUE, sep = ",")

# select BBCH phase of leaf unfolding (11)
PEP_SOS <- subset(data_PEP, data_PEP$phase_id == 11)
colnames(PEP_SOS)[1] <- "PEP_ID"

# select observations within 5000m distance of DWD plots
PEP <- merge(PEP_stats, PEP_SOS[, c("day", "PEP_ID", "species")], by = "PEP_ID", all =TRUE)
PEP <- PEP[!is.na(PEP$day),]
PEP <- PEP[!is.na(PEP$DWD_ID),]
PEP_SOS <- aggregate(day ~ DWD_ID, data = PEP, mean)

data_input <- merge(tmk, SOS_TT[, c("LOG_EVI", "GAM_EVI","LOG_NDVI","GAM_NDVI", "stat_id")], by="stat_id", all.y=TRUE)
data_LOG_NDVI <- data_input[!is.na(data_input$LOG_NDVI), ]
data_PEP <- merge(tmk, PEP_SOS, by.x="stat_id", by.y="DWD_ID", all.y=TRUE)

# Thermal Time model - using SOS estimates 
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
    
    # subset plot
    d = subset(data, data$statid == i)
    
    # SOS from RS
    doy_crit <- d$doy_crit[1]
    d = subset(d, d$doy <= doy_crit)
    
    forcing <- 0
    out <- NULL

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







