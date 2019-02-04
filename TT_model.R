
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


#################################################################################
tt_model <- function(statid,
                     t_day,
                     year,
                     doy,
                     t_base = 5,
                     t_crit = 250) {
  data = data.frame(statid,
                    t_day,
                    year,
                    doy)
  
  SOS_TT <- NULL
  k <- 0
  
  for (i in unique(data$statid)) {
    d = subset(data, data$statid == i & data$year == 2017)
    
    forcing <- 0
    k <- k + 1
    
    for (x in d$doy) {
      out <- NULL
      dat <- subset(d, d$doy == x)
      doy_sos <- dat$doy
      t <- dat$t_day
      
      if (t > t_base) {
        val = t - t_base
        forcing = forcing + val
      }
      
      if (forcing > t_crit) {
        out <- data.frame("stat_id" = i,
                          "TT" = doy_sos)
        SOS_TT <- rbind(SOS_TT, out)
        
        break
      }
    }
    if (is.null(out)) {
      print(paste(i))
    }
  }
  return(SOS_TT)
}

