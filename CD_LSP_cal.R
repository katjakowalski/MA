setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))

tmk <- tmk %>%
  group_by(STATION_ID)%>%
  mutate(datum = as.Date(datum)) %>%
  complete(datum = seq.Date(min(datum), max(datum), by="day"))

tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)

tmk <- subset(tmk,  tmk$datum >= "2016-09-01")

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

tmk <- tmk %>%
  group_by(STATION_ID) %>%
  filter(!any(is.nan(WERT)))

# load LSP data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/")
LSP_evi <- read.csv(file="20181211_mean_evi.csv", header=TRUE)
LSP_ndvi <- read.csv(file="20181211_mean_ndvi.csv", header=TRUE)

LSP <- merge(LSP_evi[, c("b4", "sp", "stat_id")], LSP_ndvi[, c("b4", "sp", "stat_id")], by="stat_id")
colnames(LSP) <- c("stat_id", "LOG_EVI", "GAM_EVI", "LOG_NDVI", "GAM_NDVI")

data <- merge(tmk, LSP_evi[, c("b4", "sp", "stat_id")],by.x="STATION_ID", by.y="stat_id", all.x=TRUE)
data <- data[!is.na(data$sp), ]
data <- data[!is.na(data$b4), ]

cd_model_lsp <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     date,
                     doy_crit,
                     f_base= 5,
                     c_base = 5) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    date,
                    doy_crit)
  
  SOS_CD <- NULL
  #k <- 0
  
  for( i in unique(data$statid)){
    
    d_f = subset(data, data$statid == i & data$year == 2017)
    d_c = subset(data, data$statid == i & data$date >= "2016-09-01" & data$date <= "2017-05-01" )
    print(i)
    print(paste("df", length(d_f$doy)))
    print(paste("dc", length(d_c$doy)))
    
    #if(length(d_f$doy) >= 330 & length(d_c$doy) >= 190 ){
    
    forcing <- 0
    chilling <- 0 
    #k <- k + 1
    k <- 0
    t1 <- 0
    
    doy_crit <- as.integer(round(d_f$doy_crit[1], 0))
    
    date_crit <- as.Date(doy_crit, origin ="2017-01-01") -1  # origin = day 0 
    
    d_f = subset(d_f, d_f$doy <= doy_crit)
    
    # chilling 
    for(e in d_c$doy){
      k = k+1
      dat_c = subset(d_c, d_c$doy == e)
      doy_chill <- dat_c$doy
      dat_chill <- dat_c$date # date of doy e
      t_c <- dat_c$t_day
      
      if( t_c <= c_base){
        chilling = chilling + 1
      }
      
      if(chilling > c_crit){
        t1 = date_crit
        print(paste(t1))
        #print(paste(k))
        break
      }
    }
    if (t1 == 0){
      print(paste("chilling insufficient for station", i))
      out <- NULL
      out <- data.frame("stat_id" = i,
                        "CD" = NA,
                        "t_CD" = NA)
      SOS_CD <- rbind(SOS_CD, out)
      next
    }
    
    
    for(f in d_f$doy){
      if(dat_chill >= t1 ){
        out <- NULL
        dat <- subset(d_f, d_f$doy == f)
        doy_sos <- dat$doy
        t <- dat$t_day
        
        
        if (t >= f_base){
          val = t-f_base
          forcing = forcing + val 
        }
      }
      else{next}
    }
        out <- data.frame("stat_id" = i, 
                          "CD" = forcing,
                          "t_CD" = t1)
        SOS_CD <- rbind(SOS_CD, out)
  }
  return(SOS_CD)
}


GDD_CD_GAM <- cd_model_lsp(statid=data$STATION_ID, 
                   t_day=data$WERT,
                   year = data$year,
                   doy= data$doy,
                   date = data$datum,
                   doy_crit = data$sp)

ggplot(GDD_CD_GAM)+
  geom_histogram(aes(x=CD), binwidth=5)
