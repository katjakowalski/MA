setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))
tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)
colnames(tmk)[2] <- "stat_id"

# load LSP data 
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/")
LSP_evi <- read.csv(file="20181211_mean_evi.csv", header=TRUE)
LSP_ndvi <- read.csv(file="20181211_mean_ndvi.csv", header=TRUE)

LSP <- merge(LSP_evi[, c("b4", "sp", "stat_id")], LSP_ndvi[, c("b4", "sp", "stat_id")], by="stat_id")
colnames(LSP) <- c("stat_id", "LOG_EVI", "GAM_EVI", "LOG_NDVI", "GAM_NDVI")

data <- merge(tmk, LSP_evi[, c("b4", "sp", "stat_id")], by="stat_id", all.x=TRUE)
data <- data[!is.na(data$sp), ]
data <- data[!is.na(data$b4), ]

cd_LSP <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     date,
                     t_crit,
                     f_base= 5,
                     c_base = 5,
                     c_crit = 80) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    date,
                    t_crit)
  
  SOS_LSP <- NULL
  #k <- 0
  
  for( i in unique(data$statid)){
    
    d_f = subset(data, data$statid == i & data$year == 2017)
    d_c = subset(data, data$statid == i & data$date >= "2016-09-01" & data$date <= "2017-05-01" )
    
    print(paste("df", length(d_f$doy)))
    print(paste("dc", length(d_c$doy)))
    
    if(length(d_f$doy) >= 330 & length(d_c$doy) >= 190 ){
      F_crit <- d_f$t_crit[1]
      forcing <- 0
      chilling <- 0 
      #k <- k + 1
      k <- 0
      # chilling 
      for(e in d_c$doy){
        k = k+1
        dat_c = subset(d_c, d_c$doy == e)
        doy_chill <- dat_c$doy
        t_c <- dat_c$t_day
        
        if( t_c <= c_base){
          chilling = chilling + 1
        }
        
        if(chilling > c_crit){
          t1 = doy_chill
          #print(paste(t1))
          print(paste(k))
          break
        }
      }
      print(paste(k))
      
      for(f in d_f$doy){
        if(f >= t1 ){
          out <- NULL
          dat <- subset(d_f, d_f$doy == f)
          doy_sos <- dat$doy
          t <- dat$t_day
          
          
          if (t >= c_base){
            val = t-c_base
            forcing = forcing + val 
          }
          
          if (forcing >= F_crit){
            
            out <- data.frame("stat_id" = i, 
                              "CD" = doy_sos)
            SOS_CD <- rbind(SOS_CD, out)
            break
          }
        }
        else{next}
      }
      #if (is.null(out)){
      #  print(paste(i))
      
      #}
    }
    else{print(paste("obs", i))}
  }
  return(SOS_CD)
}


GDD_CD_GAM <- cd_LSP(statid=data$stat_id, 
                   t_day=data$WERT,
                   year = data$year,
                   doy= data$doy,
                   date = data$datum,
                   t_crit =data$sp )
