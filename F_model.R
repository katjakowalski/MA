setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk <- transform(tmk, datum = as.Date(as.character(ZEITSTEMPEL), "%Y%m%d"))
tmk$year <- year(tmk$datum)
tmk$doy <- yday(tmk$datum)


cd_model <- function(statid, 
                     t_day, 
                     year,
                     doy,
                     date,
                     f_base= 5,
                     c_base = 5,
                     f_crit = 250,
                     c_crit = 90) {
  
  data = data.frame(statid, 
                    t_day,
                    year, 
                    doy,
                    date)
  
  SOS_CD <- NULL
  k <- 0
  
  for( i in unique(data$statid)){

    d_f = subset(data, data$statid == i & data$year == 2017)
    d_c = subset(data, data$statid == i & data$date >= "2016-09-01" & data$date <= "2017-05-01" )
    
    print(paste("df", length(d_f$doy)))
    print(paste("dc", length(d_c$doy)))
    
    if(length(d_f$doy) >= 330 & length(d_c$doy) >= 190 ){
      
      forcing <- 0
      chilling <- 0 
      k <- k + 1
    
      # chilling 
      for(e in d_c$doy){

        dat_c = subset(d_c, d_c$doy == e)
        doy_chill <- dat_c$doy
        t_c <- dat_c$t_day
        
        if( t_c <= c_base){
          chilling = chilling + 1
        }
        
        if(chilling > c_crit){
          t1 = doy_chill
          print(paste(t1))
          break
        }
      }
      
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
          
          if (forcing >= f_crit){
           
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


SOS_CD <- cd_model(statid=tmk$STATION_ID, 
                   t_day=tmk$WERT,
                   year = tmk$year,
                   doy= tmk$doy,
                   date = tmk$datum)


pheno_rs <- merge(pheno_rs, SOS_CD, by="stat_id", all.x=TRUE)

cor.test(pheno_rs$b4, pheno_rs$CD, use="complete.obs")
cor.test(pheno_rs$sp, pheno_rs$CD, use="complete.obs")


pheno_rs$diff_GAM_CD <- abs(pheno_rs$sp - pheno_rs$CD)
pheno_rs$diff_LOG_CD <- abs(pheno_rs$b4 - pheno_rs$CD)

mean(pheno_rs$diff_GAM_CD, na.rm = TRUE)
mean(pheno_rs$diff_LOG_CD, na.rm = TRUE)

quantile(pheno_rs$CD, na.rm=TRUE, c(.05, .50,  .95))



