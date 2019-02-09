library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file="data_clear.csv")

data <- subset(data, dwd_stat != 379 &   # climate data missing
                       dwd_stat != 760 & # climate data missing
                       dwd_stat != 1503 & # climate data missing
                       dwd_stat != 2878 & # climate data missing
                       dwd_stat != 3490 & # climate data missing
                       dwd_stat != 4878 & # climate data missing
                       dwd_stat != 5100 & # climate data missing
                       dwd_stat != 5715 & # climate data missing
                       dwd_stat != 4485 & # climate data missing
                       dwd_stat != 1550 &   # samples missing
                       dwd_stat != 3679 &   # sample missing
                       dwd_stat != 7424)    # sample missing

data_evi <- subset(data, data$evi < 1.1 & data$evi >= 0 & data$year == 2017)
data_ndvi <- subset(data, data$ndvi < 1.1 & data$ndvi >= 0 & data$year == 2017)


#################################################################################

pheno_model <- function(plotid, 
                        index, 
                        doy, 
                        year, 
                        stat_id,
                        min_obs = 10){
  
  data = data.frame(plotid,
                    index, 
                    doy, 
                    year,
                    stat_id)      
  
  l_samples <- length(unique(data$plotid))             
  
  nls_fit_result <- vector("list", l_samples)          
  sp_fit_result <- vector("list", l_samples)
  k <- 0

  for( p in unique(data$plotid)){
    
    transition <- c()
    b4_start <- c()
    
    d = subset(data, data$plotid == p &  data$year == "2017")
  
    stat_id <- d$stat_id[1]

    k <- k + 1
    
    if(length(d$doy) >= min_obs){
      
      d_tr <- subset(d, d$doy >= 75 & d$doy <= 250) 
      transition <- with(d_tr, doy[index == max(index)]) + 20
      d <- subset(d, d$doy <= transition)
      
      b4_start <- round(mean(d[which(d$index > median(d$index)), "doy"]), 0)
 
      base_index <- mean(subset(d, d$doy <= 50)$index)
      
      if (is.nan(base_index)){
        base_index <- min(d$index)
      }
      
      d <- d[!d$doy <= 50,]                              # delete all rows < doy 50

      df_base <- d[0,]                                   # create empty df with column names
      df_base[c(1:50), ] <- rep(NA, ncol(df_base))       # fill 50 rows with NA
      df_base$index <- base_index
      df_base$doy <- seq(1,50,1)
      
      dat <- rbind(d, df_base)

      #LOGISTIC MODEL

        nls_fit <-
          tryCatch(nls(index ~ b1 + (b2 / (1 + exp(-b3 * (doy - b4)))),
              start = list(
                b1 = min(index),
                b2 = max(index),
                b3 = 0.2,
                b4 = b4_start),
              data = dat),
            error = function(e)
              return(NA))

      if (class(nls_fit) == "nls") {
        
        dat$predict_nls <- predict(nls_fit)
        mse_log <- mean(abs(dat$predict_nls - dat$index)^2)

        nls_fit_result[[k]] <- as.data.frame(data.frame(t(coef(nls_fit)), 
                                                        "plotid"=p,
                                                        "obs_error"= 0,
                                                        "fit_error"= 0,
                                                        "transition" = transition,
                                                        "observations" = length(d$doy),
                                                        "MSE_log" = mse_log,
                                                        "stat_id" = stat_id))
        
      }
      # if class NA:
      else {
        nls_fit_result[[k]] <- as.data.frame(data.frame("b1" = NA,
                                                        "b2" = NA,
                                                        "b3" = NA,
                                                        "b4" =NA, 
                                                        "plotid" = p,
                                                        "obs_error" = 0,
                                                        "fit_error" = 1,
                                                        "transition" = transition,
                                                        "observations" = length(d$doy),
                                                        "MSE_log" = NA,
                                                        "stat_id" = stat_id))
      }
      
      
      #GAM
      
      fit_sp <- tryCatch(gam(index ~ s(doy, sp = 0.005),method="REML", data = dat), error = function(e) return(NA))
      
      # approximation of 1st derivative using finite differences 
      
      if(class(fit_sp) == "gam"){
        
        dat$predict_gam <- predict(fit_sp)
        mse_gam <- mean(abs(dat$predict_gam - dat$index)^2)
        
        newDF <- with(dat, data.frame(doy = seq(0, transition, 1)))  
        
        B <- predict(fit_sp,  newDF, type = "response", se.fit = TRUE)
        
        
        eps <- 1e-7
        X0 <- predict(fit_sp, newDF, type = 'lpmatrix')
        
        newDFeps_p <- newDF + eps
        
        X1 <- predict(fit_sp, newDFeps_p, type = 'lpmatrix')
        
        Xp <- (X0 - X1) / eps
        
        fd_d1 <- Xp %*% coef(fit_sp)
  
        sp_doy <- which.min(fd_d1)
    
      
        sp_fit_result[[k]] <- as.data.frame(data.frame("sp" = sp_doy, 
                                                       "plotid"=p,
                                                       "obs_error_sp" = 0,
                                                       "fit_error_sp" = 0,
                                                       "transition" = transition,
                                                       "observations" = length(d$doy),
                                                       "MSE_gam" = mse_gam,
                                                       "stat_id" = stat_id))
 
        fd_d1 = NULL
        fit_sp = NULL
      }
      # if class NA: 
      else {
        sp_fit_result[[k]] <- as.data.frame(data.frame("sp" = NA, 
                                                       "plotid" = p,
                                                       "obs_error_sp" = 0,
                                                       "fit_error_sp"= 1,
                                                       "transition" = transition,
                                                       "observations" = length(d$doy),
                                                       "MSE_gam" = NA, 
                                                       "stat_id" = stat_id))
      }
    }
    # if observations < 10:
    else {
      sp_fit_result[[k]] <- as.data.frame(data.frame("sp" = NA, 
                                                     "plotid"= p, 
                                                     "obs_error_sp" = 1,
                                                     "fit_error_sp" = 0,
                                                     "transition" = 0,
                                                     "observations" = length(d$doy),
                                                     "MSE_gam" = NA,
                                                     "stat_id" = stat_id))
      
      nls_fit_result [[k]] <- as.data.frame(data.frame("b1" = NA,
                                                       "b2" = NA,
                                                       "b3" = NA,
                                                       "b4" = NA,
                                                       "plotid"=p, 
                                                       "obs_error" = 1,
                                                       "fit_error" = 0,
                                                       "transition" = 0,
                                                       "observations" = length(d$doy),
                                                       "MSE_log" = NA,
                                                       "stat_id" = stat_id))
    }
  }
  return(list(nls_fit_result, sp_fit_result))
}


####################################################################

ptm <- proc.time()
pheno_result_evi <- pheno_model(data_evi$plotid, data_evi$evi, data_evi$doy, data_evi$year, data_evi$dwd_stat)
(proc.time() - ptm) / 60

ptm <- proc.time()
pheno_result_ndvi <- pheno_model(data_ndvi$plotid, data_ndvi$ndvi, data_ndvi$doy, data_ndvi$year, data_ndvi$dwd_stat)
(proc.time() - ptm) / 60
####################################################################

res_nls_evi <- data.frame(do.call(rbind, pheno_result_evi[[1]]))
res_spl_evi <- data.frame(do.call(rbind, pheno_result_evi[[2]]))
results_evi <- merge(res_spl_evi[, c(1:8)], res_nls_evi[, c(4,5,7,10)], by="plotid")

res_nls_ndvi <- data.frame(do.call(rbind, pheno_result_ndvi[[1]]))
res_spl_ndvi <- data.frame(do.call(rbind, pheno_result_ndvi[[2]]))
results_ndvi <- merge(res_spl_ndvi[, c(1:8)], res_nls_ndvi[, c(4,5,7,10)], by="plotid")

# model differences (sample)
results_evi$diff_px <- results_evi$sp - results_evi$b4
results_ndvi$diff_px <- results_ndvi$sp - results_ndvi$b4

map(GDD_SOS[2], mean, na.rm=TRUE)
mean(results_ndvi$diff_px, na.rm=TRUE)
sd(results_ndvi$diff_px, na.rm=TRUE)

mean(results_evi$diff_px, na.rm=TRUE)
sd(results_evi$diff_px, na.rm=TRUE)

# write to disk (sample)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results")
write.csv(results_evi, file = "20190201_results_px_evi.csv", row.names = FALSE)
write.csv(results_ndvi, file = "20190201_results_px_ndvi.csv", row.names = FALSE)

results_ndvi <- read.csv("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_results_px_ndvi.csv")
results_evi <- read.csv("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/20190201_results_px_evi.csv")


########################################################################
# Convergence (sample)

map(results_evi[c(2,9)], function(x) mean(is.na(x)))
map(results_ndvi[c(2,9)], function(x) mean(is.na(x)))


# Correlation (sample)
cor(results_evi$b4, results_evi$sp, use="complete.obs")
cor(results_ndvi$b4, results_ndvi$sp, use="complete.obs")

# Percentiles (sample)
map(results_evi[c(2,9)], function(x) quantile(x, na.rm=TRUE, c(.05, .50, .95)))
map(results_ndvi[c(2,9)], function(x) quantile(x, na.rm=TRUE, c(.05, .50, .95)))


# differences between indices (sample)
results_px <- merge(results_ndvi[, c("plotid","b4","sp","observations", "stat_id")], 
                    results_evi[, c("plotid","b4","sp")], by="plotid")
colnames(results_px) <- c("plotid","LOG_NDVI", "GAM_NDVI","observations", "stat_id", "LOG_EVI","GAM_EVI")
results_px$GAM_diff <- results_px$GAM_NDVI- results_px$GAM_EVI
results_px$LOG_diff <- results_px$LOG_NDVI - results_px$LOG_EVI

results_px$NDVI_diff <- results_px$GAM_NDVI- results_px$LOG_NDVI
results_px$EVI_diff <- results_px$GAM_EVI - results_px$LOG_EVI

cor(results_px$LOG_NDVI, results_px$LOG_EVI, use="complete.obs")
cor(results_px$GAM_NDVI, results_px$GAM_EVI, use="complete.obs")

map(results_px[8:11], mean, na.rm=TRUE)

########################################################################
#Aggregation to plots
mean_evi <- results_evi %>%
  group_by(stat_id) %>%
  summarise_all(funs(mean), na.rm=TRUE)

mean_ndvi <- results_ndvi %>%
  group_by(stat_id) %>%
  summarise_all(funs(mean), na.rm=TRUE)

# no. of sample fits for each station

sample_log_evi <- subset(results_evi, !is.na(b4))
sample_log_evi <- as.data.frame(table(sample_log_evi$stat_id))

sample_log_ndvi <- subset(results_ndvi, !is.na(b4))
sample_log_ndvi <- as.data.frame(table(sample_log_ndvi$stat_id))

sample_gam_ndvi <- subset(results_ndvi, !is.na(sp))
sample_gam_ndvi <- as.data.frame(table(sample_gam_ndvi$stat_id))

sample_gam_evi <- subset(results_evi, !is.na(sp))
sample_gam_evi <- as.data.frame(table(sample_gam_evi$stat_id))

mean(mean(sample_log_evi$Freq),mean(sample_log_ndvi$Freq),mean(sample_gam_ndvi$Freq),mean(sample_gam_evi$Freq))

# model differnces (station)
mean_evi$diff_station <- mean_evi$sp - mean_evi$b4
mean_ndvi$diff_station <- mean_ndvi$sp - mean_ndvi$b4

# add X and Y coordinates
dwd_stations <- read.csv("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/20190120_stations_dwd.csv", header=TRUE)
mean_evi <- merge(mean_evi, dwd_stations[, c("X", "Y","Stations_i")], by.x="stat_id", by.y="Stations_i", all.x=TRUE)
mean_ndvi <- merge(mean_ndvi, dwd_stations[, c("X", "Y", "Stations_i")], by.x="stat_id", by.y="Stations_i", all.x=TRUE)

# write to disk 
write.csv(mean_evi, file="20190201_results_stat_evi.csv",row.names = FALSE )
write.csv(mean_ndvi, file="20190201_results_stat_ndvi.csv",row.names = FALSE )


########################################################################
# mean difference (station)
mean(mean_ndvi$diff_station, na.rm=TRUE)
sd(mean_ndvi$diff_station, na.rm=TRUE)

mean(mean_evi$diff_station)
sd(mean_evi$diff_station)

# mean MSE (station)
mean(mean_evi$MSE_gam)
mean(mean_evi$MSE_log)

mean(mean_ndvi$MSE_gam)
mean(mean_ndvi$MSE_log, na.rm=TRUE)

# Correlation (station)
cor.test(mean_ndvi$b4, mean_ndvi$sp, use="complete.obs")
cor.test(mean_evi$b4, mean_evi$sp, use="complete.obs")

# Percentiles (station)
data.frame("LOG_NDVI" = c(quantile(mean_ndvi$b4, na.rm=TRUE, c(.05, .50, .95))),
            "LOG_EVI"=c(quantile(mean_evi$b4, na.rm=TRUE, c(.05, .50, .95))),
           "GAM_NDVI" = c(quantile(mean_ndvi$sp, na.rm=TRUE, c(.05, .50, .95))),
           "GAM_EVI" = c(quantile(mean_evi$sp, na.rm=TRUE, c(.05, .50, .95)))
           )

# mean SOS and sd

data.frame(c(mean(mean_ndvi$b4, na.rm=TRUE),
             mean(mean_evi$b4),
             mean(mean_ndvi$sp),
             mean(mean_evi$sp)))
         

# differences between indices (station)
mean_results <- merge(mean_evi[, c("stat_id","b4","sp","observations")], 
                      mean_ndvi[, c("stat_id","b4","sp")], by="stat_id")
colnames(mean_results) <- c("stat_id","LOG_EVI", "GAM_EVI","observations","LOG_NDVI","GAM_NDVI")
mean_results$GAM_diff <- mean_results$GAM_NDVI - mean_results$GAM_EVI
mean_results$LOG_diff <- mean_results$LOG_NDVI - mean_results$LOG_EVI

cor.test(mean_results$LOG_NDVI, mean_results$LOG_EVI, use="complete.obs")
cor.test(mean_results$GAM_NDVI, mean_results$GAM_EVI, use="complete.obs")

mean(mean_results$LOG_diff, na.rm=TRUE)
mean(mean_results$GAM_diff, na.rm=TRUE)



