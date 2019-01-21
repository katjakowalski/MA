# 

library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file="data_clear.csv")

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
    #print(paste("plot: ", p))
    
    transition <- c()
    b4_start <- c()
    
    d = subset(data, data$plotid == p &  data$year == "2017")
  
    stat_id <- d$stat_id[1]

    k <- k + 1
    
    if(length(d$doy) >= min_obs){
      
      d_tr <- subset(d, d$doy >= 75 & d$doy <= 250) ## 250 before 
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
                                                       "stat_id" = stat_id,
                                                       "GCV_gam" = as.numeric(fit_sp$gcv.ubre)))
 
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
                                                       "stat_id" = stat_id,
                                                       "GCV_gam" = NA))
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
                                                     "stat_id" = stat_id,
                                                     "GCV_gam" =NA))
      
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

res_nls_evi <- data.frame(do.call(rbind, pheno_result_evi_k[[1]]))
res_spl_evi <- data.frame(do.call(rbind, pheno_result_evi_k[[2]]))
results_evi <- merge(res_spl_evi[, c(1:9)], res_nls_evi[, c(4,5,7,10)], by="plotid")

res_nls_ndvi <- data.frame(do.call(rbind, pheno_result_ndvi_[[1]]))
res_spl_ndvi <- data.frame(do.call(rbind, pheno_result_ndvi_[[2]]))
results_ndvi <- merge(res_spl_ndvi[, c(1:9)], res_nls_ndvi[, c(4,5,7,10)], by="plotid")

# model differences (sample)
results_evi$diff_px <- abs(results_evi$sp - results_evi$b4)
results_ndvi$diff_px <- abs(results_ndvi$sp - results_ndvi$b4)

mean(results_ndvi$diff_px, na.rm=TRUE)
mean(results_evi$diff_px, na.rm=TRUE)

# write to disk (sample)
write.csv(results_evi, file = "20190117_results_px_evi.csv", row.names = FALSE)
write.csv(results_ndvi, file = "20190117_results_px_ndvi.csv", row.names = FALSE)

########################################################################
# Convergence (sample)
mean(!is.na(results_evi$b4))
mean(!is.na(results_evi$sp))

mean(!is.na(results_ndvi$b4))
mean(!is.na(results_ndvi$sp))

# Correlation (sample)
cor.test(results_evi$b4, results_evi$sp, use="complete.obs")
cor.test(results_ndvi$b4, results_ndvi$sp, use="complete.obs")

# Percentiles (sample)
quantile(results_evi$b4, na.rm=TRUE, c(.05, .50, .95))
quantile(results_evi$sp, na.rm=TRUE, c(.05, .50, .95))
quantile(results_ndvi$b4, na.rm=TRUE, c(.05, .50, .95))
quantile(results_ndvi$sp, na.rm=TRUE, c(.05, .50, .95))


# differences between indices (sample)
results_px <- merge(results_ndvi[, c("plotid","b4","sp","observations", "stat_id")], 
                    results_evi[, c("plotid","b4","sp")], 
                    by="plotid")
colnames(results_px) <- c("plotid","LOG_NDVI", "GAM_NDVI","observations", "stat_id", "LOG_EVI","GAM_EVI")
results_px$GAM_diff <- abs(results_px$GAM_NDVI- results_px$GAM_EVI)
results_px$LOG_diff <- abs(results_px$LOG_NDVI - results_px$LOG_EVI)

results_px$NDVI_diff <- abs(results_px$GAM_NDVI- results_px$LOG_NDVI)
results_px$EVI_diff <- abs(results_px$GAM_EVI - results_px$LOG_EVI)

cor.test(results_px$LOG_NDVI, results_px$LOG_EVI, use="complete.obs")
cor.test(results_px$GAM_NDVI, results_px$GAM_EVI, use="complete.obs")

mean(results_px$GAM_diff, na.rm=TRUE)
mean(results_px$LOG_diff, na.rm=TRUE)

mean(results_px$NDVI_diff, na.rm=TRUE)
mean(results_px$EVI_diff, na.rm=TRUE)

########################################################################
#Aggregation to stations
completeVec <- complete.cases(results_evi[, c("sp", "b4")])
compl_evi <- results_evi[completeVec, ]
compl_evi$plotid <- NULL
mean_evi <- aggregate(. ~ stat_id, data=compl_evi, mean)

completeVec <- complete.cases(results_ndvi[, c("sp","b4")])
compl_ndvi <- results_ndvi[completeVec, ]
compl_ndvi$plotid <- NULL
mean_ndvi <- aggregate(. ~ stat_id, data=compl_ndvi, mean)

# no. of sample fits for each station
px_station_evi <- data.frame(table(compl_evi$stat_id))
px_station_ndvi <- data.frame(table(compl_ndvi$stat_id))

median(px_station_evi$Freq)
mean(px_station_ndvi$Freq)

px_station <- merge(px_station_evi, px_station_ndvi, by="Var1")
colnames(px_station) <- c("stat_id", "Freq_EVI", "Freq_NDVI")
px_stat <- melt(px_station,id.vars="stat_id")

ggplot(px_stat)+
  geom_boxplot(aes(y=value, fill=variable))

# model differnces (station)
mean_evi$diff_station <- abs(mean_evi$sp - mean_evi$b4)
mean_ndvi$diff_station <- abs(mean_ndvi$sp - mean_ndvi$b4)

# write to disk 
write.csv(mean_evi, file="20190117_results_stat_evi.csv",row.names = FALSE )
write.csv(mean_ndvi, file="20190117_results_stat_ndvi.csv",row.names = FALSE )

########################################################################
# mean difference (station)
mean(mean_ndvi$diff_station)
mean(mean_ndvi$diff_px)
mean(mean_evi$diff_px)
mean(mean_evi$diff_station)

# mean MSE (station)
mean(mean_evi$MSE_gam)*1000
mean(mean_evi$MSE_log)*1000

mean(mean_ndvi$MSE_gam)*1000
mean(mean_ndvi$MSE_log)*1000

# Correlation (station)
cor.test(mean_ndvi$b4, mean_ndvi$sp, use="complete.obs")
cor.test(mean_evi$b4, mean_evi$sp, use="complete.obs")

# Percentiles (station)
quantile(mean_evi$b4, na.rm=TRUE, c(.05, .50, .95))
quantile(mean_ndvi$b4, na.rm=TRUE, c(.05, .50, .95))

quantile(mean_evi$sp, na.rm=TRUE, c(.05, .50, .95))
quantile(mean_ndvi$sp, na.rm=TRUE, c(.05, .50, .95))


# differences between indices (station)
mean_results <- merge(mean_evi[, c("stat_id","b4","sp","observations")], 
                      mean_ndvi[, c("stat_id","b4","sp")], by="stat_id")
colnames(mean_results) <- c("stat_id","LOG_EVI", "GAM_EVI","observations","LOG_NDVI","GAM_NDVI")
mean_results$GAM_diff <- abs(mean_results$GAM_NDVI- mean_results$GAM_EVI)
mean_results$LOG_diff <- abs(mean_results$LOG_NDVI - mean_results$LOG_EVI)

cor.test(mean_results$LOG_NDVI, mean_results$LOG_EVI, use="complete.obs")
cor.test(mean_results$GAM_NDVI, mean_results$GAM_EVI, use="complete.obs")

mean(mean_results$LOG_diff)
mean(mean_results$GAM_diff)



