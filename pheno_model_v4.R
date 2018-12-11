library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany")

data <- read.csv(header=TRUE, sep=",", file="data_clear.csv")

data_evi <- subset(data, data$evi < 1.1 & data$evi >= 0 & data$year == 2017)

data_ndvi <- subset(data, data$ndvi < 1.1 & data$ndvi >= 0 & data$year == 2017)

length(unique(data_evi$plotid))
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
      
      #d_tr <- subset(d, d$doy >= 75)
      #transition <- with(d_tr, doy[index == max(index)]) + 20
      #dat <- subset(d, d$doy <= transition)

      #LOGISTIC FIT
      #par_b3 <- seq(0.05, 0.9, 0.05)
      #for (i in par_b3) {
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
       # if (class(nls_fit) == "nls")
      #    break
        
      #}
      
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
      
      
      #SPLINE FIT  
      
      
      fit_sp <- tryCatch(gam(index ~ s(doy, sp=0.005), data = dat), error = function(e) return(NA))
      
      ## 1st derivative to estimate slope 
      
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
        #print(paste(sum(residuals(fit_sp)^2)))
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
    # if observations < 10 
    else {
      sp_fit_result[[k]] <- as.data.frame(data.frame("sp" = NA, 
                                                     "plotid"= p, 
                                                     "obs_error_sp" = 1,
                                                     "fit_error_sp" = 0,
                                                     "transition" = 0,
                                                     "observations" = length(d$doy),
                                                     "MSE_gam" = NA,
                                                     "stat_id" = stat_id,
                                                     "GCV_gam" =NA ))
      
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
pheno_result_evi_k <- pheno_model(data_evi$plotid, data_evi$evi, data_evi$doy, data_evi$year, data_evi$dwd_stat)
(proc.time() - ptm) / 60

ptm <- proc.time()
pheno_result_ndvi_ <- pheno_model(data_ndvi$plotid, data_ndvi$ndvi, data_ndvi$doy, data_ndvi$year, data_ndvi$dwd_stat)
(proc.time() - ptm) / 60


res_nls_evi <- data.frame(do.call(rbind, pheno_result_evi_k[[1]]))
res_spl_evi <- data.frame(do.call(rbind, pheno_result_evi_k[[2]]))
results_evi <- merge(res_spl_evi[, c(1:9)], res_nls_evi[, c(4,5,7,10)], by="plotid")

res_nls_ndvi_k <- data.frame(do.call(rbind, pheno_result_ndvi_[[1]]))
res_spl_ndvi_k <- data.frame(do.call(rbind, pheno_result_ndvi_[[2]]))
results_ndvi_k <- merge(res_spl_ndvi_k[, c(1:9)], res_nls_ndvi_k[, c(4,5,7,10)], by="plotid")

########################################################################

mean(!is.na(results_evi$b4))
mean(!is.na(results_evi$sp))

mean(!is.na(results_ndvi_k$b4))
mean(!is.na(results_ndvi_k$sp))

mean(!is.na(results_ndvi$b4))
mean(!is.na(results_ndvi$sp))

write.csv(results_evi, file = "20181204_results_evi.csv", row.names = FALSE)
write.csv(results_ndvi, file = "20181204_results_ndvi.csv", row.names = FALSE)


# Correlation

cor.test(results_evi$b4, results_evi$sp, use="complete.obs")
cor.test(results_ndvi$b4, results_ndvi$sp, use="complete.obs")

# Quantiles 
# quantile(results_evi$b4, na.rm=TRUE, c(.05, .50,  .75, .95))
# quantile(results_evi$sp, na.rm=TRUE, c(.05, .50,  .75, .95))
# quantile(results_ndvi$b4, na.rm=TRUE, c(.05, .50,  .75, .95))
# quantile(results_ndvi$sp, na.rm=TRUE, c(.05, .50,  .75, .95))

# differences LOG vs. GAM
results_evi$b4_f <- round(results_evi$b4,0)
results_evi$diff <- abs(results_evi$sp - results_evi$b4)

results_ndvi$b4_f <- round(results_ndvi$b4,0)
results_ndvi$diff <- abs(results_ndvi$sp - results_ndvi$b4)

# results station dwd 

completeVec <- complete.cases(results_evi[, c("sp","b4")])
compl_evi <- results_evi[completeVec, ]
compl_evi$plotid <- NULL
mean_evi <- aggregate(. ~ stat_id, data=compl_evi, mean)

completeVec <- complete.cases(results_ndvi[, c("sp","b4")])
compl_ndvi <- results_ndvi[completeVec, ]
compl_ndvi$plotid <- NULL
mean_ndvi <- aggregate(. ~ stat_id, data=compl_ndvi, mean)

mean_evi$b4_f <- round(mean_evi$b4,0)
mean_evi$diff <- abs(mean_evi$sp - mean_evi$b4)

mean_ndvi$b4_f <- round(mean_ndvi$b4,0)
mean_ndvi$diff <- abs(mean_ndvi$sp - mean_ndvi$b4)

mean(mean_ndvi$diff)
mean(mean_evi$diff)

mean(mean_evi$MSE_gam)*1000
mean(mean_evi$MSE_log)*1000

mean(mean_ndvi$MSE_gam)*1000
mean(mean_ndvi$MSE_log)*1000


############################################################################

cor.test(mean_ndvi$b4, mean_ndvi$sp, use="complete.obs")
cor.test(mean_evi$b4, mean_evi$sp, use="complete.obs")


quantile(mean_evi$b4, na.rm=TRUE, c(.05, .50,  .75, .95))
quantile(mean_ndvi$b4, na.rm=TRUE, c(.05, .50,  .75, .95))

quantile(mean_evi$sp, na.rm=TRUE, c(.05, .50,  .75, .95))
quantile(mean_ndvi$sp, na.rm=TRUE, c(.05, .50,  .75, .95))

quantiles <- data.frame(pct5=c(quantile(mean_evi$b4, na.rm=TRUE, .05),
                              quantile(mean_evi$sp, na.rm=TRUE, .05),
                              quantile(mean_ndvi$sp, na.rm=TRUE, .05),
                              quantile(mean_ndvi$b4, na.rm=TRUE, .05)),
                        pct95 = c(quantile(mean_evi$b4, na.rm=TRUE, .95),
                                 quantile(mean_evi$sp, na.rm=TRUE, .95),
                                 quantile(mean_ndvi$sp, na.rm=TRUE, .95),
                                 quantile(mean_ndvi$b4, na.rm=TRUE, .05)),
                        Index = c("EVI","EVI","NDVI","NDVI"),
                        Model = c("LOG","GAM", "GAM", "LOG" ))

cor.test(results_ndvi$sp, results_ndvi$transition, use="complete.obs")


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany")
stations <- read.csv(header=TRUE, sep=",", file="stations.csv")
colnames(stations)[1] <- "stat_id"
mean_evi <- merge(mean_evi, stations[, c("Stationsho", "stat_id")],by="stat_id", all.x=TRUE)
mean_ndvi <- merge(mean_ndvi, stations[, c("Stationsho", "stat_id")],by="stat_id", all.x=TRUE)



# observations & model differences 
results_px <- merge(results_ndvi[, c("plotid","b4","sp","observations")], 
                    results_evi[, c("plotid","b4","sp")], 
                    by="plotid")
colnames(results_px) <- c("plotid","LOG_NDVI", "GAM_NDVI","observations","LOG_EVI","GAM_EVI")
results_px$GAM_diff <- abs(results_px$GAM_NDVI- results_px$GAM_EVI)
results_px$LOG_diff <- abs(results_px$LOG_NDVI - results_px$LOG_EVI)

cor.test(results_px$GAM_diff, results_px$observations, use="complete.obs")
cor.test(results_px$LOG_diff, results_px$observations, use="complete.obs")

ggplot(data=results_px)+
  geom_point(aes(x=observations, y=LOG_diff), alpha=1/10)


# differences between indices 

mean_results <- merge(mean_evi[, c("stat_id","b4","sp","observations")], 
                      mean_ndvi[, c("stat_id","b4","sp")], 
                      by="stat_id")
colnames(mean_results) <- c("stat_id","LOG_EVI", "GAM_EVI","observations",
                            "LOG_NDVI","GAM_NDVI")

cor.test(mean_results$LOG_NDVI, mean_results$LOG_EVI, use="complete.obs")
cor.test(mean_results$GAM_NDVI, mean_results$GAM_EVI, use="complete.obs")


