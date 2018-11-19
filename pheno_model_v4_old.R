library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file="data_clear.csv")

data <- subset(data, data$evi < 1.1 & data$evi >= 0)

data <- subset(data, data$year == 2017)

#################################################################################
length(unique(data$plotid))

pheno_model <- function(plotid, 
                        index, 
                        doy, 
                        year, 
                        min_obs = 10){
  
  data = data.frame(plotid,
                    index, 
                    doy, 
                    year)      
  
  l_samples <- length(unique(data$plotid))             
  
  nls_fit_result <- vector("list", l_samples)          
  sp_fit_result <- vector("list", l_samples)
  k <- 0
  
  for( p in unique(data$plotid)){
    #print(paste("plot: ", p))
    transition <- c()
    
    dat = subset(data, data$plotid == p &  data$year == "2017")
    stat_id = dat$stat_id[1]
    
    transition <- with(dat, doy[index == max(index)]) + 20
    #print(transition)
    dat <- subset(dat, dat$doy <= transition) 
    
    
    #print(paste(length(d$doy)))
    k <- k + 1
    if(length(dat$doy) >= min_obs){
      
      base_index <- mean(subset(dat, dat$doy <= 50)$index)
      
      if (is.nan(base_index)){
        base_index <- min(dat$index)
      }
      
      dat <- dat[!dat$doy <= 50,]                              # delete all rows < doy 50
      
      df_base <- dat[0,]                                   # create empty df with column names
      df_base[c(1:50), ] <- rep(NA, ncol(df_base))       # fill 50 rows with NA 
      df_base$index <- base_index
      df_base$doy <- seq(1,50,1)
      dat <- rbind(dat, df_base)
      
      b4_start <- round(mean(dat[which(dat$index > median(dat$index)), "doy"]), 0)
      
      #LOGISTIC FIT
      #par_b3 <- seq(0.05, 0.9, 0.05)
      #for (i in par_b3) {
      nls_fit <-
        tryCatch(nls(index ~ b1 + (b2 / (1 + exp(-b3 * (doy - b4)))),
                     start = list(
                       b1 = min(index),
                       b2 = max(index),
                       b3 = 0.2,
                       b4 = round(mean(dat[which(dat$index > median(dat$index)), "doy"]), 0)),
                     data = dat),
                 error = function(e)
                   return(NA))
      # if (class(nls_fit) == "nls")
      #    break
      
      #}
      
      if (class(nls_fit) == "nls") {
        
        nls_fit_result[[k]] <- as.data.frame(data.frame(t(coef(nls_fit)), 
                                                        "plotid"=p,
                                                        "obs_error"= 0,
                                                        "fit_error"= 0,
                                                        "transition" = transition,
                                                        "observations" = length(dat$doy),
                                                        "RSS_log" = sum(resid(nls_fit)^2),
                                                        "RSS_gam" = NA,
                                                        "b4_start" = b4_start))
        
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
                                                        "observations" = length(dat$doy),
                                                        "RSS_log" = NA,
                                                        "RSS_gam"= NA,
                                                        "b4_start" = b4_start))
      }
      
      
      #SPLINE FIT  
      
      
      fit_sp <- tryCatch(gam(index ~ s(doy), data = dat), error = function(e) return(NA))
      
      ## 1st derivative to estimate slope 
      
      if(class(fit_sp) == "gam"){
        
        
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
                                                       "observations" = length(dat$doy),
                                                       "RSS_gam" = sum(resid(fit_sp)^2),
                                                       "RSS_log" = NA,
                                                       "b4_start" = b4_start))
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
                                                       "observations" = length(dat$doy),
                                                       "RSS_gam" = NA, 
                                                       "RSS_log" = NA,
                                                       "b4_start" = b4_start))
      }
    }
    # if observations < 10 
    else {
      sp_fit_result[[k]] <- as.data.frame(data.frame("sp" = NA, 
                                                     "plotid"= p, 
                                                     "obs_error_sp" = 1,
                                                     "fit_error_sp" = 0,
                                                     "transition" = 0,
                                                     "observations" = length(dat$doy),
                                                     "RSS_gam" = NA,
                                                     "RSS_log" = NA,
                                                     "b4_start" = b4_start))
      
      nls_fit_result [[k]] <- as.data.frame(data.frame("b1" = NA,
                                                       "b2" = NA,
                                                       "b3" = NA,
                                                       "b4" = NA,
                                                       "plotid"=p, 
                                                       "obs_error" = 1,
                                                       "fit_error" = 0,
                                                       "transition" = 0,
                                                       "observations" = length(dat$doy),
                                                       "RSS_log" = NA,
                                                       "RSS_gam" = NA,
                                                       "b4_start" = b4_start))
    }
  }
  return(list(nls_fit_result, sp_fit_result))
}


#################################################################################
ptm <- proc.time()
pheno_result_evi_old <- pheno_model(data$plotid, data$evi, data$doy, data$year)
(proc.time() - ptm) / 60



res_nls_evi_old <- data.frame(do.call(rbind, pheno_result_evi_old[[1]]))
res_spl_evi_old <- data.frame(do.call(rbind, pheno_result_evi_old[[2]]))
results_evi_old <- cbind(res_nls_evi_old, res_spl_evi_old[,c(1,4)])



mean(!is.na(results_evi_old$b4))
mean(!is.na(results_evi_old$sp))



cor.test(results_evi_old$b4, results_evi_old$sp, use="complete.obs")


results_sub <- subset(results_v4, results_v4$elevation_sample >= 500 & 
                        results_v4$elevation_sample <= 1000)


ggplot(data= results_evi_old, aes(x =sp, y =b4)) + 
  geom_point()+ 
  #geom_text(aes(label = plotid))
  labs(x="SOS (GAM)", y="SOS (LOG)")

ggplot(results_evi_old, aes(x = sp, y =b4)) + 
  geom_point()+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="DOY (GAM)", y="DOY (LOG)")


sub_res_v4 <- results_v4[, c("b4","sp")]
df_v4 <- melt(sub_res_v4)

ggplot(data= results_v4)+
  geom_histogram(aes(x= b4))

ggplot(data= df_v4, aes(x=variable, y=value))+
  geom_boxplot()+
  stat_boxplot(geom="errorbar", width=0.5)

boxplot(results_v4[, c("b4", "sp")])

quantile(results_v4$b4, na.rm=TRUE, c(.05, .50,  .75, .95))
quantile(results_v4$sp, na.rm=TRUE, c(.05, .50,  .75, .95))


completeVec <- complete.cases(results_v4[, c("b4","sp")])
results_compl <- results_v4[completeVec, ]


gam_mean <- aggregate(sp ~ station, data=results_compl, mean)
log_mean <- aggregate(b4 ~ station, data=results_compl, mean)
RSS_sp_mean <- aggregate(RSS_gam ~ station, data=results_compl, mean)
RSS_log_mean <- aggregate(RSS_log ~ station, data=results_compl, mean)

res_v4 <- merge(gam_mean, log_mean, by = "station")
res_v4 <- merge(res_v4, RSS_mean, by="station")

ggplot(data=res_v4)+
  geom_point(aes(x=b4, y=RSS))

ggplot(data=res_v4)+
  geom_histogram(aes(x=RSS))
