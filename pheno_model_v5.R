library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file="data_clear.csv")
data <- subset(data, data$year == 2017)

#################################################################################
# find base doy for evi
base <- data_base %>% group_by(plotid) %>% filter(evi == min(evi))
#base_doy <- data_base %>% group_by(dwd_stat) %>% filter(ndvi == min(ndvi))
data <- merge(data, base[, c("plotid", "doy")], by= "plotid", all.x=TRUE)
names(data)[names(data) == 'doy.y'] <- 'base_doy_evi'
names(data)[names(data) == 'doy.x'] <- 'doy'

pheno_model <- function(plotid, 
                        index, 
                        doy, 
                        year, 
                        base_doy_index, 
                        min_obs = 10){
  
  data = data.frame(plotid, 
                    index, 
                    doy, 
                    year,
                    base_doy_index)      
  
  l_samples <- length(unique(data$plotid))             
  
  nls_fit_result <- vector("list", l_samples)          
  sp_fit_result <- vector("list", l_samples)
  k <- 0
  
  for( p in unique(data$plotid)){
    print(paste("plot: ", p))
    
    transition <- c()
    
    d = subset(data, data$plotid == p &  data$year == "2017")
    #print(paste(length(d$doy)))
    k <- k + 1
    if(length(d$doy) >= min_obs){
      
      transition <- with(d, doy[index == max(index)]) + 10
      
      base_doy <- d$base_doy_index[1]
      
      dat <- subset(d, d$doy <= transition & d$doy >= base_doy) 
      
      #print(paste("length:", length(dat$doy)))
      
      
      #LOGISTIC FIT
      #par_b3 <- seq(0.1, 0.9, 0.5)
      #for(i in par_b3){
      nls_fit <- tryCatch(nls(index ~ b1 + (b2 / (1 + exp(- b3 * (doy - b4)))),
                              start = list(b1 = min(index), 
                                           b2 = max(index), 
                                           b3 = 0.2, 
                                           b4 = round(mean(dat[which(dat$index > median(dat$index)), "doy"]), 0)),
                              data = dat), error = function(e) return(NA))
      # if (class(nls_fit) == "nls"){
      #  break 
      # }
      
      #}
      
      if (class(nls_fit) == "nls") {
        #print(paste(round(coef(nls_fit), 2)))
        nls_fit_result[[k]] <- as.data.frame(data.frame(t(coef(nls_fit)), 
                                                        "plotid"=p,
                                                        "obs_error"= 0,
                                                        "fit_error"= 0,
                                                        "transition" = transition,
                                                        "observations" = length(d$doy),
                                                        "RSS" = sum(resid(nls_fit)^2) ))
        
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
                                                        "RSS" = NA))
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
        
        sp_doy <- which.min(fd_d1) + base_doy -1
        
        #print(paste("spline_doy: ",sp_doy ))
        sp_fit_result[[k]] <- as.data.frame(data.frame("sp" = sp_doy, 
                                                       "plotid"=p,
                                                       "obs_error_sp" = 0,
                                                       "fit_error_sp" = 0,
                                                       "transition" = transition,
                                                       "observations" = length(d$doy),
                                                       "RSS" = sum(resid(fit_sp)^2) ))
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
                                                       "RSS" = NA ))
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
                                                     "RSS" = NA ))
      
      nls_fit_result [[k]] <- as.data.frame(data.frame("b1" = NA,
                                                       "b2" = NA,
                                                       "b3" = NA,
                                                       "b4" = NA,
                                                       "plotid"=p, 
                                                       "obs_error" = 1,
                                                       "fit_error" = 0,
                                                       "transition" = 0,
                                                       "observations" = length(d$doy),
                                                       "RSS" = NA ))
    }
  }
  return(list(nls_fit_result, sp_fit_result))
}


#################################################################################
ptm <- proc.time()
pheno_result_v5 <- pheno_model(data$plotid, data$evi, data$doy, data$year, data$base_doy_evi)
(proc.time() - ptm) / 60

res_nls_v5 <- data.frame(do.call(rbind, pheno_result_v5[[1]]))
res_spl_v5 <- data.frame(do.call(rbind, pheno_result_v5[[2]]))
results_v5 <- cbind(res_nls_v5, res_spl_v5[,c(1,3,4)])


mean(!is.na(results_v5$b4))
mean(!is.na(results_v5$sp))



cor.test(results_v5$b4, results_v5$sp, use="complete.obs")

range(results_v5$b4, na.rm=TRUE)

ggplot(results_v5, aes(x = sp, y = b4)) + 
  geom_point() + 
  coord_equal() + 
  geom_abline(intercept = 0, slope = 1)+
  #geom_text(aes(label = plotid))
  labs(x="DOY (GAM)", y="DOY (LOG)")



sub_res_v2 <- results_v2[, c("b4","sp")]
df_v2 <- melt(sub_res_v2)

ggplot(data= results)+
  geom_histogram(aes(x= sp))

ggplot(data= df_v2, aes(x=variable, y=value))+
  geom_boxplot()+
  stat_boxplot(geom="errorbar", width=0.5)
