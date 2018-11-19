
library(mgcv)
library(tidyverse)
library(ggplot2)
library(reshape2)


setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/spectral")

data <- read.csv(header=TRUE, sep=",", file="data_clear.csv")
data <- subset(data, data$year == 2017)

data_base <- subset(data, data$doy <= 183)

#################################################################################

# find base doy for evi
base <- data_base %>% group_by(plotid) %>% filter(evi == min(evi))
#base_doy <- data_base %>% group_by(dwd_stat) %>% filter(ndvi == min(ndvi))
data <- merge(data, base[, c("plotid", "doy")], by= "plotid", all.x=TRUE)
names(data)[names(data) == 'doy.y'] <- 'base_doy_evi'
names(data)[names(data) == 'doy.x'] <- 'doy'

ggplot(data=data)+
  geom_histogram(aes(x= base_doy_evi))

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
        ## transition doy 
        slope <- c()
        ddoy <- c()
        transition <- c()
        
        d = subset(data, data$plotid == p &  data$year == "2017")
        print(paste(length(d$doy)))
        k <- k + 1
        if(length(d$doy) >= min_obs){
    
          for(i in 5:(nrow(d) - 5)) {
          dd <- d[(i - 4):(i + 4), ]
          dd$t <- 1:nrow(dd)
          #plot(dd$index ~ dd$t)
          #abline(lm(dd$index ~ dd$t))
          slope <- c(slope, coef(lm(dd$index ~ dd$t))[2])
          ddoy <- c(ddoy, d[i, "doy"])
        }
      
        
          transition <- c(transition, unlist(ddoy)[which(slope < 0 & 
                                                       dplyr::lead(slope) < 0 & 
                                                       dplyr::lead(slope, 2) < 0)[1]])
          #if(transition <  )
          #print(paste("transition",transition))
       
        
          base_doy <- d$base_doy_index[1]
        
        
          dat <- subset(d, d$doy <= transition & d$doy >= base_doy) 
        
          #print(paste("length:", length(dat$doy)))
        
        
          #LOGISTIC FIT
        
          
          nls_fit <- tryCatch(nls(index ~ b1 + (b2 / (1 + exp(- b3 * (doy - b4)))),
                            start = list(b1 = min(index), 
                                         b2 = max(index), 
                                         b3 = 0.2, 
                                         b4 = round(mean(dat[which(dat$index > median(dat$index)), "doy"]), 0)),
                            data = dat), error = function(e) return(NA))
  
          if (class(nls_fit) == "nls") {
          print(paste(round(coef(nls_fit), 2)))
          nls_fit_result[[k]] <- as.data.frame(data.frame(t(coef(nls_fit)), 
                                               "plotid"=p,
                                               "obs_error"= 0,
                                               "fit_error"= 0,
                                               "transition" = transition,
                                               "observations" = length(d$doy),
                                               "RSS" = sum(resid(nls_fit)^2) ))
        
          }
       
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

          
            newDF <- with(dat, data.frame(doy = seq(base_doy, transition, 1)))  
          
            B <- predict(fit_sp,  newDF, type = "response", se.fit = TRUE)
          
        
            eps <- 1e-7
            X0 <- predict(fit_sp, newDF, type = 'lpmatrix')
            
            newDFeps_p <- newDF + eps
            
            X1 <- predict(fit_sp, newDFeps_p, type = 'lpmatrix')
            
            Xp <- (X0 - X1) / eps
            
            fd_d1 <- Xp %*% coef(fit_sp)
            
            sp_doy <- which.min(fd_d1) + base_doy -1
            
            print(paste("spline_doy: ",sp_doy ))
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
pheno_result <- pheno_model(data$plotid, data$evi, data$doy, data$year, data$base_doy_evi)
proc.time() - ptm

res_nls <- data.frame(do.call(rbind, pheno_result[[1]]))
res_spl <- data.frame(do.call(rbind, pheno_result[[2]]))
results <- cbind(res_nls, res_spl[,c(1,3,4)])


mean(!is.na(results$b4))
mean(!is.na(results$sp))

cor(results$b4, results$sp, use="complete.obs")
################################################################################# 

#### Results 
ggplot(data=results)+
  geom_histogram(aes(x=b4), binwidth= 1)
ggplot(data=results)+
  geom_histogram(aes(x=sp), binwidth=1)

sub_res <- results[, c("b4","sp")]
df_m <- melt(sub_res)

ggplot(data= results)+
  geom_histogram(aes(x= sp))

ggplot(data= df_m, aes(x=variable, y=value))+
  geom_boxplot(aes(fill=variable))

#### Elevation 
ggplot(data = data)+
  geom_histogram(aes(x=base_doy_ndvi), binwidth= 1)

ggplot(data=data)+
  geom_histogram(aes(x=elevation_diff), binwidth=20)



## EVI
ggplot(data=data)+
  geom_histogram(aes(x=evi), binwidth= 2)

######################################### single plots ##################

results_check <- subset(results, results$b4 < 200 & results$sp < 75) 
results_check <- results_check %>% arrange(b4, sp)


plot_id <- 626509


dat_plot <- subset(data, data$plotid == plot_id)



base_index <- mean(subset(dat_plot, dat_plot$doy <= 50)$evi)
dat_plot$evi[dat_plot$doy <= 50] <- base_index

ggplot(data=dat_plot, aes(x=doy, y=evi))+
  geom_point(size= 1)+
  geom_line(size=0.7)+
  geom_text(aes(label=qa_saturation))


ggplot(results, aes(x = sp, y = b4)) + 
  geom_point() + 
  coord_equal() + 
  geom_abline(intercept = 0, slope = 1)
  #geom_text(aes(label = plotid))
  #labs(x="DOY (GAM)", y="DOY (LOG)")

# problem splines: highest slope is not SOS 
# - 398720, 
# - 634424
# - 145114
# - 145124
# - 16106

# S2 problem: 626317

# base missing:
# - 145114
# - 145124
# - 260029




  
