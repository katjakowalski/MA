library(ggplot2)




# Scatterplot px level

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181120_EVI.png", 
    width= 1200, height=1000, res=200 )
ggplot(results_evi, aes(x = sp, y =b4)) + 
  geom_point(alpha=1/20)+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1, color="red")+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 50), 
                     breaks= seq(0,350, 50),
                     limits = c(50,250))+
  scale_y_continuous(limits=c(50,250))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) 

dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181123_NDVI.png", 
    width= 1200, height=1000, res=200 )
ggplot(results_ndvi, aes(x = sp, y =b4)) + 
  geom_point(alpha=1/20)+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1, color="red")+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 50), 
                     breaks= seq(0,350, 50),
                     limits=c(50,300))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14),
        legend.position="none") 

dev.off()

# differences EVI GAM vs. LOG

ggplot(data=results_evi)+
  geom_histogram(aes(diff),binwidth=1, fill="lightgrey", color="black")

ggplot(data=results_evi)+
  geom_boxplot(aes(y=diff))

quantile(results_evi$diff, na.rm=TRUE, c(0,.05, .50,  .75, .95))

# MSE 
ggplot(data=mean_evi)+
  geom_point(aes(x=sp, y=MSE_gam))

ggplot(data=mean_evi)+
  geom_point(aes(x=b4, y=MSE_log))

ggplot(data=mean_evi)+
  geom_histogram(aes(MSE_gam))

ggplot(data=mean_evi)+
  geom_histogram(aes(MSE_log))



# histogram px level 
estimates_evi <- results_evi[, c("b4","sp")]
estimates_evi <- melt(estimates_evi)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181123_EVI_hist.png", 
    width= 1200, height=1000, res=200 )
ggplot(estimates_evi, aes(x=value, fill=variable)) + 
  geom_histogram(alpha=0.6, position="identity", binwidth=1)+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16))+
  scale_fill_manual(values=c("gray0", "sienna3"), 
                    labels=c("LOG", "GAM"),
                    name=c("Model"))+
  scale_x_continuous(limits=c(75,200),
                     breaks=seq(80,200,20))+
  labs(x="DOY", y= "Count")
dev.off()


# scatterplot station level

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181123_EVI_station.png", 
    width= 1200, height=1000, res=200 )

ggplot(mean_evi) + 
  geom_point(aes(x= sp, y=b4),color="darkblue", alpha=1/5)+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(80,160))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(80,160))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) 

dev.off()

# histogram station level

estimates_evi_stat <- mean_evi[, c("b4","sp")]
estimates_evi_stat <- melt(estimates_evi_stat)

estimates_ndvi_stat <- mean_ndvi[, c("b4","sp")]
estimates_ndvi_stat <- melt(estimates_ndvi_stat)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181124_EVI_hist_station.png", 
    width= 1200, height=1000, res=200 )
ggplot(estimates_evi_stat, aes(x=value, fill=variable)) + 
  geom_histogram(alpha=0.6, position="identity", binwidth=1)+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16))+
  scale_fill_manual(values=c("gray0", "sienna3"), 
                    labels=c("LOG", "GAM"),
                    name=c("Model"))+
  scale_x_continuous(limits=c(75,200),
                     breaks=seq(80,200,20))+
  labs(x="DOY", y= "Count")
dev.off()


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181124_NDVI_hist_station.png", 
    width= 1200, height=1000, res=200 )
ggplot(estimates_ndvi_stat, aes(x=value, fill=variable)) + 
  geom_histogram(alpha=0.6, position="identity", binwidth=1)+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16))+
  scale_fill_manual(values=c("gray0", "sienna3"), 
                    labels=c("LOG", "GAM"),
                    name=c("Model"))+
  scale_x_continuous(limits=c(75,200),
                     breaks=seq(80,200,20))+
  labs(x="DOY", y= "Count")
dev.off()

# percent converged 
model_fits <- data.frame(
  Convergence = c(mean(!is.na(results_evi$b4)),
            mean(!is.na(results_evi$sp)),
            mean(!is.na(results_ndvi$b4)),
            mean(!is.na(results_ndvi$sp))),
  Index = c("EVI", "EVI", "NDVI", "NDVI"),
  Model = c ("LOG", "GAM", "LOG", "GAM")
)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181127_convergence.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=model_fits)+
  geom_bar(aes(x= Index, y=Convergence, fill=Model), 
           stat="identity", position = position_dodge2(),
           width = 0.5, color="black")+
  scale_fill_manual(values=c( "sienna3", "grey28"))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16))

dev.off()

