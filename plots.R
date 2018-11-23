library(ggplot2)




# Scatterplot px level

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181120_EVI.png", 
    width= 1200, height=1000, res=200 )
ggplot(results_evi, aes(x = sp, y =b4)) + 
  geom_point(alpha=1/20)+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
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
  geom_abline(intercept = 0, slope = 1)+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) 

dev.off()


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

ggplot(mean_evi, aes(x = sp, y =b4)) + 
  geom_point()+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 50), breaks= seq(0,350, 50))+
  theme(axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"),
        text = element_text(size=16),
        legend.text=element_text(size=14)) 
