library(ggplot2)
library(reshape2)



# Scatterplot px level

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181204_EVI.svg", 
    width=7, 
    height=7,
    pointsize = 96) 

ggplot(data=results_evi, aes(x = sp, y =b4)) + 
  geom_point(alpha=1/20,color="darkblue")+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1, color="black")+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 50), 
                     breaks= seq(0,350, 50),
                     limits = c(50,250))+
  scale_y_continuous(limits=c(50,250))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=18)) +
  ggtitle("EVI")

dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181204_NDVI.png", 
    width= 1200, height=1000, res=200 )
ggplot(results_ndvi, aes(x = sp, y =b4)) + 
  geom_point(alpha=1/20, color="darkblue")+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="SOS (GAM)", y="SOS (LOG)", main="NDVI")+
  scale_x_continuous(labels=seq(0,350, 50), 
                     breaks= seq(0,350, 50),
                     limits=c(50,300))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=18),
        legend.position="none") +
  ggtitle("NDVI")

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
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  scale_fill_manual(values=c("gray0", "sienna3"), 
                    labels=c("LOG", "GAM"),
                    name=c("Model"))+
  scale_x_continuous(limits=c(75,200),
                     breaks=seq(80,200,20))+
  labs(x="DOY", y= "Count")
dev.off()


# scatterplot station level
#p1 <- 
  ggplot(GDD_SOS, aes(x= GAM_EVI, y=LOG_EVI)) + 
  geom_point(color="darkblue", alpha=1/5)+
  coord_equal()+
  geom_smooth(method='lm')+
  geom_abline(intercept = 0, slope = 1)+
  labs(x=expression(SOS[GAM]), y=expression(SOS[LOG]))+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(60,160))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(60,160))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  ggtitle("EVI")+
  annotate("text", x=140, y=68, label= "r = 0.88", size=3.5, hjust=0)

#p2 <- 
  ggplot(GDD_SOS, aes(x=GAM_NDVI, y=LOG_NDVI)) + 
  geom_point(color="darkblue", alpha=1/5)+
  coord_equal()+
  geom_smooth(method='lm')+
  geom_abline(intercept = 0, slope = 1)+
  labs(x=expression(SOS[GAM]), y=expression(SOS[LOG]))+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(60,160))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(60,160))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12)) +
  ggtitle("NDVI")+
  annotate("text", x=140, y=68, label= "r = 0.83", size=3.5, hjust=0)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190124_Model_Index_station.png", 
    width= 1200, height=800, res=200 )
grid.arrange(p1,p2, nrow=1 )
dev.off()

#scatterplot station level in one

LSP$stat_id=NULL
LSP_plot <- LSP

ggplot(data=LSP) + 
  geom_point(aes(x= LOG_EVI, y=GAM_EVI),color="darkblue", alpha=1/5)+
  geom_point(aes(x= LOG_NDVI, y=GAM_NDVI),color="red", alpha=1/9)+
  coord_equal()+
  geom_abline(intercept = 0, slope = 1)+
  labs(x="SOS (GAM)", y="SOS (LOG)")+
  scale_x_continuous(labels=seq(0,350, 20), 
                     breaks= seq(0,350, 20),
                     limits= c(60,160))+
  scale_y_continuous(labels=seq(0,350,20), 
                     breaks=seq(0,350,20),
                     limits= c(60,160))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20),
        legend.text=element_text(size=18)) +
  ggtitle("NDVI")

dev.off()


# histogram station level

estimates_evi_stat <- mean_evi[, c("b4","sp")]
estimates_evi_stat <- melt(estimates_evi_stat)

estimates_ndvi_stat <- mean_ndvi[, c("b4","sp")]
estimates_ndvi_stat <- melt(estimates_ndvi_stat)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181204_EVI_hist_station.png", 
    width= 1200, height=1000, res=200 )
ggplot(estimates_evi_stat, aes(x=value, fill=variable)) + 
  geom_histogram(alpha=0.6, position="identity", binwidth=1)+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  scale_fill_manual(values=c("black", "royalblue3"), 
                    labels=c("LOG", "GAM"),
                    name=c("Model"))+
  scale_x_continuous(limits=c(75,200),
                     breaks=seq(80,200,20))+
  labs(x="DOY", y= "Count")+
  ggtitle("EVI")
dev.off()


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181204_NDVI_hist_station.png", 
    width= 1200, height=1000, res=200 )
ggplot(estimates_ndvi_stat, aes(x=value, fill=variable)) + 
  geom_histogram(alpha=0.6, position="identity", binwidth=1)+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  scale_fill_manual(values=c("black", "royalblue3"), 
                    labels=c("LOG", "GAM"),
                    name=c("Model"))+
  scale_x_continuous(limits=c(75,200),
                     breaks=seq(80,200,20))+
  labs(x="DOY", y= "Count")+
  ggtitle("NDVI")
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

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181204_convergence.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=model_fits)+
  geom_bar(aes(x= Index, y=Convergence, fill=Model), 
           stat="identity", position = position_dodge2(),
           width = 0.5)+
  scale_fill_manual(values=c( "royalblue3", "grey28"))+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))

dev.off()

# percentiles SOS plot

plot_SOS <- GDD_SOS[, c("GAM_EVI","GAM_NDVI", "LOG_EVI", "LOG_NDVI", "TT", "PEP_SOS")]
plot_SOS <- melt(plot_SOS)
plot_SOS$index[plot_SOS$variable == "LOG_EVI" |  plot_SOS$variable== "GAM_EVI"] <- "EVI"
plot_SOS$index[plot_SOS$variable == "LOG_NDVI" |  plot_SOS$variable== "GAM_NDVI"] <- "NDVI"
plot_SOS$variable <- as.character(plot_SOS$variable)
plot_SOS$variable[plot_SOS$variable == "LOG_EVI" |  plot_SOS$variable== "LOG_NDVI"] <- "LOG"
plot_SOS$variable[plot_SOS$variable == "GAM_EVI" |  plot_SOS$variable== "GAM_NDVI"] <- "GAM"
plot_SOS$variable[plot_SOS$variable == "PEP_SOS"] <- "PEP"


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190129_boxplot_stations.png", 
    width= 900, height=700, res=200 )
ggplot(plot_SOS, aes(y=value, x=variable, fill=index )) +
  geom_boxplot(varwidth=TRUE)+
  scale_fill_manual(values=c( "royalblue2", "grey45"),
                    labels = c("EVI", "NDVI", " "))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.title=element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x="Model", y="SOS")
dev.off()



ggplot(data=mean_ndvi)+
  geom_point(aes(x=sp, y=MSE_gam))

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181210_LOG_plot.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=mean_results)+
  geom_point(aes(x=LOG_EVI, y=LOG_NDVI), color="darkblue", alpha=1/5)+
  geom_abline(intercept = 0, slope = 1)+
  coord_equal()+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  scale_x_continuous(labels=seq(90,150, 10), 
                     breaks= seq(90,150, 10),
                     limits= c(90,150))+
  scale_y_continuous(labels=seq(90,150,10), 
                     breaks=seq(90,150,10),
                     limits= c(90,150))+
  labs(x="EVI", y="NDVI")+
  ggtitle("LOG")
dev.off()

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181210_GAM_plot.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=mean_results)+
  geom_point(aes(x=GAM_EVI, y=GAM_NDVI), color="darkblue", alpha=1/5)+
  geom_abline(intercept = 0, slope = 1)+
  coord_equal()+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  scale_x_continuous(labels=seq(70,170, 10), 
                     breaks= seq(70,170, 10),
                     limits= c(90,160))+
  scale_y_continuous(labels=seq(80,170,10), 
                     breaks=seq(80,170,10),
                     limits= c(90,160))+
  labs(x="EVI", y="NDVI")+
  ggtitle("GAM")
dev.off()

# MAP SOS 

ggplot() + 
  geom_rect(data = extent, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2) +
  geom_polygon(data = fortify(loc_latlong), aes(x = long, y = lat, group = group), fill = NA, col = "black") +
  geom_point(data = stations_included, aes(x = geoLaenge, y = geoBreite), shape = 2, col = "red")

