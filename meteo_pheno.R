
setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/download/tmk")
tmk <- read.csv(file="TMK_MN004.txt", header=TRUE, sep=";")

tmk$QN <- NULL
tmk$QB <- NULL

tmk_2017 <- tmk[tmk$ZEITSTEMPEL> 20170131 & tmk$ZEITSTEMPEL< 20170601 ,]

tmk_spring <- aggregate(tmk_2017[, "WERT"], by=list(tmk_2017$STATION_ID), mean)
colnames(tmk_spring) <- c("stat_id", "spring_mean_temp")

mean_evi <- merge(mean_evi, tmk_spring, by="stat_id", all.x=TRUE)
results_evi <- merge(results_evi, tmk_spring, by="stat_id", all.x=TRUE)
mean_ndvi <- merge(mean_ndvi, tmk_spring, by="stat_id", all.x=TRUE)

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20181203_T_spring_EVI.png", 
    width= 1200, height=1000, res=200 )
ggplot(data=mean_evi, aes(x=sp, y=spring_mean_temp))+
  geom_point(col="darkblue", alpha=1/2)+
  geom_smooth(method='lm', se=FALSE, color="darkblue")+
  theme(axis.text.x = element_text(size=18, color="black"),
        axis.text.y = element_text(size=18, color="black"),
        text = element_text(size=20))+
  labs( x="SOS (EVI)", y= "T spring [°C]")

dev.off()

cor.test(mean_evi$sp, mean_evi$spring_mean_temp, use="complete.obs")
cor.test(mean_ndvi$sp, mean_ndvi$spring_mean_temp, use="complete.obs")
