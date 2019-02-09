

# plot histogram of all GDD estimates
pl_GDD_EVI <- melt(GDD_SOS[, c("GDD_GAM_EVI", "GDD_LOG_EVI", "GDD_PEP")])
pl_GDD_NDVI <- melt(GDD_SOS[, c("GDD_GAM_NDVI", "GDD_LOG_NDVI", "GDD_PEP")])
pl_SOS_EVI <- melt(GDD_SOS[, c("GAM_EVI", "LOG_EVI", "PEP_SOS")])

pl_both <- rbind(pl_GDD_EVI, pl_GDD_NDVI)



p2 <- ggplot(data=pl_GDD_NDVI)+
  geom_density(aes(x=value,fill=variable), adjust=0.6, alpha=0.8, position="identity")+
  labs(x="GDD")+
  scale_fill_manual(values=c("#1f78b4","grey30","grey70"),labels=c("GAM","LOG","GO"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom")
        

p1 <- ggplot(data=pl_GDD_EVI)+
  geom_density(aes(x=value,fill=variable), adjust=0.6, alpha=0.8, position="identity")+
  labs(x="GDD")+
  scale_fill_manual(values=c("#1f78b4","grey30","grey70"),labels=c("GAM","LOG","GO"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom")

png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/201900207_GDD_EVI_density.png",
    width= 16, height=10, res=200, unit="cm" )
grid.arrange(p1, p2, nrow=1)
dev.off()


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/201900208_SOS_EVI_density.png",
    width= 16, height=8, unit="cm", res=500 )
ggplot(data=plot_GDD, aes(x=value))+
  geom_density(aes(color=variable),adjust=0.6, alpha=0.8, position="identity",size=0.6)+
  labs(x="GDD")+
  scale_color_manual(values=c("black","#1f78b4","gray50"),labels=c("GAM","LOG","GO"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_blank())+
 facet_wrap( ~ index, ncol=2)
dev.off()




##################

plot_GDD <- GDD_SOS[, c("GDD_GAM_EVI","GDD_GAM_NDVI", "GDD_LOG_EVI", "GDD_LOG_NDVI","GDD_PEP")]
plot_GDD$GDD_PEP2 <- plot_GDD$GDD_PEP
plot_GDD <- melt(plot_GDD)
plot_GDD$index[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_GAM_EVI"] <- "EVI"
plot_GDD$index[plot_GDD$variable == "GDD_LOG_NDVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "NDVI"
plot_GDD$variable <- as.character(plot_GDD$variable)
plot_GDD$variable[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_LOG_NDVI"] <- "LOG"
plot_GDD$variable[plot_GDD$variable == "GDD_GAM_EVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "GAM"
plot_GDD$variable[plot_GDD$variable == "GDD_PEP"] <- "GO"
plot_GDD$variable[plot_GDD$variable == "GDD_PEP2"] <- "GO"
plot_GDD$index[plot_GDD$variable == "GO"] <- "EVI"
plot_GDD$index[plot_GDD$variable == "GDD_PEP2"] <- "NDVI"
plot_GDD$variable <- factor(plot_GDD$variable, levels = c('GAM','LOG', 'GO'),ordered = TRUE)

plot_GDD <- subset(plot_GDD, !is.na(value))





png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190204_boxplot_GDD.png",
    width= 900, height=700, res=200 )
ggplot(plot_GDD, aes(y=value, x=variable, fill=index )) +
  geom_boxplot(varwidth=TRUE)+
  scale_fill_manual(values=c( "royalblue2", "grey45"),
                    labels = c("EVI", "NDVI", " "))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        text = element_text(size=12),
        legend.title=element_blank(),
        panel.grid.major.x = element_blank())+
  labs(x=" ", y="GDD")
dev.off()


plot_SOS <- GDD_SOS[, c("GAM_EVI","GAM_NDVI", "LOG_EVI", "LOG_NDVI","PEP_SOS", "TT", "SQ")]
plot_SOS <- melt(plot_SOS)
plot_SOS$index[plot_SOS$variable == "LOG_EVI" |  plot_SOS$variable== "GAM_EVI"] <- "EVI"
plot_SOS$index[plot_SOS$variable == "LOG_NDVI" |  plot_SOS$variable== "GAM_NDVI"] <- "NDVI"
plot_SOS$variable <- as.character(plot_SOS$variable)
plot_SOS$variable[plot_SOS$variable == "LOG_EVI" |  plot_SOS$variable== "LOG_NDVI"] <- "LOG"
plot_SOS$variable[plot_SOS$variable == "GAM_EVI" |  plot_SOS$variable== "GAM_NDVI"] <- "GAM"
plot_SOS$variable[plot_SOS$variable == "PEP_SOS"] <- "GO"
plot_SOS$variable <- factor(plot_SOS$variable, levels = c('GAM','LOG', 'SQ','TT', 'GO'),ordered = TRUE)


png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190204_boxplot_SOS.png",
    width= 18, height=11, unit="cm",  res=200 )
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
  labs(x=" ", y="GDD")
dev.off()


