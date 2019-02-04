

# # plot histogram of all GDD estimates
# pl_GDD_EVI <- melt(GDD_SOS[, c("GDD_GAM_EVI", "GDD_LOG_EVI", "GDD_PEP")])
# pl_GDD_NDVI <- melt(GDD_SOS[, c("GDD_GAM_NDVI", "GDD_LOG_NDVI", "GDD_PEP")])
# pl_GDD <- rbind(pl_GDD_EVI, pl_GDD_NDVI)
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190119_GDD_EVI_density.png", 
#     width= 900, height=700, res=200 )
# ggplot(data=pl_GDD_EVI)+
#   geom_density(aes(x=value, fill=variable), adjust=0.6, alpha=0.8, position="identity")+
#   labs(x="GDD")+
#   scale_fill_manual(values=c("#1f78b4","grey30","grey70"),labels=c("GAM","LOG","PEP"))+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.text=element_text(size=12),
#         legend.title = element_blank())
# dev.off()
# 

# 
# 
# plot_GDD <- GDD_SOS[, c("GDD_GAM_EVI","GDD_GAM_NDVI", "GDD_LOG_EVI", "GDD_LOG_NDVI","GDD_PEP")]
# plot_GDD <- melt(plot_GDD)
# plot_GDD$index[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_GAM_EVI"] <- "EVI"
# plot_GDD$index[plot_GDD$variable == "GDD_LOG_NDVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "NDVI"
# plot_GDD$variable <- as.character(plot_GDD$variable)
# plot_GDD$variable[plot_GDD$variable == "GDD_LOG_EVI" |  plot_GDD$variable== "GDD_LOG_NDVI"] <- "LOG"
# plot_GDD$variable[plot_GDD$variable == "GDD_GAM_EVI" |  plot_GDD$variable== "GDD_GAM_NDVI"] <- "GAM"
# plot_GDD$variable[plot_GDD$variable == "GDD_PEP"] <- "PEP"
# 
# 
# png(file="\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/maps/20190127_boxplot_GDD.png", 
#     width= 900, height=700, res=200 )
# ggplot(plot_GDD, aes(y=value, x=variable, fill=index )) +
#   geom_boxplot(varwidth=TRUE)+
#   scale_fill_manual(values=c( "royalblue2", "grey45"),
#                     labels = c("EVI", "NDVI", " "))+
#   theme_bw()+
#   theme(axis.text.x = element_text(size=12, color="black"),
#         axis.text.y = element_text(size=12, color="black"),
#         text = element_text(size=12),
#         legend.title=element_blank(),
#         panel.grid.major.x = element_blank())+
#   labs(x="Model", y="GDD")
# dev.off()
# 
