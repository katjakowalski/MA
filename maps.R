library(ggplot)
library(scales)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/")
ger <- readOGR("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/germany.shp")
SOS <- read.csv(file="20181211_mean_evi.csv", header=TRUE)


ger_df <- fortify(ger)

qn = quantile(SOS$sp, c(0.05, 0.95), na.rm = TRUE)
qn01 <- rescale(c(qn, range(SOS$sp))) 

rb <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

rb <- colorRampPalette(c("blue", "white", "red"),interpolate="linear")

ggplot()+
  geom_polygon(data = ger_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', fill="grey")+
  geom_point(data = SOS, aes(x=X, y=Y, color=sp), size=3)+
  coord_fixed(1.0)+
  scale_colour_gradientn(colours = rb(10))+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
  
