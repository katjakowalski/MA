library(ggplot)
library(scales)
library(raster)
library(rgdal)
library(gplots)

setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/results/")
ger <- readOGR("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dwd/germany.shp")
SOS <- read.csv(file="20190128_GDD_SOS.csv", header=TRUE)

dem <- raster("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/dem_ger.tif")

slope <- terrain(dem,opt='slope')
aspect <- terrain(dem,opt='aspect')


hs <- hillShade(slope = slope, aspect = aspect, 40,270)

ger_df <- fortify(ger)

breaks_GAM_EVI = quantile(SOS$sp, seq(0.1,0.9,0.01))
breaks_LOG_EVI = quantile(SOS$b4, seq(0.1,0.9,0.02))
ramp_GAM_EVI <- colorpanel(n=length(breaks_GAM_EVI)-1, low="#440154", mid="#20928c", high="#fde725")
ramp_LOG_EVI <- colorpanel(n=length(breaks_GAM_EVI)-1, low="#440154", mid="#20928c", high="#fde725")

hist(SOS$GAM_EVI, breaks=150)

p1 <- ggplot()+
  geom_polygon(data = ger_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', fill="grey")+
  geom_point(data = SOS, aes(x=X, y=Y, color=GAM_EVI), size=3)+
  coord_fixed(1.0)+
  scale_colour_gradientn(colours = ramp_GAM_EVI)+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

p2 <- ggplot()+
  geom_polygon(data = ger_df, 
               aes(x = long, y = lat, group = group),
               color = 'black', fill="grey")+
  geom_point(data = SOS, aes(x=X, y=Y, color=b4), size=3)+
  coord_fixed(1.0)+
  scale_colour_gradientn(colours = ramp_LOG_EVI)+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


grid.arrange(p1,p2, nrow=1)
