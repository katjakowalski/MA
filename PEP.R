setwd("\\\\141.20.140.91/SAN_Projects/Spring/workspace/Katja/germany/PEP/PEP725_Kowalski")

data_PEP <- read.csv(file="PEP725_Kowalski.csv", header=TRUE, sep=";")
PEP_stats <- read.csv(file="PEP_5km_dwd.csv", header=TRUE, sep=";")