# import raster data
library(maps)

#awt <- borders("world", colour="gray50", fill="gray65")
awt<-raster::brick("E:/论文下载_1/wc2.1_cruts4.06_10m_tmax_2020-2021/wc2.1_10m_tmax_2021-10.tif")
#awt <- map(database = "world")
#awt <- borders("world", colour="gray50", fill="gray65")
#awt <- raster::brick("E:/学习资料/blockCV-v2.1.0/rvalavi-blockCV-3d5dae4/inst/extdata/awt.grd")

# import data
P <- read.csv("E:/论文/CEA/daima/Rs.csv")
P$Measure_Year <- as.factor(P$Measure_Year)
P$Measure_Month <- as.factor(P$Measure_Month)
P$Climate_Koeppon <- as.factor(P$Climate_Koeppon)
P$TopClimatetype <- as.factor(P$TopClimatetype)
P$IGBP <- as.factor(P$IGBP)
#RsForRandomForest$Country <- as.factor(RsForRandomForest$Country)
#RsForRandomForest$SiteID <- as.factor(RsForRandomForest$SiteID)
P$MiddleClimate <- as.factor(P$MiddleClimate)
#RsForRandomForest$IGBP_FromPaper <- as.factor(RsForRandomForest$IGBP_FromPaper)
#RsForRandomForest$Meas_Method <- as.factor(RsForRandomForest$Meas_Method)
P$MYear <- as.factor(P$MYear)
P$IGBP1 <- as.factor(P$IGBP1)

# import  data
PA <- read.csv("E:/论文/CEA/daima/RDRs.csv")
PA$Measure_Year <- as.factor(PA$Measure_Year)
PA$Measure_Month <- as.factor(PA$Measure_Month)
PA$Climate_Koeppon <- as.factor(PA$Climate_Koeppon)
PA$TopClimatetype <- as.factor(PA$TopClimatetype)
PA$IGBP <- as.factor(PA$IGBP)
#RsForRandomForest$Country <- as.factor(RsForRandomForest$Country)
#RsForRandomForest$SiteID <- as.factor(RsForRandomForest$SiteID)
PA$MiddleClimate <- as.factor(PA$MiddleClimate)
#RsForRandomForest$IGBP_FromPaper <- as.factor(RsForRandomForest$IGBP_FromPaper)
#RsForRandomForest$Meas_Method <- as.factor(RsForRandomForest$Meas_Method)
PA$MYear <- as.factor(PA$MYear)
PA$IGBP1 <- as.factor(PA$IGBP1)

# import  data
PB <- read.csv("E:/论文/CEA/daima/RandRs.csv")
PB$Measure_Year <- as.factor(PB$Measure_Year)
PB$Measure_Month <- as.factor(PB$Measure_Month)
PB$Climate_Koeppon <- as.factor(PB$Climate_Koeppon)
PB$TopClimatetype <- as.factor(PB$TopClimatetype)
PB$IGBP <- as.factor(PB$IGBP)
#RsForRandomForest$Country <- as.factor(RsForRandomForest$Country)
#RsForRandomForest$SiteID <- as.factor(RsForRandomForest$SiteID)
PB$MiddleClimate <- as.factor(PB$MiddleClimate)
#RsForRandomForest$IGBP_FromPaper <- as.factor(RsForRandomForest$IGBP_FromPaper)
#RsForRandomForest$Meas_Method <- as.factor(RsForRandomForest$Meas_Method)
PB$MYear <- as.factor(PB$MYear)
PB$IGBP1 <- as.factor(PB$IGBP1)

set.seed (12345)
train <- createDataPartition(y = PA$Rs_Norm,
                             p =0.8,
                             list = F,
                             times = 1)

TrainSet <- PA[train,]
#Traindata <- RsForRandomForest[train,]
ValidSet <- PA[-train,]

# make a SpatialPointsDataFrame object from data.frame
pa_data <- sf::st_as_sf(TrainSet, coords = c("Longitude", "Latitude"), crs = crs(awt))
# see the first few rows
pa_data

# plot species data on the map
plot(awt[[1]]) # plot raster data
plot(pa_data, pch = 16, col="blue", add=TRUE) # add presence points
#plot(pa_data[which(pa_data$Species==0), ], pch = 16, col="blue", add=TRUE) # add absence points
#legend(x=500000, y=8250000, legend=c("Presence","Absence"), col=c(2, 4), pch=c(16,16), bty="n")