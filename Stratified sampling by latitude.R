#Stratified sampling by latitude

set.seed (2025)
train <- createDataPartition(y = P$Latitude,
                             p =0.8,
                             list = F,
                             times = 1)

TrainSet1 <- P[train,]
#Traindata <- RsForRandomForest[train,]
ValidSet1 <- P[-train,]

PT_data <- sf::st_as_sf(TrainSet1, coords = c("Longitude", "Latitude"), crs = crs(awt))
# see the first few rows
PT_data
df <- as.data.frame(PT_data)