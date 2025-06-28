##KDW
set.seed(20250110)
# 训练随机森林模型
rf_modelK <- randomForest(Rs_Norm ~ P_LastMonth   #+ Measure_Month  
                          + Pm + Tm   + absLog +absLat #+ Climate_Koeppon
                          + LAI  + Elevation, data = TrainSet1, mtry=3, importance=TRUE, ntree=500)
# 对测试集进行预测
predictions <- predict(rf_modelK)

#calculate out-of-bag residuals
TrainSet1$res <- TrainSet1$Rs_Norm - predictions
#convert to sp class
#df_spat <- train_data; coordinates(df_spat) = ~X+Y
#crs(df_spat) <- crs(awt)

df_spat_sf <- st_as_sf(TrainSet1, coords = c("X", "Y"))

kde <- sf.kde(x = df_spat_sf,bw=62, res =0.1 ,scale.factor = 10000)
#kde5 <- sf.kde(x = df_spat_sf, res =0.5 ,scale.factor = 10000)
# Extract raster values
#kde_values <- raster::values(kde5)
# write.csv(kde_values,"kde_values.csv")
# # Extract coordinates of each raster cell (grid)
# kde_coords <- raster::xyFromCell(kde5, 1:ncell(kde5))

# Combine coordinates and values into a data frame
#kde_df <- data.frame(x = kde_coords[,1], y = kde_coords[,2], density = kde_values)

#kde_values <- raster::values(kde)
#write.csv(kde_df,"kde_df.csv")

points_sf <- TrainSet1[c(40,41)]  # 使用你的输入样本点

# 提取点对应的 KDE 值
point <- terra::extract(kde, points_sf)

point1 <- na.omit(point)
#point[is.na(point)] <- 0.5
point[is.na(point)] <- min(point1$z)
wt<-point
min(wt)
max(wt)
# 查看结果
#print(wt)

# wt <- terra::extract(kde, df_spat_sf)
#scale with respect to the minimum
wt <- min(wt)/wt
max(wt)
min(wt)
#wt[is.na(wt)] <- 0.1
TrainSet1$wt <-wt$z

rfPK <- randomForest(Rs_Norm ~ P_LastMonth   #+ Measure_Month   
                     + Pm + Tm  + absLog +absLat #+ Climate_Koeppon
                     + LAI  + Elevation, data = TrainSet1,weights = TrainSet1$wt, mtry=3, importance=TRUE, ntree=500)

# 对测试集进行预测
#predictions <- predict(rfPK, ValidSet1)

#rmse <- rmse(ValidSet1$Rs_Norm, predictions)

#ve <- ve(ValidSet1$Rs_Norm, predictions)
# 对测试集进行预测
preTK <- predict(rfPK, TrainSet1)
# 对测试集进行预测
preVK <- predict(rfPK, ValidSet1)
importance_scoresK <- importance(rfPK)
# 保存训练集预测结果
#    write.csv(data.frame(Original = TrainSet1$Rs_Norm, Predicted = preTK), "TrainSet_PredictionsK.csv", row.names = FALSE)

# 保存验证集预测结果
#    write.csv(data.frame(Original = ValidSet1$Rs_Norm, Predicted = preVK), "ValidSet_PredictionsK.csv", row.names = FALSE)

#    write.csv(importance_scoresK,"importanceK.csv")

rmse(TrainSet1$Rs_Norm, preTK)
rmse(ValidSet1$Rs_Norm, preVK)

ve(TrainSet1$Rs_Norm, preTK)
ve(ValidSet1$Rs_Norm, preVK)