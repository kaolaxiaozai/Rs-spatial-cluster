###SCW

set.seed(20250110)
# 训练随机森林模型
rf_modelC <- randomForest(Rs_Norm ~ P_LastMonth   #+ Measure_Month   
                          + Pm + Tm+ absLog +absLat # + Climate_Koeppon
                          + LAI  + Elevation, data = TrainSet1, mtry=3, importance=TRUE, ntree=500)
# 对测试集进行预测
predictions <- predict(rf_modelC)

#calculate out-of-bag residuals
TrainSet1$res <- TrainSet1$Rs_Norm - predictions
#convert to sp class
#df_spat <- train_data; coordinates(df_spat) = ~X+Y
#crs(df_spat) <- crs(awt)

res <- data.frame(
  x = TrainSet1$X,
  y = TrainSet1$Y,
  res = TrainSet1$Rs_Norm - predictions
)

#calculate out-of-bag residuals
#      df$res <- df$Rs_Norm - predict(rf, df)
#convert to sp class
#      df_spat <- df; coordinates(df_spat) = ~X+Y

v <- variogram(object = res~1, locations = ~x+y, data = res, cutoff = 360)

#try fitting an exponential model
#fit <- fit.variogram(v, model = vgm("Exp"))
# plot(v, fit)

#Automatically fit the best variogram model (Gau, Exp, Sph)
models <- c("Sph", "Gau", "Exp")
best_fit <- NULL
best_sse <- Inf

for (m in models) {
  fit <- tryCatch(
    {
      fit.variogram(v, model = vgm(m))  # 添加初始值
    },
    error = function(e) NULL
  )
  
  # 绘制拟合结果以便检查
  if (!is.null(fit)) {
    plot(v, fit, main = paste("Fitting", model))
  }
  
  # 检查拟合结果是否有效
  if (!is.null(fit) && !is.na(attr(fit, "SSErr"))) {
    sse <- attr(fit, "SSErr")
    if (sse < best_sse) {
      best_fit <- fit
      best_sse <- sse
    }
  }
}
#calculate distance matrix between sample points
d = dist(TrainSet1[ ,c("X", "Y")])
wtP <- covWt(dmat = d, model = best_fit)
#wtP <- covWt(dmat = d, model = fit)
TrainSet1$wtP <- wtP
#write.csv(TrainSet1,"TrainSet.csv")
rfPC <- randomForest(Rs_Norm ~ P_LastMonth   #+ Measure_Month   
                     + Pm + Tm + absLog +absLat  #+ Climate_Koeppon
                     + LAI  + Elevation, data = TrainSet1,weights = TrainSet1$wtP, mtry=3, importance=TRUE, ntree=500)
# 对测试集进行预测
#prePC <- predict(rfP, ValidSet1)

#rmse <- rmse(ValidSet1$Rs_Norm, predictions)

#ve <- ve(ValidSet1$Rs_Norm, predictions)
# 对测试集进行预测
preTC <- predict(rfPC, TrainSet1)
# 对测试集进行预测
preVC <- predict(rfPC, ValidSet1)
importance_scoresC <- importance(rfPC)
# 保存训练集预测结果
#    write.csv(data.frame(Original = TrainSet1$Rs_Norm, Predicted = preTC), "TrainSet_PredictionsC.csv", row.names = FALSE)

# 保存验证集预测结果
#    write.csv(data.frame(Original = ValidSet1$Rs_Norm, Predicted = preVC), "ValidSet_PredictionsC.csv", row.names = FALSE)

#    write.csv(importance_scoresC,"importanceC.csv")

rmse(TrainSet1$Rs_Norm, preTC)
rmse(ValidSet1$Rs_Norm, preVC)

ve(TrainSet1$Rs_Norm, preTC)
ve(ValidSet1$Rs_Norm, preVC)
