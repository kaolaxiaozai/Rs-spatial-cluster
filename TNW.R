#TNW
set.seed(20250110)
# 训练随机森林模型
rf_modelT <- randomForest(Rs_Norm ~ P_LastMonth   #+ Measure_Month   
                          + Pm + Tm   #+ absLog +absLat + Climate_Koeppon
                          + LAI  + Elevation, data = TrainSet1, mtry=3, importance=TRUE, ntree=500)
# 对测试集进行预测
pre1 <- predict(rf_modelT,TrainSet1)
# 对测试集进行预测
pre2 <- predict(rf_modelT, ValidSet1)
importance_scoresT <- importance(rf_modelT)
# 保存训练集预测结果
#    write.csv(data.frame(Original = TrainSet1$Rs_Norm, Predicted = pre1), "TrainSet_PredictionsT.csv", row.names = FALSE)

# 保存验证集预测结果
#    write.csv(data.frame(Original = ValidSet1$Rs_Norm, Predicted = pre2), "ValidSet_PredictionsT.csv", row.names = FALSE)

#    write.csv(importance_scoresT,"importanceT.csv")

rmse(TrainSet1$Rs_Norm, pre1)
rmse(ValidSet1$Rs_Norm, pre2)

ve(TrainSet1$Rs_Norm, pre1)
ve(ValidSet1$Rs_Norm, pre2)