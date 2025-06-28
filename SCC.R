##SCC
set.seed(20250110)
# 训练随机森林模型
rf_modelF <- randomForest(Rs_Norm ~ P_LastMonth   #+ Measure_Month   
                          + Pm + Tm   + absLog +absLat # + Climate_Koeppon
                          + LAI  + Elevation, data = TrainSet1, mtry=3, importance=TRUE, ntree=500)
# 对测试集进行预测
preT <- predict(rf_modelF,TrainSet1)
# 对测试集进行预测
preV <- predict(rf_modelF, ValidSet1)
importance_scoresF <- importance(rf_modelF)
# 保存训练集预测结果
#    write.csv(data.frame(Original = TrainSet1$Rs_Norm, Predicted = preT), "TrainSet_Predictions.csv", row.names = FALSE)

# 保存验证集预测结果
#    write.csv(data.frame(Original = ValidSet1$Rs_Norm, Predicted = preV), "ValidSet_Predictions.csv", row.names = FALSE)

#    write.csv(importance_scores,"importance.csv")

rmse(TrainSet1$Rs_Norm, preT)
rmse(ValidSet1$Rs_Norm, preV)

ve(TrainSet1$Rs_Norm, preT)
ve(ValidSet1$Rs_Norm, preV)