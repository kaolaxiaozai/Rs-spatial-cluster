# 设置种子以确保结果的可重复性
#set.seed(2025)

# 初始化一个列表存储每次循环的平均准确率
overall_rmse <- numeric(100)
overall_ve <- numeric(100)

# 开始循环100次
for (j in 1:100) {
  # 创建一个数据分区，用于十折交叉验证
  folds <- createFolds(df$Rs_Norm, k = 10, list = TRUE, returnTrain = TRUE)
  
  # 初始化一个向量存储每折的准确率
  fold_RMSE <- numeric(10)
  fold_VE <- numeric(10)
  # 开始十折交叉验证
  for (i in seq_along(folds)) {
    # 获取训练集和测试集
    train_indices <- folds[[i]]
    train_data <- df[train_indices, ]
    test_data <- df[-train_indices, ]
    
    # 训练随机森林模型
    rf_model <- randomForest(Rs_Norm ~ P_LastMonth   # + Measure_Month
                             + Pm + Tm + absLog +absLat # + Climate_Koeppon 
                             + LAI  + Elevation, data = train_data, mtry=3, importance=TRUE, ntree=500)
    
    # 对测试集进行预测
    predictions <- predict(rf_model, test_data)
    
    rmse <- rmse(test_data$Rs_Norm, predictions)
    
    ve <- ve(test_data$Rs_Norm, predictions)
    
    # 计算预测准确率
    # accuracy <- mean(predictions == test_data$Species)
    
    # 保存每折的准确率
    fold_RMSE[i] <- rmse
    fold_VE[i] <- ve
  }
  
  # 计算当前循环的平均准确率并保存
  overall_rmse[j] <- mean(fold_RMSE)
  overall_ve[j] <- mean(fold_VE)
}

# 输出100次循环的结果
cat("循环100次的十折交叉验证完成。\n")
cat("每次循环的rmse：\n", overall_rmse, "\n")
cat("每次循环的ve：\n", overall_ve, "\n")

# 计算总体平均准确率和标准差
final_average_rmse <- mean(overall_rmse)
final_sd_rmse <- sd(overall_rmse)

final_average_ve <- mean(overall_ve)
final_sd_ve <- sd(overall_ve)

cat("总体平均rmse：", final_average_rmse, "\n")
cat("rmse的标准差为：", final_sd_rmse, "\n")

cat("总体平均ve：", final_average_ve, "\n")
cat("ve的标准差为：", final_sd_ve, "\n")

# 可视化结果
hist(overall_rmse, breaks = 10, main = "100次循环的平均rmse分布",
     xlab = "平均rmse", col = "skyblue", border = "white")
hist(overall_ve, breaks = 10, main = "100次循环的平均ve分布",
     xlab = "平均ve", col = "skyblue", border = "white")