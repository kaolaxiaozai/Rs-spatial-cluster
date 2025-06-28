
# 拟合 GLMM
modelT <- glmer(
  Rs_Norm ~ Tm + (1 | studynumber), # 固定效应和随机效应
  data = P,
  family = Gamma(link = "log") # Gamma 分布，log 链接函数
)

# 查看模型结果
summary(modelT)

# 提取拟合值和残差
fitted_vals <- fitted(modelT)
residuals <- resid(modelT, type = "pearson")

# 残差图
ggplot(data.frame(Fitted = fitted_vals, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Pearson Residuals")

library(visreg)
# 使用 visreg 绘图
visreg(modelT, 
       xlab = "Tm (℃)", 
       ylab = "Rs_Norm", 
       scale = "response", 
       line = list(col = "red"), 
       ylim = c(0, 15))

library(effects)
plot(Effect("Tm", modelT))