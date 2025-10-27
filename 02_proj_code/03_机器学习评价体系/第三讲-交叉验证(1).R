
######################## 机器学习评价指标体系（下）####################

#安装包
install.packages("MASS")     # 包含birthwt数据集
install.packages("caret")    # 数据拆分、机器学习工具和评估指标

#加载包
library(MASS)    #案例数据需要的包
library(caret)   #数据拆分、机器学习工具和评估指标需要的包

#查看数据集
str(birthwt)     
summary(birthwt)
head(birthwt)





###################### 1.留出法 ##################
#设置随机种子，使结果可重现
set.seed(123)

# 加载数据集
data(birthwt)

# 数据预处理，将分类变量转换为因子类型
birthwt$race <- factor(birthwt$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ht <- factor(birthwt$ht, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ui <- factor(birthwt$ui, levels = c(0, 1), labels = c("no", "yes"))

# 定义线性回归模型公式
model_formula <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

# 将数据分为训练集和验证集 (70% 训练, 30% 验证)
train_index <- createDataPartition(birthwt$bwt, p = 0.7, list = FALSE)
train_data <- birthwt[train_index, ]
test_data <- birthwt[-train_index, ]

# 在训练集上拟合模型
holdout_model <- lm(model_formula, data = train_data)

# 在验证集上进行预测
holdout_predictions <- predict(holdout_model, newdata = test_data)

# 计算留出法的RMSE
holdout_rmse <- sqrt(mean((test_data$bwt - holdout_predictions)^2))
cat("留出法 RMSE:", holdout_rmse)

# 使用全部数据拟合最终模型
final_model <- lm(model_formula, data = birthwt)
print(summary(final_model))







###################### 2.重复留出法 ##################
# 加载包
library(MASS)     #案例数据需要的包
library(caret)   #数据拆分、机器学习工具和评估指标需要的包

# 设置随机种子，使结果可重现
set.seed(123)

# 加载数据集
data(birthwt)

# 数据预处理，将分类变量转换为因子类型
birthwt$race <- factor(birthwt$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ht <- factor(birthwt$ht, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ui <- factor(birthwt$ui, levels = c(0, 1), labels = c("no", "yes"))

# 定义线性回归模型公式
model_formula <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

# 设置重复次数和训练集、验证集比例
n_repeats <- 50     # 重复50次
train_ratio <- 0.7  # 70%训练，30%验证

# 创建向量来存储每次迭代的结果
rmse_values <- numeric(n_repeats)

# 执行重复留出法
for (i in 1:n_repeats) {
  # 随机划分训练集和验证集
  train_index <- createDataPartition(birthwt$bwt, p = train_ratio, list = FALSE)
  train_data <- birthwt[train_index, ]
  test_data <- birthwt[-train_index, ]
  
  # 在训练集上拟合模型
  model <- lm(model_formula, data = train_data)
  
  # 在验证集上进行预测
  predictions <- predict(model, newdata = test_data)
  
  # 计算RMSE
  rmse_values[i] <- sqrt(mean((test_data$bwt - predictions)^2))
  
  # 每10次迭代输出一次进度
  if (i %% 10 == 0) {
    cat("已完成", i, "次重复留出法迭代\n")
  }
}

# 计算平均RMSE
mean_rmse <- mean(rmse_values)

# 计算标准差
sd_rmse <- sd(rmse_values)

# 输出结果
cat("\n重复留出法验证结果 (", n_repeats, "次重复):\n")
cat("平均 RMSE:", round(mean_rmse, 2), "(标准差:", round(sd_rmse, 2), ")\n")

# 可视化结果
# 创建数据框以便绘图
results_df <- data.frame(
  Iteration = 1:n_repeats,
  RMSE = rmse_values
)

# 绘制RMSE的分布
hist(rmse_values, 
     main = "RMSE分布",
     xlab = "RMSE",
     col = "lightblue",
     border = "black")
abline(v = mean_rmse, col = "red", lwd = 2)
legend("topright", 
       legend = paste("均值:", round(mean_rmse, 2)),
       col = "red",
       lwd = 2)

# 输出前10次迭代的详细结果
cat("\n前10次迭代的详细结果:\n")
print(head(results_df, 10))

# 创建性能指标汇总表
performance_summary <- data.frame(
  指标 = "RMSE",
  平均值 = mean_rmse,
  标准差 = sd_rmse,
  最小值 = min(rmse_values),
  最大值 = max(rmse_values)
)
print(performance_summary)

# 使用全部数据拟合最终模型
final_model <- lm(model_formula, data = birthwt)
print(summary(final_model))







###################### 3.k折交叉验证 ##################
# 加载包
library(MASS)      # 案例数据需要的包
library(caret)     #数据拆分、机器学习工具和评估指标需要的包

# 设置随机种子，使结果可重现
set.seed(123)

# 加载数据集
data(birthwt)

# 数据预处理，将分类变量转换为因子类型
birthwt$race = factor(birthwt$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))
birthwt$smoke = factor(birthwt$smoke, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ht = factor(birthwt$ht, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ui = factor(birthwt$ui, levels = c(0, 1), labels = c("no", "yes"))

# 定义线性回归模型公式
model_formula <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

# 设置10折交叉验证的参数
train_control <- trainControl(
  method = "cv",      # 使用交叉验证
  number = 10,        # 10折
  savePredictions = "final", # 保存最终预测结果
  verboseIter = TRUE  # 显示迭代进度
)

# 执行10折交叉验证
cv_model <- train(
  model_formula, 
  data = birthwt, 
  method = "lm",           # 使用线性回归
  trControl = train_control,
  metric = "RMSE"          # 使用RMSE作为评估指标
)

# 查看交叉验证的总体结果
print(cv_model)

# 获取每折的详细结果
cv_results <- cv_model$resample
print(cv_results)

# 计算RMSE的均值和标准差
mean_rmse <- mean(cv_results$RMSE)
sd_rmse <- sd(cv_results$RMSE)

cat("\n10折交叉验证结果:\n")
cat("平均 RMSE:", round(mean_rmse, 2), "\n")
cat("RMSE 标准差:", round(sd_rmse, 2), "\n")

# 绘制RMSE的分布
hist(cv_results$RMSE, 
     main = "RMSE分布",
     xlab = "RMSE",
     col = "lightblue",
     border = "black")
abline(v = mean_rmse, col = "red", lwd = 2)
legend("topright", 
       legend = paste("均值:", round(mean_rmse, 2)),
       col = "red",
       lwd = 2)

# 创建模型性能汇总表
performance_summary <- data.frame(
  折数 = 1:10,
  RMSE = round(cv_results$RMSE, 2)
)
print(performance_summary)

# 使用全部数据拟合最终模型
final_model <- cv_model$finalModel
print(summary(final_model))








###################### 4.p次k折交叉验证 ##################
# 加载包
library(MASS)      # 案例数据需要的包
library(caret)     #数据拆分、机器学习工具和评估指标需要的包

# 设置随机种子，使结果可重现
set.seed(123)

# 加载数据集
data(birthwt)

# 数据预处理，将分类变量转换为因子类型
birthwt$race = factor(birthwt$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))
birthwt$smoke = factor(birthwt$smoke, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ht = factor(birthwt$ht, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ui = factor(birthwt$ui, levels = c(0, 1), labels = c("no", "yes"))

# 定义线性回归模型公式
model_formula <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

# 设置重复交叉验证参数
train_control <- trainControl(
  method = "repeatedcv",  # 使用重复交叉验证
  number = 10,            # 10折
  repeats = 10,           # 重复10次
  savePredictions = "final", # 保存最终预测结果
  returnResamp = "all"    # 返回所有重采样的结果
)

# 执行重复交叉验证
cv_model <- train(
  model_formula, 
  data = birthwt, 
  method = "lm",           # 使用线性回归
  trControl = train_control,
  metric = "RMSE"          # 使用RMSE作为评估指标
)

# 查看模型结果
print(cv_model)

# 提取所有重采样的结果
all_results <- cv_model$resample

# 查看结果结构
cat("\n结果数据结构:\n")
str(all_results)

# 绘制所有100次折叠的RMSE分布
ggplot(all_results, aes(x = RMSE)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean_rmse, linetype = "dashed", color = "red", size = 1) +
  labs(title = "100次折叠的RMSE分布 (10次10折交叉验证)",
       x = "RMSE",
       y = "频数") +
  theme_minimal()

# 创建箱线图比较不同重复的表现
ggplot(all_results, aes(x = factor(rep(1:10, each = 10)), y = RMSE)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "10次10折交叉验证 - 每次重复的RMSE分布",
       x = "重复次数",
       y = "RMSE") +
  theme_minimal()

# 使用全部数据拟合最终模型
final_model <- cv_model$finalModel
print(summary(final_model))








###################### 5.留一法交叉验证 ##################
# 加载包
library(MASS)      # 案例数据需要的包
library(caret)     #数据拆分、机器学习工具和评估指标需要的包

# 设置随机种子，使结果可重现
set.seed(123)

# 加载数据集
data(birthwt)

# 数据预处理，将分类变量转换为因子类型
birthwt$race = factor(birthwt$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))
birthwt$smoke = factor(birthwt$smoke, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ht = factor(birthwt$ht, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ui = factor(birthwt$ui, levels = c(0, 1), labels = c("no", "yes"))

# 定义线性回归模型公式
model_formula <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

# 设置参数为留一法交叉验证
train_control <- trainControl(
  method = "LOOCV"      # 使用留一法交叉验证
)

# 运行留一法交叉验证
loocv_model <- train(
  model_formula, 
  data = birthwt, 
  method = "lm",
  trControl = train_control,
  metric = "RMSE"
)

# 查看结果
print(loocv_model)

# 输出RMSE值
loocv_rmse <- loocv_model$results$RMSE
cat("留一法交叉验证的RMSE:", round(loocv_rmse, 2), "\n\n")

# 使用全部数据拟合最终模型
final_model <- loocv_model$finalModel
print(summary(final_model))








###################### 6.自助法交叉验证 ##################
# 加载包
library(MASS)      # 案例数据需要的包
library(caret)     #数据拆分、机器学习工具和评估指标需要的包

# 设置随机种子，使结果可重现
set.seed(123)

# 加载数据集
data(birthwt)

# 数据预处理，将分类变量转换为因子类型
birthwt$race = factor(birthwt$race, levels = c(1, 2, 3), labels = c("white", "black", "other"))
birthwt$smoke = factor(birthwt$smoke, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ht = factor(birthwt$ht, levels = c(0, 1), labels = c("no", "yes"))
birthwt$ui = factor(birthwt$ui, levels = c(0, 1), labels = c("no", "yes"))

# 定义线性回归模型公式
model_formula <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

# 设置自助法交叉验证参数
train_control <- trainControl(
  method = "boot",     # 使用自助法
  number = 100,        # 自助法重复次数
  savePredictions = "final"
)

# 运行自助法交叉验证
bootstrap_model <- train(
  model_formula, 
  data = birthwt, 
  method = "lm",
  trControl = train_control,
  metric = "RMSE"
)

# 查看结果
print(bootstrap_model)

# 提取自助法结果
bootstrap_results <- bootstrap_model$resample
bootstrap_rmse <- bootstrap_model$results$RMSE
cat("自助法交叉验证的RMSE:", round(bootstrap_rmse, 2), "\n\n")

# 绘制自助法RMSE的分布
ggplot(bootstrap_results, aes(x = RMSE)) +
  geom_histogram(binwidth = 5, fill = "steelblue", alpha = 0.7, color = "black") +
  geom_vline(xintercept = bootstrap_rmse, linetype = "dashed", color = "red", size = 1) +
  labs(title = "自助法交叉验证 - RMSE分布",
       subtitle = paste("平均 RMSE =", round(bootstrap_rmse, 2), 
                        "±", round(sd(bootstrap_results$RMSE), 2)),
       x = "RMSE",
       y = "频数") +
  theme_minimal()

# 拟合最终模型
final_model <- bootstrap_model$finalModel
print(summary(final_model))
