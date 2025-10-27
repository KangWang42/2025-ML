# 加载必要包
library(e1071) # SVM
library(pROC) # ROC曲线
library(ggplot2) # 绘图
library(dplyr) # 数据处理
library(caret) # 划分训练集/测试集
library(reshape2) # 数据格式转换

# 数据准备：Pima Indians Diabetes
data(PimaIndiansDiabetes, package = "mlbench")
df <- PimaIndiansDiabetes

# 转换成因子类型
df$diabetes <- as.factor(df$diabetes)

# 描述性统计
summary(df)

# 划分训练/测试集
set.seed(123)
train_idx <- createDataPartition(df$diabetes, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]

# 计算类别权重（解决 pos 样本较少的问题）
class_weights <- 1 / table(train$diabetes)  # 给少数类更高权重
class_weights <- class_weights / sum(class_weights)  # 归一化

# SVM调参
tuned <- tune.svm(
  diabetes ~ .,
  data = train,
  kernel = "radial",
  gamma = seq(0.001, 0.1, length.out = 20), 
  cost = c(0.1, 0.5, 1, 5, 10, 50, 100, 500),
  class.weights = class_weights,    # 处理不平衡
  scale = TRUE,
  tunecontrol = tune.control(cross = 10, sampling = "cross")  # 10折CV
)

cat("最优参数:\n")
print(tuned$best.parameters)

# 用最优参数训练
best_svm <- svm(
  x = train[, 1:8],
  y = train$diabetes, 
  kernel = "radial",
  gamma = tuned$best.parameters$gamma,
  cost = tuned$best.parameters$cost,
  class.weights = class_weights,
  scale = TRUE,
  probability = TRUE
)
# 预测（正类 = "pos"）
pred_class <- predict(best_svm, test[, 1:8])
pred_prob  <- attr(predict(best_svm, test[, 1:8], probability = TRUE),
                   "probabilities")[, "pos"]

# 评估
cm <- confusionMatrix(pred_class, test$diabetes, positive = "pos")
auc_val <- auc(roc(test$diabetes, pred_prob))

metrics <- data.frame(
  Accuracy = round(as.numeric(cm$overall["Accuracy"]), 3),
  Sensitivity = round(as.numeric(cm$byClass["Sensitivity"]), 3),
  Specificity = round(as.numeric(cm$byClass["Specificity"]), 3),
  AUC = round(as.numeric(auc_val), 3)
)
print(metrics)

# 混淆矩阵
cm_df <- melt(table(Actual = test$diabetes, Predicted = pred_class))
p1 <- ggplot(cm_df, aes(Actual, Predicted, fill = value)) +
  geom_tile() + geom_text(aes(label = value), size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "混淆矩阵") + theme_minimal()

# ROC 曲线
p2 <- ggroc(roc(test$diabetes, pred_prob)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  labs(title = paste("ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal()

# 2D 决策边界：选择 glucose 和 mass
df2 <- df %>% select(glucose, mass, diabetes) %>% slice_sample(n = 200)

tuned2d <- tune.svm(diabetes ~ ., data = df2, kernel = "radial", gamma = c(0.01, 0.1, 1), cost = c(1, 10), scale = TRUE)
svm2d <- svm(diabetes ~ ., data = df2, kernel = "radial", 
             gamma = tuned2d$best.parameters$gamma, 
             cost = tuned2d$best.parameters$cost, 
             scale = TRUE)

grid <- expand.grid(
  glucose = seq(min(df2$glucose), max(df2$glucose), length = 80),
  mass = seq(min(df2$mass), max(df2$mass), length = 80)
)
grid$Pred <- predict(svm2d, grid)

p3 <- ggplot(grid, aes(glucose, mass, fill = Pred)) +
  geom_tile(alpha = 0.3) +
  geom_point(data = df2, aes(x = glucose, y = mass, color = diabetes), 
             size = 1, inherit.aes = FALSE) +
  scale_fill_manual(values = c("neg" = "lightblue", "pos" = "pink")) +
  scale_color_manual(values = c("neg" = "blue", "pos" = "red")) +
  labs(title = "SVM 决策边界（糖尿病预测）") +
  theme_minimal()

# 绘图
print(p1)
print(p2)
print(p3)
