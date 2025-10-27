#威斯康星州乳腺癌数据库
install.packages("rpart")
install.packages("rattle")
install.packages("partykit")
install.packages("pROC")
# 1. 导入数据
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
wdbc <- read.csv(url, header = FALSE)

## 添加列名
colnames(wdbc) <- c(
  "ID", "Diagnosis",
  paste0(rep(c("radius", "texture", "perimeter", "area", "smoothness",
               "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension"), each = 3),
         "_", rep(c("mean", "se", "worst"), times = 10))
)

# 2. 数据预处理
## 删除 ID 列
df <- wdbc[, -1]

## 转换因子型标签
df$Diagnosis <- factor(df$Diagnosis,
                       levels = c("B", "M"),
                       labels = c("benign", "malignant"))

## 检查类别分布
table(df$Diagnosis)

# 3. 划分训练集 & 测试集 
set.seed(1234)
index <- sample(nrow(df), 0.7 * nrow(df))
train <- df[index, ]
test  <- df[-index, ]

table(train$Diagnosis)
table(test$Diagnosis)


# 4. 经典决策树 (rpart)
library(rpart)
library(rpart.plot)
library(rattle)

## 生成决策树（完全树）
dtree<- rpart(Diagnosis ~ ., data = train,
                    method = "class",
                    parms = list(split="gini"),
                    control = rpart.control(cp = 0, minsplit = 2, maxdepth = 30))
rpart.plot(dtree, main="Fully Grown Tree")


## 剪枝
###预剪枝（限制深度/样本数）
dtree_pre <- rpart(Diagnosis ~ ., data = train,
                   method = "class",
                   parms = list(split="gini"),
                   control = rpart.control(cp = 0.01, minsplit = 20, maxdepth = 5))
fancyRpartPlot(dtree_pre, main="Fully Grown Tree (pre)")
dtree.pred1 <- predict(dtree_pre,test,type = "class")
dtree.perf1 <- table(test$Diagnosis,dtree.pred1,
                    dnn=c("Actual","Predicted"))
dtree.perf1

###后剪枝（最优 cp 值）
dtree$cptable
best_cp <- dtree$cptable[which.min(dtree$cptable[,"xerror"]), "CP"]
dtree_pruned <- prune(dtree, cp = best_cp)
fancyRpartPlot(dtree_pruned, sub = "Classification Tree")

## 预测 + 混淆矩阵
dtree.pred2 <- predict(dtree_pruned,test,type = "class")
dtree.perf2 <- table(test$Diagnosis,dtree.pred2,
                    dnn=c("Actual","Predicted"))
dtree.perf2

##绘制ROC曲线（以后剪枝为例）
library(pROC)
dtree.prob <- predict(dtree_pruned, test, type = "prob")[, "malignant"]       # 提取预测概率（恶性类别）
y_true <- ifelse(test$Diagnosis == "malignant", 1, 0)     # 创建真实标签 (1 = malignant, 0 = benign)
roc_obj <- roc(y_true, dtree.prob)       # 计算 ROC 曲线对象
auc_val <- as.numeric(auc(roc_obj))      # 计算 AUC 值
plot(roc_obj, 
     col = "blue",
     lwd = 2,
     main = "ROC Curve for Decision Tree",
     legacy.axes = TRUE)                         # 绘制 ROC 曲线

abline(a = 0, b = 1, lty = 2, col = "gray")      # 添加基准线
legend("bottomright",
       legend = paste0("AUC = ", formatC(auc_val, digits = 3, format = "f")),
       bty = "n",
       text.font = 2)                # 图中显示 AUC 值



# 5. 条件推断树 
library(partykit)

fit.ctree <- ctree(Diagnosis ~ ., data = train)
plot(fit.ctree, main = "Conditional Inference Tree", gp = gpar(fontsize = 8))

ctree.pred <- predict(fit.ctree, test, type = "response")
ctree.perf <- table(Actual = test$Diagnosis, Predicted = ctree.pred)
ctree.perf






#糖尿病案例
# 1. 数据准备
install.packages("mlbench")
install.packages("rpart")
install.packages("rpart.plot")
library(mlbench)
library(rpart)
library(rpart.plot)
data(PimaIndiansDiabetes2)  
df <- na.omit(PimaIndiansDiabetes2)

# 2. 构建决策树
fit <- rpart(diabetes ~ ., data = df, method = "class")
rpart.plot(fit,
           type = 3,        
           extra = 104,     
           under = TRUE,  
           faclen = 0,     
           cex = 0.8,    
           tweak = 1.2,  
           fallen.leaves = TRUE, 
           main = "Decision Tree for Diabetes Prediction")
# 3. 划分训练集和验证集之后
set.seed(123) 
n <- nrow(df)
train_index <- sample(1:n, size = 0.7*n) 

train <- df[train_index, ]
test  <- df[-train_index, ]

## 用训练集建模
fit <- rpart(diabetes ~ ., data = train, method = "class")
rpart.plot(fit, 
           type = 3, extra = 104, under = TRUE, faclen = 0, 
           cex = 0.8, tweak = 1.2, fallen.leaves = TRUE, 
           main = "Decision Tree for Diabetes Prediction")

## 在测试集上预测并评估
pred <- predict(fit, test, type = "class")
table(Predicted = pred, Actual = test$diabetes)

## 预测概率
pred_prob_train <- predict(fit, train, type = "prob")[,2]  # 训练集概率
pred_prob_test  <- predict(fit, test, type = "prob")[,2]   # 测试集概率

## 计算 ROC
roc_train <- roc(train$diabetes, pred_prob_train, levels = c("neg","pos"))
roc_test  <- roc(test$diabetes, pred_prob_test, levels = c("neg","pos"))

## 绘图
plot(roc_train, col="blue", lwd=2, main="ROC Curve: Train vs Test")
plot(roc_test, col="red", lwd=2, add=TRUE)
legend("bottomright", legend=c(paste0("Train AUC=", round(auc(roc_train),3)),
                               paste0("Test AUC=", round(auc(roc_test),3))),
       col=c("blue","red"), lwd=2)

