
#rm(list=ls()) #清除所有信息
#### 加载必要包 ##############
library(gbm) #GBDT
library(xgboost) #xgboost
library(lightgbm) #lightgbm
library(catboost) #catboost
library(caret) # 划分训练集/测试集/调参
library(pROC) # ROC曲线
library(ggplot2) # 绘图
library(dplyr) # 数据处理
library(gridExtra) #ggplot图排版
library(mlbench) #糖尿病数据

#catboost包安装
#在https://github.com/catboost/catboost/tag找到并下载对应自己电脑的包，如catboost-R-Windows-1.2.8.tgz
#下载后解压文件夹到本地电脑，如D盘，运行以下代码
#install.packages("D:/catboost",repos = NULL, type = "source")

######### 读取数据1：乳腺癌诊断 ################
df <- read.csv("breast_cancer.csv",header=T,encoding = "UTF-8")
sum(is.na(df))

# 数据准备
summary(df)
names(df)
table(df$Diagnosis)
df$Class <- ifelse(df$Diagnosis == "M", 1, 0)

# 划分训练/测试集
set.seed(123)
train_idx <- createDataPartition(df$Class, p = 0.7, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]


#### GBDT ####
x <- as.matrix(train[ , c(3:32)])
y <-train$Class
dat <- cbind(x , label = y)
dat_df <-as.data.frame(dat)

set.seed(123)
# 训练 GBDT（梯度提升树）
model <- gbm(  formula = label ~ ., # label 为因变量，其余所有列为自变量 
                 data = dat_df,   # 输入数据框必须包含 label 列和特征
                 distribution = "bernoulli", #二分类 
                 n.trees =5000,  # 最大迭代树数 
                 interaction.depth = 3,  # 单棵树的最大深度
                 shrinkage =0.01, # 学习率
                 n.minobsinnode =10,# 每个叶节点最少样本数
                 bag.fraction =0.7,# 每轮随机采样的样本比例
                 train.fraction =1.0,# 用于训练的样本比例
                 cv.folds =5, # K折交叉验证，用于挑最佳迭代树数
                 verbose = FALSE # 是否打印训练过程
                 ) 

#用 CV 挑最佳迭代轮数：最佳树数
best_iter <- gbm.perf(model, method ="cv") #method="OOB"
best_iter

#某个变量的部份依赖图
plot(model,i.var=1,n.trees=best_iter) #i.var=1:2 可画前两个变量
plot(model,i.var=2,n.trees=best_iter) #i.var=1:2 可画前两个变量

#特征重要性
imp <- summary(model, n.trees = best_iter, plotit = T)


#预测
pred_prob_train <-predict(model, newdata=train, n.trees=best_iter, type="response")
pred_prob <-predict(model, newdata=test, n.trees=best_iter, type="response")
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type),positive = "1")


#### XGBoost ####
x <- as.matrix(train[,c(3:32)])
y <-train$Class
dtrain <- xgb.DMatrix(data = x, label = y)  #xgboost特别设计的数据格式
head(getinfo(dtrain,"label")) #通过getinfo函数获取dtrain中的信息
dtest <- xgb.DMatrix(data = as.matrix(test[,c(3:32)]), label = test$Class)  

# 简单封装函数：xgboost() ####
#交叉验证
depth_values <- c(3, 5, 7) #树的最大深度
eta_values <- c(0.01, 0.1, 0.3) #学习率
results <- list() #存放结果

for (depth in depth_values) {
  for (eta in eta_values) {
    params <- list(
      objective = "binary:logistic",
      max_depth = depth,
      eta = eta,
      nthread = 2 #使用的CPU线程数
    )
    
    cv_result <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 100,
      nfold = 5,
      metrics = "auc",
      verbose = FALSE
    )
    
    # 记录结果
    key <- paste0("depth=", depth, " and eta=", eta)
    results[[key]] <- min(cv_result$evaluation_log$test_error_mean)
  }
}

# 找到最佳参数
best_params <- names(which.min(unlist(results)))
cat("最佳参数组合:", best_params, "\n")

#训练一个模型，xgboost()
model <- xgboost(data=dtrain, #输入数据
                max.depth=3, #树的最大深度
                nrounds=100, #最大迭代次数-最终模型中树的数量
                objective="binary:logistic",
                nthread=2,#使用的CPU线程数
                eta=0.01 #学习率
                ) #lambda=1,#L2正则化,alpha=0 #L1正则化

#预测
pred_prob_train <-predict(model,dtrain)
pred_prob <-predict(model,dtest)
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type))

# 特征重要性图
importance_matrix <-xgb.importance(model=model)
importance_matrix
xgb.plot.importance(xgb.importance(model = model))
xgb.ggplot.importance(importance_matrix)
xgb.ggplot.importance(importance_matrix,n_clusters = 1) #指定变量类别为1

# 树结构信息查询及可视化
xgb.dump(model,with_stats=T)
xgb.plot.tree(model=model) #打印所有树
xgb.plot.multi.trees(model=model,fill=TRUE)#多棵树展示在一起
xgb.plot.deepness(model=model,positive = "1") #查看不同树深度下叶子的数量和每片叶子归一化后的加权覆盖

# 模型训练高级接口: xgb.train() ####
#准备预测变量和因变量放入专用格式中
train.mat <-xgb.DMatrix(data=as.matrix(train[,3:32]),label=train$Class)
train.mat
test.mat <-xgb.DMatrix(data=as.matrix(test[,3:32]),label=test$Class)
test.mat

#直接选择参数值
param<-list(objective="binary:logistic",
              booster="gbtree",
              eval_metric="auc",
              eta=0.3,
              max_depth=3,
              subsample=1,
              colsample_bytree=1,
              gamma=0.5)

#训练模型
set.seed(123)
model <-xgb.train(params=param,
                     data=train.mat,
                     nrounds=100)
xgb.plot.importance(xgb.importance(model = model)) #其他结果输出见前面的代码

# 基于caret函数调参 ###
# 设置超参数范围
grid <-expand.grid(nrounds=c(75,100), #最大迭代次数-最终模型中树的数量
                   eta=c(0.01,0.1,0.3), #学习率
                   gamma=c(0.5,0.25), #树中新增一个叶子分区时所需的最小减损
                   max_depth=c(2,3), #单个树最大深度
                   min_child_weight=1, #对树进行提升时使用的最小权重
                   subsample=0.5, #子样本数据占整个观测的比例
                   colsample_bytree=1 #建立树时随机抽取的特征数量
                   )


# 控制参数设定
cntrl <- trainControl(method="cv",
                      number=5,
                      verboseIter=F,
                      returnData=F,
                      returnResamp="final")

# 开始调优
set.seed(123)
train.xgb <-train(x=as.matrix(train[,3:32]),
                  y=train$Diagnosis,
                  trControl=cntrl,
                  tuneGrid=grid,
                  method="xgbTree")
train.xgb
# The final values used for the model were nrounds = 75, max_depth = 2, eta = 0.3, gamma = 0.25, colsample_bytree =
#   1, min_child_weight = 1 and subsample = 0.5.

ggplot(train.xgb)

#选择最优的参数值
param<-list(objective="binary:logistic",
              booster="gbtree",
              eval_metric="auc",
              eta=0.3,
              max_depth=2,
              subsample=0.5,
              colsample_bytree=1,
              gamma=0.25)


#拟合模型
set.seed(123)
model <-xgb.train(params=param,
                     data=train.mat,
                     nrounds=75)
xgb.plot.importance(xgb.importance(model = model)) #其他结果输出见前面的代码




#### LightGBM ####
#与xgboost函数很多地方十分相像，也有两种函数lightgbm()与lgb.train()
train.mat <-lgb.Dataset(data=as.matrix(train[,3:32]),label=train$Class)
train.mat
test.mat <-lgb.Dataset(data=as.matrix(test[,3:32]),label=test$Class)
test.mat
#lgb.Dataset.set.categorical(train.mat,c(1,4)) #可指定分类变量

#直接设定参数进行过模型训练
param <- list( boosting_type = 'gbdt', #梯度提升算法的具体实现框架        
               objective = 'binary',     #目标函数类型    
               metric = 'auc',     #评估指标    
               nthread = 4,        #并行线程数 
               learning_rate = 0.05,    #学习率     
               max_depth =5,      #树的最大深度（-1 表示无限制）  
               num_leaves = 40,    #每棵树的最大叶子数    
               feature_fraction = 0.1,  #每棵树训练时随机选择特征的比例    
               bagging_fraction = 0.1,  #每棵树训练时随机选择样本的比例（行采样Bagging）       
               bagging_freq = 1, #控制bagging的频率（每隔多少轮迭代执行一次）
               max_bin = 255, #每个特征最多可以分成的分箱（bin）的数量
               min_data_in_bin = 3 #每个分箱中至少需要包含的样本数量
               ) 

#拟合模型
set.seed(123)
model <-lgb.train(params=param,
                  data=train.mat,
                  nrounds=100)

#预测
pred_prob_train <-predict(model,as.matrix(train[,3:32]))
pred_prob <-predict(model,as.matrix(test[,3:32]))
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type),positive = "1")

# 特征重要性图
importance_matrix <-lgb.importance(model=model)
importance_matrix

lgb.plot.importance(lgb.importance(model = model,percentage = TRUE),
                    measure = "Gain", # 重要性度量标准："Gain", "Cover", "Frequency"
                    top_n = 30) # 显示前N个最重要的特征
xgb.ggplot.importance(importance_matrix)
xgb.ggplot.importance(importance_matrix,n_clusters = 1) #指定变量类别数为1


#调试learning_rate参数
grid_search <- expand.grid(    
  learning_rate = 2 ^ (-(8:1))
  )

perf_learning_rate_1 <- numeric(length = nrow(grid_search))

for(i in 1:nrow(grid_search)){    
   
  # 参数   
  params <- list(        
    boosting_type = 'gbdt', #梯度提升算法的具体实现框架        
    objective = 'binary',     #目标函数类型    
    metric = 'auc',     #评估指标    
    nthread = 4,        #并行线程数 
    learning_rate = grid_search[i, 'learning_rate'] ,    #学习率     
    max_depth =5,      #树的最大深度（-1 表示无限制）  
    num_leaves = 40,    #每棵树的最大叶子数    
    feature_fraction = 0.1,  #每棵树训练时随机选择特征的比例    
    bagging_fraction = 0.1,  #每棵树训练时随机选择样本的比例（行采样Bagging）       
    bagging_freq = 1, #控制bagging的频率（每隔多少轮迭代执行一次）
    max_bin = 255, #每个特征最多可以分成的分箱（bin）的数量
    min_data_in_bin = 3 #每个分箱中至少需要包含的样本数量
  )    
  # 交叉验证    
  lgb_tr_mod <- lgb.cv(        
    params=param,
    data=train.mat,
    nrounds=100,
    stratified = TRUE,        
    nfold = 10,        
    early_stopping_rounds = 10    
  )    
  perf_learning_rate_1[i] <- unlist(lgb_tr_mod$record_evals$valid$auc$eval)[length(unlist(lgb_tr_mod$record_evals$valid$auc$eval))]

  }

grid_search
grid_search$perf <- perf_learning_rate_1
ggplot(grid_search,aes(x = learning_rate, y = perf)) +  
  geom_point() +   
  geom_smooth()

param <- list( boosting_type = 'gbdt', #梯度提升算法的具体实现框架        
               objective = 'binary',     #目标函数类型    
               metric = 'auc',     #评估指标    
               nthread = 4,        #并行线程数 
               learning_rate = 0.25,    #学习率     
               max_depth =5,      #树的最大深度（-1 表示无限制）  
               num_leaves = 40,    #每棵树的最大叶子数    
               feature_fraction = 0.1,  #每棵树训练时随机选择特征的比例    
               bagging_fraction = 0.1,  #每棵树训练时随机选择样本的比例（行采样Bagging）       
               bagging_freq = 1, #控制bagging的频率（每隔多少轮迭代执行一次）
               max_bin = 255, #每个特征最多可以分成的分箱（bin）的数量
               min_data_in_bin = 3 #每个分箱中至少需要包含的样本数量
) 

#拟合模型
set.seed(123)
model <-lgb.train(params=param,
                  data=train.mat,
                  nrounds=100) #其他程序可以回到前面预测处



#### CatBoost ####
#准备数据
train.pool <- catboost.load_pool(data=as.matrix(train[,3:32]), 
                                label =train$Class) #cat_features =c(0,1,3:5)可指定分类变量
test.pool <- catboost.load_pool(data=as.matrix(test[,3:32]), 
                                 label =test$Class) #cat_features =c(0,1,3:5)可指定分类变量

#设置参数
param <- list(iterations = 1000,  #迭代次数
              loss_function = 'Logloss', #损失函数
              random_seed=123, #设定种子数
              learning_rate = 0.01, #学习率
              verbose = 0,  #不打印运行记录
              use_best_model = T, #使用最佳模型
              od_type = 'Iter', #过拟合检测
              od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
              eval_metric='AUC' #模型评估
)

#模型构建
model <- catboost.train(learn_pool = train.pool, 
                        params = param)


#预测
pred_prob_train <- catboost.predict(model, train.pool, prediction_type = "Probability") #预测分类结果，可设定“Probability”得到概率,设定“Class”得到分类
pred_prob <- catboost.predict(model, test.pool, prediction_type = "Probability") #预测分类结果，可设定“Probability”得到概率,设定“Class”得到分类
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type),positive = "1")


# 交叉验证
# 迭代次数
grid_search <- expand.grid(    
  iterations = c(5,50,60,100,300,500,700,1000)
)

perf_1 <- numeric(length = nrow(grid_search))


for(i in 1:nrow(grid_search)){    
  
  # 参数   
  params <- list(        
    iterations =  grid_search[i, 'iterations'],  #迭代次数
    loss_function = 'Logloss', #损失函数
    random_seed=123, #设定种子数
    learning_rate = 0.01, #学习率
    verbose = 0,  #不打印运行记录
    use_best_model = T, #使用最佳模型
    od_type = 'Iter', #过拟合检测
    od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
    eval_metric='AUC' #模型评估
  )    
  # 交叉验证    
  cvmod <-  catboost.cv(train.pool, fold_count = 10,param= params) 
  perf_1[i] <- mean(cvmod$test.AUC.mean)
  
}

grid_search$perf <- perf_1
grid_search
ggplot(grid_search,aes(x = iterations, y = perf)) +  
  geom_point() +   
  geom_smooth() #结果受迭代次数影响不大


#学习率
grid_search <- expand.grid(    
  learning_rate = 2 ^ (-(8:1))
)

perf_1 <- numeric(length = nrow(grid_search))


for(i in 1:nrow(grid_search)){    
  
  # 参数   
  params <- list(        
    iterations =  100,  #迭代次数
    loss_function = 'Logloss', #损失函数
    random_seed=123, #设定种子数
    learning_rate = grid_search[i, 'learning_rate'], #学习率
    verbose = 0,  #不打印运行记录
    use_best_model = T, #使用最佳模型
    od_type = 'Iter', #过拟合检测
    od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
    eval_metric='AUC' #模型评估
  )    
  # 交叉验证    
  cvmod <-  catboost.cv(train.pool, fold_count = 10,param= params) 
  perf_1[i] <- mean(cvmod$test.AUC.mean)
  
}

grid_search$perf <- perf_1
grid_search
ggplot(grid_search,aes(x = learning_rate, y = perf)) +  
  geom_point() +   
  geom_smooth() #结果受迭代次数影响不大

param <- list(iterations = 1000,  #迭代次数
              loss_function = 'Logloss', #损失函数
              random_seed=123, #设定种子数
              learning_rate = 0.125, #学习率
              verbose = 0,  #不打印运行记录
              use_best_model = T, #使用最佳模型
              od_type = 'Iter', #过拟合检测
              od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
              eval_metric='AUC' #模型评估
)

#模型构建
model <- catboost.train(learn_pool = train.pool, 
                        params = param)


######### 读取数据2：糖尿病案例 #############

data(PimaIndiansDiabetes2)
df <- na.omit(PimaIndiansDiabetes2)

# 数据准备
summary(df)
table(df$diabetes)
df$Class <- ifelse(df$diabetes == "pos", 1, 0)

# 划分训练/测试集
set.seed(123)
train_idx <- createDataPartition(df$Class, p = 0.7, list = FALSE)
train <- df[train_idx, -9]
test  <- df[-train_idx, -9]
head(train)

#### GBDT ####
x <- as.matrix(train[ , c(1:8)])
y <-train$Class
dat <- cbind(x , label = y)
dat_df <-as.data.frame(dat)

set.seed(123)
# 训练 GBDT（梯度提升树）
model <- gbm(  formula = label ~ ., # label 为因变量，其余所有列为自变量 
               data = dat_df,   # 输入数据框必须包含 label 列和特征
               distribution = "bernoulli", #二分类 
               n.trees =5000,  # 最大迭代树数 
               interaction.depth = 3,  # 单棵树的最大深度
               shrinkage =0.01, # 学习率
               n.minobsinnode =10,# 每个叶节点最少样本数
               bag.fraction =0.7,# 每轮随机采样的样本比例
               train.fraction =1.0,# 用于训练的样本比例
               cv.folds =5, # K折交叉验证，用于挑最佳迭代树数
               verbose = FALSE # 是否打印训练过程
) 

#用 CV 挑最佳迭代轮数：最佳树数
best_iter <- gbm.perf(model, method ="cv") #method="OOB"
best_iter

#某个变量的部份依赖图
plot(model,i.var=1,n.trees=best_iter) #i.var=1:2 可画前两个变量

#特征重要性
imp <- summary(model, n.trees = best_iter, plotit = T)


#预测
pred_prob_train <-predict(model, newdata=train, n.trees=best_iter, type="response")
pred_prob <-predict(model, newdata=test, n.trees=best_iter, type="response")
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type),positive = "1")

#XGBoost
#### XGBoost ####
x <- as.matrix(train[,c(1:8)])
y <-train$Class
dtrain <- xgb.DMatrix(data = x, label = y)  #xgboost特别设计的数据格式
head(getinfo(dtrain,"label")) #通过getinfo函数获取dtrain中的信息
dtest <- xgb.DMatrix(data = as.matrix(test[,c(1:8)]), label = test$Class)  

#交叉验证
depth_values <- c(3, 5, 7) #树的最大深度
eta_values <- c(0.001,0.005,0.01, 0.1, 0.3) #学习率
results <- list() #存放结果

for (depth in depth_values) {
  for (eta in eta_values) {
    params <- list(
      objective = "binary:logistic",
      max_depth = depth,
      eta = eta,
      nthread = 2 #使用的CPU线程数
    )
    
    cv_result <- xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 1000,
      nfold = 5,
      metrics = "auc",
      verbose = FALSE
    )
    
    # 记录结果
    key <- paste0("depth=", depth, " and eta=", eta)
    results[[key]] <- min(cv_result$evaluation_log$test_error_mean)
  }
}

# 找到最佳参数
best_params <- names(which.min(unlist(results)))
cat("最佳参数组合:", best_params, "\n")

#训练一个模型，xgboost()
model <- xgboost(data=dtrain, #输入数据
                 max.depth=3, #树的最大深度
                 nrounds=1000, #最大迭代次数-最终模型中树的数量
                 objective="binary:logistic",
                 nthread=2,#使用的CPU线程数
                 eta=0.001 #学习率
) #lambda=1,#L2正则化,alpha=0 #L1正则化

#预测
pred_prob_train <-predict(model,dtrain)
pred_prob <-predict(model,dtest)
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type))

# 特征重要性图
importance_matrix <-xgb.importance(model=model)
importance_matrix
xgb.plot.importance(xgb.importance(model = model))
xgb.ggplot.importance(importance_matrix)
xgb.ggplot.importance(importance_matrix,n_clusters = 1) #指定变量类别为1


#### LightGBM ####
train.mat <-lgb.Dataset(data=as.matrix(train[,1:8]),label=train$Class)
test.mat <-lgb.Dataset(data=as.matrix(test[,1:8]),label=test$Class)
#lgb.Dataset.set.categorical(train.mat,c(1,4)) #可指定分类变量

#直接设定参数进行过模型训练
param <- list( boosting_type = 'gbdt', #梯度提升算法的具体实现框架        
               objective = 'binary',     #目标函数类型    
               metric = 'auc',     #评估指标    
               nthread = 4,        #并行线程数 
               learning_rate = 0.01,    #学习率     
               max_depth =3,      #树的最大深度（-1 表示无限制）  
               num_leaves = 40,    #每棵树的最大叶子数    
               feature_fraction = 0.4,  #每棵树训练时随机选择特征的比例    
               bagging_fraction = 0.4,  #每棵树训练时随机选择样本的比例（行采样Bagging）       
               bagging_freq = 1, #控制bagging的频率（每隔多少轮迭代执行一次）
               max_bin = 100, #每个特征最多可以分成的分箱（bin）的数量
               min_data_in_bin = 3 #每个分箱中至少需要包含的样本数量
) 

#拟合模型
set.seed(123)
model <-lgb.train(params=param,
                  data=train.mat,
                  nrounds=1000)

#预测
pred_prob_train <-predict(model,as.matrix(train[,1:8]))
pred_prob <-predict(model,as.matrix(test[,1:8]))
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type),positive = "1")

# 特征重要性图
importance_matrix <-lgb.importance(model=model)
importance_matrix

lgb.plot.importance(lgb.importance(model = model,percentage = TRUE),
                    measure = "Gain", # 重要性度量标准："Gain", "Cover", "Frequency"
                    top_n = 30) # 显示前N个最重要的特征
xgb.ggplot.importance(importance_matrix)
xgb.ggplot.importance(importance_matrix,n_clusters = 1) #指定变量类别数为1

#### CatBoost ####
#准备数据
train.pool <- catboost.load_pool(data=as.matrix(train[,1:8]), 
                                 label =train$Class) #cat_features =c(0,1,3:5)可指定分类变量
test.pool <- catboost.load_pool(data=as.matrix(test[,1:8]), 
                                label =test$Class) #cat_features =c(0,1,3:5)可指定分类变量

#设置参数
param <- list(iterations = 1000,  #迭代次数
              loss_function = 'Logloss', #损失函数
              random_seed=123, #设定种子数
              learning_rate = 0.01, #学习率
              verbose = 0,  #不打印运行记录
              use_best_model = T, #使用最佳模型
              od_type = 'Iter', #过拟合检测
              od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
              eval_metric='AUC' #模型评估
)

#模型构建
model <- catboost.train(learn_pool = train.pool, 
                        params = param)


#预测
pred_prob_train <- catboost.predict(model, train.pool, prediction_type = "Probability") #预测分类结果，可设定“Probability”得到概率,设定“Class”得到分类
pred_prob <- catboost.predict(model, test.pool, prediction_type = "Probability") #预测分类结果，可设定“Probability”得到概率,设定“Class”得到分类
head(pred_prob)
range(pred_prob)

#AUC及其95%置信区间
roc_train <- roc(train$Class, pred_prob_train)
auc_val_train <- auc(train$Class, pred_prob_train)
auc_val_train
ci(roc_train)

roc <- roc(test$Class, pred_prob)
auc_val <- auc(test$Class, pred_prob)
auc_val
ci(roc)

#ROC曲线
p1 <- ggroc(roc(train$Class, pred_prob_train), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Training ROC (AUC =", round(auc_val_train, 3), ")")) +
  theme_minimal() 

p2 <- ggroc(roc(test$Class, pred_prob), color = "black", size = 1.2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey" , lwd=1) +
  labs(title = paste("Testing ROC (AUC =", round(auc_val, 3), ")")) +
  theme_minimal() 

grid.arrange(p1, p2, ncol = 2)

#混淆矩阵
pred_type <-ifelse(pred_prob>0.5,1,0)
head(pred_type)
confusionMatrix(factor(test$Class),factor(pred_type),positive = "1")


# 交叉验证
# 迭代次数
grid_search <- expand.grid(    
  iterations = c(5,50,60,100,300,500,700,1000)
)

perf_1 <- numeric(length = nrow(grid_search))


for(i in 1:nrow(grid_search)){    
  
  # 参数   
  params <- list(        
    iterations =  grid_search[i, 'iterations'],  #迭代次数
    loss_function = 'Logloss', #损失函数
    random_seed=123, #设定种子数
    learning_rate = 0.01, #学习率
    verbose = 0,  #不打印运行记录
    use_best_model = T, #使用最佳模型
    od_type = 'Iter', #过拟合检测
    od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
    eval_metric='AUC' #模型评估
  )    
  # 交叉验证    
  cvmod <-  catboost.cv(train.pool, fold_count = 10,param= params) 
  perf_1[i] <- mean(cvmod$test.AUC.mean)
  
}

grid_search$perf <- perf_1
grid_search
ggplot(grid_search,aes(x = iterations, y = perf)) +  
  geom_point() +   
  geom_smooth() #结果受迭代次数影响不大


#学习率
grid_search <- expand.grid(    
  learning_rate = 2 ^ (-(8:1))
)

perf_1 <- numeric(length = nrow(grid_search))


for(i in 1:nrow(grid_search)){    
  
  # 参数   
  params <- list(        
    iterations =  100,  #迭代次数
    loss_function = 'Logloss', #损失函数
    random_seed=123, #设定种子数
    learning_rate = grid_search[i, 'learning_rate'], #学习率
    verbose = 0,  #不打印运行记录
    use_best_model = T, #使用最佳模型
    od_type = 'Iter', #过拟合检测
    od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
    eval_metric='AUC' #模型评估
  )    
  # 交叉验证    
  cvmod <-  catboost.cv(train.pool, fold_count = 10,param= params) 
  perf_1[i] <- mean(cvmod$test.AUC.mean)
  
}

grid_search$perf <- perf_1
grid_search
ggplot(grid_search,aes(x = learning_rate, y = perf)) +  
  geom_point() +   
  geom_smooth() #

param <- list(iterations = 1000,  #迭代次数
              loss_function = 'Logloss', #损失函数
              random_seed=123, #设定种子数
              learning_rate = 0.01, #学习率
              verbose = 0,  #不打印运行记录
              use_best_model = T, #使用最佳模型
              od_type = 'Iter', #过拟合检测
              od_wait = 10 ,  #得到最佳阈值后继续迭代的次数
              eval_metric='AUC' #模型评估
)

#模型构建
model <- catboost.train(learn_pool = train.pool, 
                        params = param)