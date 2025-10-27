install.packages("randomForest")
install.packages("randomForestExplainer")

library(randomForest)
library(randomForestExplainer)

data(Boston,package="MASS")
Boston$chas<-as.logical(Boston$chas)

set.seed(2025)
forest<-randomForest(medv~.,data=Boston,localImp=TRUE)
forest 
min_depth_frame<-min_depth_distribution(forest)
min_depth_frame

plot_min_depth_distribution(min_depth_frame) #计算并图示前10重要变量
plot_min_depth_distribution(min_depth_frame,mean_sample="relevant_trees",k=15) #对大量缺失值，增加树的棵树

importance_frame<-measure_importance(forest)
importance_frame

plot_multi_way_importance(measure_importance(forest)) #多因子重要图


plot_importance_ggpairs(importance_frame) #因子重要性图

vars<-important_variables(importance_frame,k=5,measures = c("mean_min_depth","no_of_trees"))#提取重要变量
vars #"lstat" "rm"    "nox"   "crim"  "dis"  
