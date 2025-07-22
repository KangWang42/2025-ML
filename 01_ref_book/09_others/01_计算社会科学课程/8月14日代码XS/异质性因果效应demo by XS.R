#安装causlatree包
# library(devtools) 
# install_github("susanathey/causalTree")

#设置工作路径
setwd("~/Desktop/Dropbox 备份/2023xs_machine learning")

#设置随机数初始值 
set.seed(1)  

#读入数据
dta<-read.csv("cgssdemo.csv")


####因果树
library("causalTree")
tr <- causalTree(lninc ~ gender+age+age2+hukou+fedu+medu+party,   #函数
                 data = dta,  #数据
                 treatment = dta$treat,  #处理变量
                 split.Rule = "CT",   #划分依据：CT是指causal tree，重视处理效应异质性，fit是指优先模型拟合而不是处理效应
                 cv.option = "matching", #交叉验证的评估函数类型
                 split.Honest = T, #用互不重合的数据 分别进行split 和 estimate
                 cv.Honest = T,  
                 xval = 5, # 几折交叉验证
                 minsize = 20,#叶片内最小样本数量
                 propensity = 0.5, #倾向性得分阈值
                 split.alpha = 0.5)  #样本选择函数的系数： 异质性与准确性的偏好系数
rpart.plot(tr)

variable_importance(tr)

#split.Honest = T, cv.Honest = T  使用“诚实”算法
#minsize ：叶节点最小样本数
#propensity：如何根据ps分配处理组与对照组
#split.alpha:

#####练习：模型调参，改变参数设置看会发生什么变化
