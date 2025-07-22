##载入软件包、数据和network
library(ergm)
data(florentine)
flomarriage

#绘图
plot(flomarriage,
     main="Florentine Marriage",
     cex.main=0.8,
     label = network.vertex.names(flomarriage)) # Plot the network

#构局统计
summary(flomarriage ~ edges + triangles)

#ERGM
flomodel.01 = ergm(flomarriage ~ edges)
summary(flomodel.01)

flomodel.02 = ergm(flomarriage ~ edges + triangle)
summary(flomodel.02)

#模型诊断
flomodel.02.gof = gof(flomodel.02)
flomodel.02.gof
plot(flomodel.02.gof)

#根据模型模拟出一个网络
flomodel.02.sim = simulate(flomodel.02,nsim=10)
summary(flomodel.02.sim)

#与观察到的网络进行对比
plot(flomodel.02.sim[[10]],
     label= flomodel.02.sim[[10]] %v% "vertex.names")

#原模型效果不好，用新的模型
wealth = flomarriage %v% 'wealth' # %v% references vertex attributes
wealth

plot(flomarriage,
     vertex.cex=wealth/25,
     main="Florentine marriage by wealth", cex.main=0.8,
     label = network.vertex.names(flomarriage))

flomodel.03 = ergm(flomarriage ~ edges + nodecov('wealth'))
summary(flomodel.03)

flomodel.03.gof = gof(flomodel.03)
flomodel.03.gof
plot(flomodel.03.gof)

flomodel.03.sim = simulate(flomodel.03,nsim=10)
summary(flomodel.03.sim)

plot(flomodel.03.sim[[3]],
     label= flomodel.03.sim[[3]] %v% "vertex.names",
     vertex.cex = (flomodel.03.sim[[3]] %v% "wealth")/25)

flomodel.03 = ergm(flomarriage ~ edges + nodecov('wealth') + kstar(2))
summary(flomodel.03)

#原模型效果不好，用新的模型
priorates = flomarriage %v% 'priorates' # %v% references vertex attributes
priorates

plot(flomarriage,
     vertex.cex=priorates/25,
     main="Florentine marriage by wealth", cex.main=0.8,
     label = network.vertex.names(flomarriage))

flomodel.03 = ergm(flomarriage ~ edges + nodecov('wealth') + nodecov('priorates'))
summary(flomodel.03)

#新的列示
install.packages("texreg")
library(texreg)
print(screenreg(list(flomodel.01,flomodel.02,flomodel.03)))

