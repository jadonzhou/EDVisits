rm(list=ls())
dat<-read.csv("C:/Users/Administrator/Desktop/UCLA paper/Model/0929-prediction/WI.csv")
head(dat)
library(lars)
X <- as.matrix(dat[,2:122])
Y <- as.matrix(dat[,1])
model.lasso <- lars(x=X, y=Y, type='lar')
model.lasso
summary(model.lasso)
#画个图     
plot(model.lasso) 


rm(list = ls())
dat<-read.csv("E:/0. New starts/UCLA paper/Opioid prediction project/dataForPrediction.csv")
head(dat)
library(lars)
X <- as.matrix(dat[,2:65])
Y <- as.matrix(dat[,1])
model.lasso <- lars(x=X, y=Y, type='lar') 
model.lasso
summary(model.lasso)
#画个图     
plot(model.lasso) 


rm(list = ls())
dat<-read.csv("C:/Users/Administrator/Desktop/UCLA paper/Model/dataForPrediction.csv")
head(dat)
library(lars)
X <- as.matrix(dat[,2:101])
Y <- as.matrix(dat[,1])
model.lasso <- lars(x=X, y=Y, type='lar')  
model.lasso
summary(model.lasso)
#画个图     
plot(model.lasso) 



#下面做三个回归
# (1) 最小二乘回归
model.ols <- lm(Y~.-1, data=dat)
summary(model.ols)
#提取回归系数
coef.ols <- coef(model.ols)
#查看不等于0的回归系数
coef.ols[coef.ols!=0]  

# (2)  ridge regression（岭回归）
model.rid <- linearRidge(Y~.-1, data=dat)
#查看结果
summary(model.rid)
Call:
  linearRidge(formula = Y ~ . - 1, data = dat)


Coefficients:
  Estimate Scaled estimate Std. Error (scaled) t value (scaled) Pr(>|t|)    
X1  3.34326        18.15297             3.02490            6.001 1.96e-09 ***
  X2  1.55602         9.33877             3.24737            2.876  0.00403 ** 
  X3 -0.16886        -1.14619             3.43476            0.334  0.73860    
X4  1.18625         6.86298             3.47735            1.974  0.04842 *  
  X5  0.01244         0.05443             3.09414            0.018  0.98596    
X6 -0.22464        -1.33800             2.84413            0.470  0.63804    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Ridge parameter: 0.07890282, chosen automatically, computed using 5 PCs

Degrees of freedom: model 5.315 , variance 4.744 , residual 5.886 
#提取系数
coef.rid <- coef(model.rid)
#查看不等于0的系数
coef.rid[coef.rid!=0]                           
X1          X2          X3          X4          X5          X6 
3.34325841  1.55601878 -0.16886083  1.18625138  0.01243652 -0.22464097 
# (3) 做 lasso regression
#模型设定
library(lars)
X <- as.matrix(dat[,2:41])
Y <- as.matrix(dat[,1])
model.lasso <- lars(x=X, y=Y, type='lar')    
#画个图     
plot(model.lasso) 