# Bootstrap example

## 1. correlation
library(boot)
x <- c(15,26,10,9,15,20,18,11,8,20,7,9,10,11,11,10,12,17,11,10)
y <- c(95,71,83,91,102,87,93,100,104,94,113,96,83,84,102,100,105,121,86,100)

cor.X <- function(data,indices) cor(data[indices,1],data[indices,2])
boot.cor <- boot(data=cbind(x,y),statistic = cor.X,R=1000)
print(boot.cor)
hist(boot.cor$t,xlim=c(-1,1),nclass=20,main='bootstrap correlation',xlab='corr')
boot.ci(boot.cor,type=c('prec','bca'))


## 2. logistic regression
library(AER)
data(Affairs,package='AER')
# ynaffair variable
Affairs$ynaffair[Affairs$affairs==0] <- 0
Affairs$ynaffair[Affairs$affairs>0] <- 1
log.model <- glm(ynaffair~gender+age+yearsmarried+religiousness+rating,family=binomial(),data=Affairs)
summary(log.model)

# define coefficient function
coefs <- function(data,indices,formula){
  data.1<-data[indices,]
  fit <- glm(formula,family=binomial(),data=data.1)
  return(coef(fit))
}

log.boot <- boot(data=Affairs,statistic=coefs,R=1000,formula=ynaffair~gender+age+yearsmarried+religiousness+rating)
print(log.boot,index=1:6,digits=2)
# gender - confidence interval
boot.ci(log.boot,type=c('perc','bca'),conf=0.95,index=2)
# histogram
hist(log.boot$t[,2],xlab='coef of gender',main='bootstrap coefficient of gender')

