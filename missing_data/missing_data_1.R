# github blog post
library(VIM)
library(mice)
library(DMwR)
library(ggplot2)
library(gridExtra)

z1 = rnorm(500,0,1)
z2 = rnorm(500,0,1)
z3 = rnorm(500,0,1)
y1 = 1+z1; y2 = 5+2*z1+z2



# MCAR
# original data
df.full = data.frame(cbind(y1,y2))

u = z3
na.idx = which(u<0)
df = data.frame(cbind(y1,y2))
df.complete = df[-na.idx,]
df.incomplete = df[na.idx,]
df[na.idx,]$y2 = NA

marginplot(df,main='Margin plot of data')
na.plot <- aggr(df,numbers=TRUE,sortVars=TRUE,labels=names(df),
                ylab=c('Histogram of missing data','Pattern'),gap=3,cex.axis=0.7)

# 1) check distribution
df.full$missing = 0
df.full[na.idx,]$missing = 1
df.full$missing = as.factor(df.full$missing)

ggplot(data=df.full,aes(y1,y2,color=missing))+geom_point()+
  scale_color_manual(values=c('black','red'))+
  ggtitle('MCAR data')

# 2) complete vs incomplete
h1 <- ggplot(df.complete,aes(y1))+
  geom_histogram(fill='red',color='black',alpha=0.2)+
  coord_cartesian(xlim = c(-3,4)) +
  ggtitle('MCAR - Complete')

h2 <- ggplot(df.incomplete,aes(y1))+
  geom_histogram(fill='blue',color='black',alpha=0.2)+
  coord_cartesian(xlim = c(-3,4)) +
  ggtitle('MCAR - Incomplete')
grid.arrange(h1, h2, ncol=2)


# 3) full vs complete
plot.mean.full <- function(data){
  ggplot(data,aes(y2))+
    geom_histogram(fill='blue',color='black',alpha=0.2)+
    geom_vline(xintercept=mean(data$y2),color='red',size=1)+
    coord_cartesian(xlim = c(-1,12)) +
    ggtitle('Full data')
}

plot.mean.comp <- function(data){
  ggplot(data,aes(y2))+
    geom_histogram(fill='green',color='black',alpha=0.2)+
    geom_vline(xintercept=mean(data$y2),color='red',size=1)+
    coord_cartesian(xlim = c(-1,12)) +
    ggtitle('Complete data')
}

g1 <- plot.mean.full(df.full)
g2 <- plot.mean.comp(df.complete)
grid.arrange(g1, g2, ncol=2)

# 4) mean and variance comparison
mean(df.complete$y2)  
mean(df.incomplete$y2)
mean(df.full$y2)    

var(df.complete$y2)  
var(df.incomplete$y2)
var(df.full$y2)    

# 5
reg.mcar.comp <- lm(y2~y1,df.complete)
reg.mcar.full <- lm(y2~y1, df.full)

ggplot(df.complete,aes(y1,y2))+
  geom_point()+geom_abline(intercept = reg.mcar.comp$coefficients[1], slope = reg.mcar.comp$coefficients[2],color='red',size=1)+
  geom_abline(intercept = reg.mcar.full$coefficients[1], slope = reg.mcar.full$coefficients[2],color='blue',size=1)+
  ggtitle('MCAR - fitted regression line')


# MAR
# original data
df.full2 = data.frame(cbind(y1,y2))

u2 = 2*(y1-1)+z3
na.idx2 = which(u2<0)
df2 = data.frame(cbind(y1,y2))
df2.complete = df2[-na.idx2,]
df2.incomplete = df2[na.idx2,]
df2[na.idx2,]$y2 = NA

marginplot(df2,main='Margin plot of data')
na.plot <- aggr(df2,numbers=TRUE,sortVars=TRUE,labels=names(df),
                ylab=c('Histogram of missing data','Pattern'),gap=3,cex.axis=0.7)

# 1) check distribution
df.full2$missing = 0
df.full2[na.idx,]$missing = 1
df.full2$missing = as.factor(df.full2$missing)

ggplot(data=df.full2,aes(y1,y2,color=missing))+geom_point()+
  scale_color_manual(values=c('black','red'))+
  ggtitle('MAR data')

# 2) complete vs incomplete
h1 <- ggplot(df2.complete,aes(y1))+
  geom_histogram(fill='red',color='black',alpha=0.2)+
  coord_cartesian(xlim = c(-3,4)) +
  ggtitle('MAR - Complete')

h2 <- ggplot(df2.incomplete,aes(y1))+
  geom_histogram(fill='blue',color='black',alpha=0.2)+
  coord_cartesian(xlim = c(-3,4)) +
  ggtitle('MAR - Incomplete')
grid.arrange(h1, h2, ncol=2)

# 3) full vs complete
plot.mean.full <- function(data){
  ggplot(data,aes(y2))+
    geom_histogram(fill='blue',color='black',alpha=0.2)+
    geom_vline(xintercept=mean(data$y2),color='red',size=1)+
    coord_cartesian(xlim = c(-1,12)) +
    ggtitle('Full data')
}

plot.mean.comp <- function(data){
  ggplot(data,aes(y2))+
    geom_histogram(fill='green',color='black',alpha=0.2)+
    geom_vline(xintercept=mean(data$y2),color='red',size=1)+
    coord_cartesian(xlim = c(-1,12)) +
    ggtitle('Complete data')
}

g1 <- plot.mean.full(df.full2)
g2 <- plot.mean.comp(df2.complete)
grid.arrange(g1, g2, ncol=2)

# 4) mean and variance comparison
mean(df2.complete$y2)  
mean(df2.incomplete$y2)
mean(df.full2$y2)    

var(df2.complete$y2)  
var(df2.incomplete$y2)
var(df.full2$y2)    

# 5
reg.mar.comp <- lm(y2~y1,df2.complete)
reg.mar.full <- lm(y2~y1, df.full2)

ggplot(df2.complete,aes(y1,y2))+
  geom_point()+geom_abline(intercept = reg.mar.comp$coefficients[1], slope = reg.mar.comp$coefficients[2],color='red',size=1)+
  geom_abline(intercept = reg.mar.full$coefficients[1], slope = reg.mar.full$coefficients[2],color='blue',size=1)+
  ggtitle('MAR - fitted regression line')

# MNAR
# original data
df.full3 = data.frame(cbind(y1,y2))

u3 = 2*(y2-5)+z3
na.idx3 = which(u3<0)
df3 = data.frame(cbind(y1,y2))
df3.complete = df3[-na.idx3,]
df3.incomplete = df3[na.idx3,]
df3[na.idx3,]$y2 = NA

marginplot(df3,main='Margin plot of data')
na.plot <- aggr(df3,numbers=TRUE,sortVars=TRUE,labels=names(df),
                ylab=c('Histogram of missing data','Pattern'),gap=3,cex.axis=0.7)

# 1) check distribution
df.full3$missing = 0
df.full3[na.idx,]$missing = 1
df.full3$missing = as.factor(df.full3$missing)

ggplot(data=df.full3,aes(y1,y2,color=missing))+geom_point()+
  scale_color_manual(values=c('black','red'))+
  ggtitle('MNAR data')

# 2) complete vs incomplete
h1 <- ggplot(df3.complete,aes(y1))+
  geom_histogram(fill='red',color='black',alpha=0.2)+
  coord_cartesian(xlim = c(-3,4)) +
  ggtitle('MNAR - Complete')

h2 <- ggplot(df3.incomplete,aes(y1))+
  geom_histogram(fill='blue',color='black',alpha=0.2)+
  coord_cartesian(xlim = c(-3,4)) +
  ggtitle('MNAR - Incomplete')
grid.arrange(h1, h2, ncol=2)

# 3) full vs complete
plot.mean.full <- function(data){
  ggplot(data,aes(y2))+
    geom_histogram(fill='blue',color='black',alpha=0.2)+
    geom_vline(xintercept=mean(data$y2),color='red',size=1)+
    coord_cartesian(xlim = c(-1,12)) +
    ggtitle('Full data')
}

plot.mean.comp <- function(data){
  ggplot(data,aes(y2))+
    geom_histogram(fill='green',color='black',alpha=0.2)+
    geom_vline(xintercept=mean(data$y2),color='red',size=1)+
    coord_cartesian(xlim = c(-1,12)) +
    ggtitle('Complete data')
}

g1 <- plot.mean.full(df.full3)
g2 <- plot.mean.comp(df3.complete)
grid.arrange(g1, g2, ncol=2)

# 4) mean and variance comparison
mean(df3.complete$y2)  
mean(df3.incomplete$y2)
mean(df.full3$y2)    

var(df3.complete$y2)  
var(df3.incomplete$y2)
var(df.full3$y2)    

# 5
reg.mnar.comp <- lm(y2~y1,df3.complete)
reg.mnar.full <- lm(y2~y1, df.full3)

ggplot(df3.complete,aes(y1,y2))+
  geom_point()+geom_abline(intercept = reg.mnar.comp$coefficients[1], slope = reg.mnar.comp$coefficients[2],color='red',size=1)+
  geom_abline(intercept = reg.mnar.full$coefficients[1], slope = reg.mnar.full$coefficients[2],color='blue',size=1)+
  ggtitle('MNAR - fitted regression line')


par(mfrow=c(1,1))
marginplot(df,main='Margin plot of data')
marginplot(df2,main='Margin plot of data')
marginplot(df3,main='Margin plot of data')

       
