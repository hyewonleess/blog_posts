# code for AR/MA

# AR
set.seed(0)
weights = c(0.7, 0.2, -0.1, -0.3)

AR.models <- list()
for (i in 1:4){
   AR.models[[i]] = arima.sim(n=1000, model=list(order = c(i,0,0),ar=weights[1:i]))
}

par(mfrow=c(4,3))
for (p in 1:4){
   plot.ts(AR.models[[p]], main=paste('AR(',p,')',sep=""))
   acf(AR.models[[p]], lag.max = 10, main="")
   pacf(AR.models[[p]], lag.max = 10, ylab='PACF', main="")
}

# MA
set.seed(0)
weights = c(0.7, 0.2, -0.1, -0.3)

MA.models <- list()
for (i in 1:4){
   MA.models[[i]] = arima.sim(n=1000, model=list(order = c(0,0,i),ma=weights[1:i]))
}

par(mfrow=c(4,3))
for (p in 1:4){
   plot.ts(MA.models[[p]], main=paste('MA(',p,')',sep=""))
   acf(MA.models[[p]], lag.max = 10, main="")
   pacf(MA.models[[p]], lag.max = 10, ylab='PACF', main="")
}