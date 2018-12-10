#Random Forest Time Series
#Load Time series.RData first!
#load("./Time series.Rdata")
library(randomForest)

Y1s = cbind(Y1[1:995],Y1[2:996],Y1[3:997],Y1[4:998],Y1[5:999])
Y2s = cbind(Y2[1:995],Y2[2:996],Y2[3:997],Y2[4:998],Y2[5:999])
Is = cbind(I[1:995],I[2:996],I[3:997],I[4:998],I[5:999])

Y1incs = Y1[2:1000] - Y1[1:999]
Y2incs = Y2[2:1000] - Y2[1:999]
Y1incss = cbind(Y1incs[1:995],Y1incs[2:996],Y1incs[3:997],Y1incs[4:998])
Y2incss = cbind(Y2incs[1:995],Y2incs[2:996],Y2incs[3:997],Y2incs[4:998])
past = cbind(Y1s,Y2s, Y1incss, Y2incss,Is)
#past= cbind(Y1s, Y2s, Is)
Y1ts = cbind(Y1[1001:1195],Y1[1002:1196],Y1[1003:1197],Y1[1004:1198],Y1[1005:1199])
Y2ts = cbind(Y2[1001:1195],Y2[1002:1196],Y2[1003:1197],Y2[1004:1198],Y2[1005:1199])
Its = cbind(I[1001:1195],I[1002:1196],I[1003:1197],I[1004:1198],I[1005:1199])
Y1incts = Y1[1001:1200] - Y1[1000:1199]
Y2incts = Y2[1001:1200] - Y2[1000:1199]
Y1inctss = cbind(Y1incts[1:195],Y1incts[2:196],Y1incts[3:197],Y1incts[4:198])
Y2inctss = cbind(Y2incts[1:195],Y2incts[2:196],Y2incts[3:197],Y2incts[4:198])
testpast = cbind(Y1ts,Y2ts,Y1inctss,Y2inctss,Its)
#testpast = cbind(Y1ts, Y2ts, Its)
Y1pred = randomForest(past,Y1[6:1000],xtest=testpast,ytest=Y1[1006:1200],keep.forest = TRUE)
Y2pred = randomForest(past,Y2[6:1000],xtest=testpast,ytest=Y2[1006:1200],keep.forest = TRUE)


Y1incpred = randomForest(past,Y1incs[5:999],xtest=testpast,ytest=Y1incts[2:200],keep.forest = TRUE)
Y2incpred = randomForest(past,Y2incs[5:999],xtest=testpast,ytest=Y2incts[2:200],keep.forest = TRUE)

Y1byinc = vector("numeric",1000)
Y1byinc[1] = Y1[5]
for (i in 1:1199){
Y1byinc[i+1] = Y1[5] + sum(Y1incpred$predicted[1:i])
}
fittt = lm(Y1incs[5:999]~past)
Y1byinclm = vector("numeric",1000)
Y1byinclm[1]=Y1[5]
for (i in 1:995){
  Y1byinclm[i+1]=Y1[5]+sum(fittt$fitted.values[1:i])
}

Y1byinct = vector("numeric",200)

Y1byinct[1] = Y1[1006]
for (i in 1:199){
  Y1byinct[i+1] = Y1[1006] + sum(Y1incpred$test$predicted[1:i])
}


Y2byinc = vector("numeric",1000)
Y2byinc[1] = Y2[5]
for (i in 1:995){
  Y2byinc[i+1] = Y2[5] + sum(Y2incpred$predicted[1:i])
}
fittt = lm(Y2incs[5:999]~past)
Y2byinclm = vector("numeric",1000)
Y2byinclm[1]=Y2[5]
for (i in 1:999){
  Y2byinclm[i+1]=Y2[5]+sum(fittt$fitted.values[1:i])
}


Y2byinct = vector("numeric",200)

Y2byinct[1] = Y2[1006]
for (i in 1:199){
  Y2byinct[i+1] = Y2[1006] + sum(Y2incpred$test$predicted[1:i])
}

Y1b = Y1[6:1000] + Y1incpred$predicted
Y2b = Y2[6:1000] + Y2incpred$predicted

plot(Y1[6:1000], type="l")
lines(Y1byinclm, col="blue")
lines(Y1pred$predicted,col="red")
lines(Y1byinc, col="green")


plot(Y1[1005:1200],type="l")
lines(Y1pred$test$predicted,col="red")
lines(Y1byinct, col="green")



plot(Y2[5:1000], type="l")
lines(Y2byinclm, col="blue")
lines(Y2pred$predicted,col="red")
lines(Y2byinc, col="green")


plot(Y2[1000:1200],x=(1:1200)-5,type="l")
lines(Y2pred$test$predicted,col="red")
lines(Y2byinct, col="green")

Z1 = (Y1[6:1000]- Y1pred$predicted)^2
Z2 = (Y2[6:1000]- Y2pred$predicted)^2
Zc = (Y1[6:1000]-Y1pred$predicted)*(Y2[6:1000]-Y2pred$predicted)


Z1t = (Y1[1006:1200]-Y1pred$test$predicted)^2
Z2t = (Y1[1006:1200]-Y2pred$test$predicted)^2
Zct = (Y1[1006:1200]-Y1pred$test$predicted)*(Y1[1006:1200]-Y2pred$test$predicted)


Z1pred = randomForest(past,Z1,xtest=testpast,ytest=Z1t,keep.forest = TRUE)
Z2pred = randomForest(past,Z2,xtest=testpast,ytest=Z2t,keep.forest = TRUE)
Zcpred = randomForest(past,Zc,xtest=testpast,ytest=Zct,keep.forest = TRUE)
Z1preds = Z1pred$predicted
Z2preds = Z2pred$predicted
Zcpreds = Zcpred$predicted
Z1predst = Z1pred$test$predicted
Z2predst = Z2pred$test$predicted
Zcpredst = Zcpred$test$predicted
#Build a suitible square root matrix. Since this thing is symmetric, we can get a symmetric square root too.

Ms = sqrt(Z1preds*Z2preds-Zcpreds^2)
Mst = sqrt(Z1predst*Z2predst-Zcpredst^2)


Mt = sqrt((Z1preds + Z2preds) + 2*Ms)
Mtt = sqrt((Z1predst + Z2predst) + 2*Mst)
Y1pd = Y1[6:1000] - Y1pred$predicted
Y2pd = Y2[6:1000] - Y2pred$predicted
Y1pdt = Y1[1006:1200]-Y1pred$test$predicted
Y2pdt = Y2[1006:1200]-Y2pred$test$predicted

Id = matrix(c(1,0,0,1),2,2)
ZTestDat = c()
for(i in 1:995){
  MZtest = (matrix(c(Z1preds[i],Zcpreds[i],Zcpreds[i],Z2preds[i]),2,2) + Ms[i]*Id)/Mt[i]
  ZTestDat = cbind(ZTestDat, solve(MZtest)%*%rbind(Y1pd[i],Y2pd[i]))
}


ZTestDatt = c()
for(i in 1:195){
  MZtest = (matrix(c(Z1predst[i],Zcpredst[i],Zcpredst[i],Z2predst[i]),2,2) + Mst[i]*Id)/Mtt[i]
  ZTestDatt = cbind(ZTestDatt, solve(MZtest)%*%rbind(Y1pdt[i],Y2pdt[i]))
}


Z1z = (Y1[6:1000] - Y1pred$predicted)/sqrt(Z1pred$predicted)
Z2z = (Y2[6:1000] - Y2pred$predicted)/sqrt(Z2pred$predicted)
Z1tz = (Y1[1006:1200]-Y1pred$test$predicted)/sqrt(Z1pred$test$predicted)
Z2tz = (Y2[1006:1200]-Y2pred$test$predicted)/sqrt(Z2pred$test$predicted)



N = 100
library(MASS)
Y1start = Y1[1196:1200]
Y2start = Y2[1196:1200]
Y1istart = Y1[1197:1200] - Y1[1196:1199]
Y2istart = Y2[1197:1200] - Y2[1196:1199]
Is = c(I[1196:1200],Ifuture)
start = c(Y1start,Y2start,Y1istart,Y2istart,Is[1:5])
Y1s = c()
Y2s = c()
for(i in 1:100){
  Y1sim = c()
  Y2sim = c()
  print(i)
  Y1cur = Y1start
  Y2cur = Y2start
  Y1icur = Y1istart
  Y2icur = Y2istart
  Icur = Is[1:5]
  cur = c(Y1cur,Y2cur,Y1icur,Y2icur,Icur)
  for(k in 1:200){
    Z1c = predict(Z1pred,cur)
    Z2c = predict(Z2pred,cur)
    Zcc = predict(Zcpred,cur)
    Sigma = matrix(c(Z1c,Zcc,Zcc,Z2c),2,2)
    Y1c = predict(Y1pred,cur)

    Y2c = predict(Y2pred,cur)

    noise = mvrnorm(1,c(0,0),Sigma)
    Y1c = Y1c + noise[1]
    Y2c = Y2c + noise[2]
    Y1cur = c(Y1cur[2:5],Y1c)
    Y2cur = c(Y2cur[2:5],Y2c)
    Y1icur = c(Y1icur[2:4],Y1c - Y1cur[4])
    Y2icur = c(Y2icur[2:4],Y2c - Y2cur[4])
    Icur = Is[(1+i):(5+i)]
    Y1sim = c(Y1sim,Y1c)
    Y2sim = c(Y2sim,Y2c)
    cur = c(Y1cur,Y2cur,Y1icur,Y2icur,Icur)
  }
  Y1s = cbind(Y1s,Y1sim)
  Y2s = cbind(Y2s,Y2sim)
}
