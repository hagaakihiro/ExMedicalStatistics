# 2025 report

data = read.csv("E_1.csv")
ymax = 25
xmax = 15
xmin = -5
hist(data$A,xlim=c(xmin,xmax), ylim=c(0,ymax), breaks=seq(-10,ymax,1),col = rgb(1, 0, 0, alpha = 0.5))
par(new=T)
hist(data$B,xlim=c(xmin,xmax), ylim=c(0,ymax),breaks=seq(-10,ymax,1),col = rgb(0, 0, 1, alpha = 0.5))
#stop()
hist(data$C,xlim=c(xmin,xmax), ylim=c(0,ymax), breaks=seq(-10,ymax,1),col = rgb(1, 0, 0, alpha = 0.5))
par(new=T)
hist(data$D,xlim=c(xmin,xmax), ylim=c(0,ymax),breaks=seq(-10,ymax,1),col = rgb(0, 0, 1, alpha = 0.5))
#stop()



meanA = mean(data$A)
sdA = sd(data$A)
medianA = median(data$A)
meanB = mean(data$B)
sdB = sd(data$B)
medianB = median(data$B)
print("A:")
cat("mean",meanA,"\n")
cat("sd",sdA,"\n")
cat("median",medianA,"\n")
print("B:")
cat("mean",meanB,"\n")
cat("sd",sdB,"\n")
cat("median",medianB,"\n")

print("")
meanC = mean(data$C)
sdC = sd(data$C)
medianC = median(data$C)
meanD = mean(data$D)
sdD = sd(data$D)
medianD = median(data$D)
print("C:")
cat("mean",meanC,"\n")
cat("sd",sdC,"\n")
cat("median",medianC,"\n")
print("D:")
cat("mean",meanD,"\n")
cat("sd",sdD,"\n")
cat("median",medianD,"\n")


print("var-test")
print(var.test(data$A,data$B))

print("t-test")
print(t.test(data$A,data$B))

######### ROC #########
library(ROCR)

dataROC = c(data$A, data$B)
truevalue = c(rep(0,length(data$A)), rep(1,length(data$B)))

dataROC2 = c(data$C, data$D)
truevalue2 = c(rep(0,length(data$C)), rep(1,length(data$D)))

#print(truevalue)


pred1 = prediction(dataROC, truevalue)
pred2 = prediction(dataROC2, truevalue2)

perf = performance(pred1,"tpr","fpr")
plot(perf,col=2)
#stop()
#par(new=T)
perf2 = performance(pred2,"tpr","fpr")
plot(perf2,col=4)

auc.tmp <- performance(pred1,"auc")
auc <- as.numeric(auc.tmp@y.values)
print(auc)

auc2.tmp <- performance(pred2,"auc")
auc2 <- as.numeric(auc2.tmp@y.values)
print(auc2)

pred = pred1
ndata = length(dataROC)
table = data.frame(Cutoff=unlist(pred@cutoffs),TP=unlist(pred@tp), FP=unlist(pred@fp),FN=unlist(pred@fn),TN=unlist(pred@tn),Sensitivity=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),Specificity=unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),Accuracy=((unlist(pred@tp)+unlist(pred@tn))/ndata), BER=(1-unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)))/2+(1-unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)))/2, Odds=unlist(pred@tp)*unlist(pred@tn)/unlist(pred@fp)/unlist(pred@fn),Risk=(unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fp)))/(unlist(pred@fn)/(unlist(pred@fn)+unlist(pred@tn))))
print(table)


