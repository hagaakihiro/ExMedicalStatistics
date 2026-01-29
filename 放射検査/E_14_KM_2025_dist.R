# 2025 report

library(survival)
data = read.csv("E14_2.csv")
kp=survfit(Surv(survival_period,dead_status==1)~1, data)
summary(kp)
plot(kp,lty=1,conf.int=FALSE,mark.t=T)
#stop()

medmak = median(data$maker)
n = nrow(data)
val = numeric(length=n)
roc = numeric(length=n)
k = 0
for (i in 1:n)
{
    if(data$maker[i] < medmak){
        val[i] = 0
    }
    else{
        val[i] = 1
    }
    
    if(data$survival_period[i] >= 40){
        roc[i] = 1
        k = k + 1
    }
    else{
        roc[i] = 0
    }
}
print("All Data:")
print(n)
print("Data for surv period > 40")
print(k)

print("Median (maker) :")
print(medmak)

print("mean surv for < medmak")
print(mean(data[data$maker<medmak,]$survival_period))

print("mean surv for > medmak")
print(mean(data[data$maker>medmak,]$survival_period))


data = data.frame(data,val)
kp2=survfit(Surv(survival_period,dead_status==1)~val,data)

plot(kp2,lty=1:3,conf.int=FALSE,mark.t=T)
pval <- survdiff(Surv(survival_period,dead_status==1)~val,data,rho=1)
print(pval)
stop()

######### ROC #########
library(ROCR)

data = data.frame(data,roc)
rocdata = data[data$dead_status==1,]
print("Num. of dead_status = 1")
print(nrow(rocdata))

print("Num. of surv > 40")
print(nrow(rocdata[rocdata$roc==1,]))

#print(rocdata)
pred1 = prediction(rocdata$maker, rocdata$roc)
perf = performance(pred1,"tpr","fpr")
plot(perf,col=2)
auc.tmp <- performance(pred1,"auc")
auc <- as.numeric(auc.tmp@y.values)
print(auc)
# end
