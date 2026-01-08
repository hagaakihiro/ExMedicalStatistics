# E_11_lecture.R
library(ROCR)
ROCdata = read.csv("E_11_ROC.csv")
data0 <- subset(ROCdata,ROCdata$Histology=="squamous cell carcinoma")
data1 <- subset(ROCdata,ROCdata$Histology=="adenocarcinoma")
print("squamous cell carcinoma:")
print(nrow(data0))
print("adenocarcinoma:")
print(nrow(data1))

ndata = length(ROCdata$Histology)

val = numeric(length=ndata)
for (i in 1:ndata)
{
    if(ROCdata$Histology[i] == "squamous cell carcinoma"){
        val[i] = 1
    }
    else{
        val[i] = 0
    }
}
ROCdata = data.frame(ROCdata,val)

pred = prediction(ROCdata$Energy, ROCdata$val)
perf = performance(pred,"tpr","fpr")
plot(perf,col="Red",lwd = 2)
auc.tmp = performance(pred,"auc")
auc = as.numeric(auc.tmp@y.values)
print("AUC:")
print(auc)


table = data.frame(Cutoff=unlist(pred@cutoffs),TP=unlist(pred@tp), FP=unlist(pred@fp),FN=unlist(pred@fn),TN=unlist(pred@tn),Sensitivity=unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)),Specificity=unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)),Accuracy=((unlist(pred@tp)+unlist(pred@tn))/ndata), BER=(1-unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fn)))/2+(1-unlist(pred@tn)/(unlist(pred@fp)+unlist(pred@tn)))/2, Odds=unlist(pred@tp)*unlist(pred@tn)/unlist(pred@fp)/unlist(pred@fn),Risk=(unlist(pred@tp)/(unlist(pred@tp)+unlist(pred@fp)))/(unlist(pred@fn)/(unlist(pred@fn)+unlist(pred@tn))))
print(table)

############


nix = ncol(ROCdata)-1
aucmax = 0
jmax = 0
for ( j in 2:nix) {
    pred = prediction(ROCdata[,j], ROCdata$val)
    perf = performance(pred,"tpr","fpr")
    auc.tmp = performance(pred,"auc")
    auc = as.numeric(auc.tmp@y.values)
    if(aucmax < auc) {
        aucmax = auc
        jmax = j
    }
}
print("Feature giving Maximum AUC:")
print(colnames(ROCdata)[jmax])
print(aucmax)


# ここまで
