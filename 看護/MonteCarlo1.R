SampMean = numeric(length=10000)
for(i in 1:10000){
    sample = rnorm(16,6300,1700)
    SampMean[i] = mean(sample)
}
hist(SampMean,probability=T,xlim=c(5000,8000))
