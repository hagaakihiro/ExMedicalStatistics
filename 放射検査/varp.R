# varp.R
varp <- function(x){
    sampvar <- var(x)*(length(x)-1)/length(x)
    sampvar
}
