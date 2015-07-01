N <- 10

components <- sample(1:2,prob=c(0.05,0.95),size=N,replace=TRUE)
mus
mus <- c(0,0)
sds <- sqrt(c(1,9))

samples <- rnorm(n=N,mean=mus[components],sd=sds[components])
samples
