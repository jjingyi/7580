## 7580 project

## Set A
## Unifrm(0, 1)
# 1 group

con.int.a<-function(alpha,n,m){
  set.seed(45)
  est.var <- replicate(m,
                     {x <- runif(n)
                      tau<-sum(abs(x-median(x)))/n
                      delta<-(mean(x)-median(x))/tau
                      gamma<-var(x)/tau^2
                      #gamma<-1.23
                      (delta^2+gamma-1)/n})
  tau <- replicate(m,
                   {x <- runif(n)
                    sum(abs(x-median(x)))/n})
  
  coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y<-cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var))
  return(exp(Y))
}

## see if it CIs cover true_tau


coverage.a<-function(alpha,n,m){
  true_tau<-sqrt((1/12)/1.33)
  
  ans<-con.int.a(alpha,n,m)
  L<-(ans[,1]<true_tau)*1
  U<-(ans[,2]>true_tau)*1
  count=L*U

  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}
#setA # n=10 # 

m=50000
a<-data.frame(n=10,alpha_0.1=coverage.a(0.1,10,m), alpha_0.05=coverage.a(0.05,10,m),alpha_0.01=coverage.a(0.01,10,m))
#set A #n=25 
b<-data.frame(n=25,alpha_0.1=coverage.a(0.1,25,m), alpha_0.05=coverage.a(0.05,25,m),alpha_0.01=coverage.a(0.01,25,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=coverage.a(0.1,50,m), alpha_0.05=coverage.a(0.05,50,m),alpha_0.01=coverage.a(0.01,50,m))
# set A n=200 # 
d<-data.frame(n=200,alpha_0.1=coverage.a(0.1,200,m), alpha_0.05=coverage.a(0.05,200,m),alpha_0.01=coverage.a(0.01,200,m))
setA=rbind(a,b,c,d);setA

## set B

con.int.b<-function(alpha,n,m){
  set.seed(45)
  est.var <- replicate(m,
                       {x <- rnorm(n, 0, 1)
                        tau<-sum(abs(x-median(x)))/n
                        delta<-(mean(x)-median(x))/tau
                        gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n})
  tau <- replicate(m,
                   {x <- rnorm(n, 0, 1)
                    sum(abs(x-median(x)))/n})
  coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y<-cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var))
  return(exp(Y))
}

## true_tau


coverage.b<-function(alpha,n,m){
  true_tau<-sqrt(1/1.57)
  ans<-con.int.b(alpha,n,m)
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#set B # n=10 # norm
m=5000
a<-data.frame(n=10,alpha_0.1=coverage.b(0.1,10,m), alpha_0.5=coverage.b(0.05,10,m),alpha_0.01=coverage.b(0.01,10,m))
#set B #n=25 # norm
b<-data.frame(n=25,alpha_0.1=coverage.b(0.1,25,m), alpha_0.5=coverage.b(0.05,25,m),alpha_0.01=coverage.b(0.01,25,m))
# set B # n=50 # norm
c<-data.frame(n=50,alpha_0.1=coverage.b(0.1,50,m), alpha_0.5=coverage.b(0.05,50,m),alpha_0.01=coverage.b(0.01,50,m))
# set B # n=200 # norm
d<-data.frame(n=200,alpha_0.1=coverage.b(0.1,200,m), alpha_0.5=coverage.b(0.05,200,m),alpha_0.01=coverage.b(0.01,200,m))

setB=rbind(a,b,c,d);setB
############ set C # t.distrbution

con.int.c<-function(alpha,n,m){
  set.seed(45)
  est.var <- replicate(m,
                       {x <- rt(n,5)
                        tau<-sum(abs(x-median(x)))/n
                        delta<-(mean(x)-median(x))/tau
                        gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n})
  tau <- replicate(m,
                   {x <- rt(n,5 )
                    sum(abs(x-median(x)))/n})
  coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y<-cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var))
  return(exp(Y))
}

## true_tau


coverage.c<-function(alpha,n,m){
  true_tau<-sqrt((5/3)/2)
  ans<-con.int.c(alpha,n,m)
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )

}

#set C # n=10 # t.dist
m=1000
a<-data.frame(n=10,alpha_0.1=coverage.c(0.1,10,m), alpha_0.5=coverage.c(0.05,10,m),alpha_0.01=coverage.c(0.01,10,m))
#set C #n=25 #  t.dist
b<-data.frame(n=25,alpha_0.1=coverage.c(0.1,25,m), alpha_0.5=coverage.c(0.05,25,m),alpha_0.01=coverage.c(0.01,25,m))
# set c # n=50 # t.dist
c<-data.frame(n=50,alpha_0.1=coverage.c(0.1,50,m), alpha_0.5=coverage.c(0.05,50,m),alpha_0.01=coverage.c(0.01,50,m))
# set c # n=200 # t.dist
d<-data.frame(n=200,alpha_0.1=coverage.c(0.1,200,m), alpha_0.5=coverage.c(0.05,200,m),alpha_0.01=coverage.c(0.01,200,m))
setC=rbind(a,b,c,d)

##############set D Halfnorm
con.int.d<-function(alpha,n,m){
  set.seed(45)
  est.var <- replicate(m,
                       {x <- rgamma(n,7,1)
                        tau<-sum(abs(x-median(x)))/n
                        delta<-(mean(x)-median(x))/tau
                       gamma<-var(x)/tau^2
                       (delta^2+gamma-1)/n})
  tau <- replicate(m,
                    {x <- rgamma(n,7,1)
                     sum(abs(x-median(x)))/n})
                     coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y<-cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var))
   return(exp(Y))
}

## true_tau


coverage.d<-function(alpha,n,m){
  true_tau<-sqrt(7/1.63)
  ans<-con.int.d(alpha,n,m)
  
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
  
}

#set D # n=10 
m=1000
a<-data.frame(n=10,alpha_0.1=coverage.d(0.1,10,m), alpha_0.05=coverage.d(0.05,10,m),alpha_0.01=coverage.d(0.01,10,m))
#set D #n=25 
b<-data.frame(n=25,alpha_0.1=coverage.d(0.1,25,m), alpha_0.05=coverage.d(0.05,25,m),alpha_0.01=coverage.d(0.01,25,m))
# set D # n=50  
c<-data.frame(n=50,alpha_0.1=coverage.d(0.1,50,m), alpha_0.05=coverage.d(0.05,50,m),alpha_0.01=coverage.d(0.01,50,m))
# set D # n=200 
d<-data.frame(n=200,alpha_0.1=coverage.d(0.1,200,m), alpha_0.05=coverage.d(0.05,200,m),alpha_0.01=coverage.d(0.01,200,m))
setD=rbind(a,b,c,d) 


##### set E # chi-square

con.int.e<-function(alpha,n,m){
  set.seed(45)
  est.var <- replicate(m,
                       {x <- rchisq(n,4)
                        tau<-sum(abs(x-median(x)))/n
                        delta<-(mean(x)-median(x))/tau
                        gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n})
  tau <- replicate(m,
                 {x <- rchisq(n,4)
                  sum(abs(x-median(x)))/n})
  coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y<-cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var))
  return(exp(Y))
}

## true_tau
coverage.e<-function(alpha,n,m){
  true_tau<-sqrt((2*4)/1.81)
  ans<-con.int.e(alpha,n,m)
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#set E # n=10 # rexp
m=1000
a<-data.frame(n=10, alpha_0.1=coverage.e(0.1,10,m), alpha_0.5=coverage.e(0.05,10,m),alpha_0.01=coverage.e(0.01,10,m))
#set E #n=25 #  rexp
b<-data.frame(n=25, alpha_0.1=coverage.e(0.1,25,m), alpha_0.5=coverage.e(0.05,25,m),alpha_0.01=coverage.e(0.01,25,m))
# set E # n=50 # rexp
c<-data.frame(n=50, alpha_0.1=coverage.e(0.1,50,m), alpha_0.5=coverage.e(0.05,50,m),alpha_0.01=coverage.e(0.01,50,m))
# set E # n=200 # rexp
d<-data.frame(n=200, alpha_0.1=coverage.e(0.1,200,m), alpha_0.5=coverage.e(0.05,200,m),alpha_0.01=coverage.e(0.01,200,m))
setE=rbind(a,b,c,d)

