Appendix
========================================================

One-group design:


```r
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

m=1000
a<-data.frame(n=10,alpha_0.1=coverage.a(0.1,10,m), alpha_0.05=coverage.a(0.05,10,m),alpha_0.01=coverage.a(0.01,10,m))
#set A #n=25 
b<-data.frame(n=25,alpha_0.1=coverage.a(0.1,25,m), alpha_0.05=coverage.a(0.05,25,m),alpha_0.01=coverage.a(0.01,25,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=coverage.a(0.1,50,m), alpha_0.05=coverage.a(0.05,50,m),alpha_0.01=coverage.a(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=coverage.a(0.1,200,m), alpha_0.05=coverage.a(0.05,200,m),alpha_0.01=coverage.a(0.01,200,m))
setA=rbind(a,b,c,d)

################ set B

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
m=1000
a<-data.frame(n=10,alpha_0.1=coverage.b(0.1,10,m), alpha_0.5=coverage.b(0.05,10,m),alpha_0.01=coverage.b(0.01,10,m))
#set B #n=25 # norm
b<-data.frame(n=25,alpha_0.1=coverage.b(0.1,25,m), alpha_0.5=coverage.b(0.05,25,m),alpha_0.01=coverage.b(0.01,25,m))
# set B # n=50 # norm
c<-data.frame(n=50,alpha_0.1=coverage.b(0.1,50,m), alpha_0.5=coverage.b(0.05,50,m),alpha_0.01=coverage.b(0.01,50,m))
# set B # n=200 # norm
d<-data.frame(n=200,alpha_0.1=coverage.b(0.1,200,m), alpha_0.5=coverage.b(0.05,200,m),alpha_0.01=coverage.b(0.01,200,m))

setB=rbind(a,b,c,d)

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

########### set D # rgamma

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

#set D # n=10 # rgamma
m=1000
a<-data.frame(n=10,alpha_0.1=coverage.d(0.1,10,m), alpha_0.05=coverage.d(0.05,10,m),alpha_0.01=coverage.d(0.01,10,m))
#set D #n=25 #  rgamma
b<-data.frame(n=25,alpha_0.1=coverage.d(0.1,25,m), alpha_0.05=coverage.d(0.05,25,m),alpha_0.01=coverage.d(0.01,25,m))
# set D # n=50 # rgamma
c<-data.frame(n=50,alpha_0.1=coverage.d(0.1,50,m), alpha_0.05=coverage.d(0.05,50,m),alpha_0.01=coverage.d(0.01,50,m))
# set D # n=200 # rgamma
d<-data.frame(n=200,alpha_0.1=coverage.d(0.1,200,m), alpha_0.05=coverage.d(0.05,200,m),alpha_0.01=coverage.d(0.01,200,m))
setD=rbind(a,b,c,d) 


## set E # chi-square(4)
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

#set E # n=10 
m=1000
a<-data.frame(n=10, alpha_0.1=coverage.e(0.1,10,m), alpha_0.5=coverage.e(0.05,10,m),alpha_0.01=coverage.e(0.01,10,m))
#set E #n=25 
b<-data.frame(n=25, alpha_0.1=coverage.e(0.1,25,m), alpha_0.5=coverage.e(0.05,25,m),alpha_0.01=coverage.e(0.01,25,m))
# set E # n=50 
c<-data.frame(n=50, alpha_0.1=coverage.e(0.1,50,m), alpha_0.5=coverage.e(0.05,50,m),alpha_0.01=coverage.e(0.01,50,m))
# set E # n=200 
d<-data.frame(n=200, alpha_0.1=coverage.e(0.1,200,m), alpha_0.5=coverage.e(0.05,200,m),alpha_0.01=coverage.e(0.01,200,m))
setE=rbind(a,b,c,d)
```

Two-group design:


```r
## uniform and norm
con.int.ab<-function(alpha,n1,n2,m){
  set.seed(1990)
  est.var1 <- replicate(m,
                       {x <- runif(n1)
                        tau<-sum(abs(x-median(x)))/n1
                        delta<-(mean(x)-median(x))/tau
                        gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n1})
  tau1 <- replicate(m,
                      {x <- runif(n1)
                       sum(abs(x-median(x)))/n1})

  est.var2 <- replicate(m,
                       {x <- rnorm(n2, 0, 1)
                        tau<-sum(abs(x-median(x)))/n2
                        delta<-(mean(x)-median(x))/tau
                        gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n2})
  tau2 <- replicate(m,
                    {x <- rnorm(n2, 0, 1)
                     sum(abs(x-median(x)))/n2})
  coe<-qnorm(1-alpha/2)
  c1<-n1/(n1-1)
  c2<-n2/(n2-1)
                     
  Y<-cbind(log((c1*tau1)/(c2*tau2))-coe*sqrt(est.var1+est.var2), log((c1*tau1)/(c2*tau2))+coe*sqrt(est.var1+est.var2))
  return(exp(Y))
}


## true_tau
coverage.ab<-function(alpha,n1,n2,m){
  true_tau1<-sqrt((1/12)/1.33)
  true_tau2<-sqrt(1/1.57)
  true_tau=true_tau1/true_tau2
  ans<-con.int.ab(alpha,n1,n2,m)
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#setA and B # n=10 

m=1000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.ab(0.1,10,10,m), alpha_0.5=coverage.ab(0.05,10,10,m),alpha_0.01=coverage.ab(0.01,10,10,m))
b<-data.frame(n1=25, n2=10, alpha_0.1=coverage.ab(0.1,25,10,m), alpha_0.5=coverage.ab(0.05,25,10,m),alpha_0.01=coverage.ab(0.01,25,10,m))
c<-data.frame(n1=10, n2=25, alpha_0.1=coverage.ab(0.1,10,25,m), alpha_0.5=coverage.ab(0.05,10,25,m),alpha_0.01=coverage.ab(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.ab(0.1,25,25,m), alpha_0.5=coverage.ab(0.05,25,25,m),alpha_0.01=coverage.ab(0.01,25,25,m))
e<-data.frame(n1=100,n2=100,alpha_0.1=coverage.ab(0.1,100,100,m), alpha_0.5=coverage.ab(0.05,100,100,m),alpha_0.01=coverage.ab(0.01,100,100,m))

setAB=rbind(a,b,c,d,e)


##########3 set B ans Set C (norm ,t-disttrbution)
con.int.bc<-function(alpha,n1,n2,m){
  set.seed(1990)
  est.var1 <- replicate(m,
                        {x <- rnorm(n1, 0, 1)
                         tau<-sum(abs(x-median(x)))/n1
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                         (delta^2+gamma-1)/n1})
  tau1 <- replicate(m,
                    {x <- rnorm(n1,0,1)
                     sum(abs(x-median(x)))/n1})
  est.var2 <- replicate(m,
                        {x <- rt(n2, 5)
                         tau<-sum(abs(x-median(x)))/n2
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                         (delta^2+gamma-1)/n2})
  tau2 <- replicate(m,
                    {x <- rt(n2, 5)
                     sum(abs(x-median(x)))/n2})
  coe<-qnorm(1-alpha/2)
  c1<-n1/(n1-1)
  c2<-n2/(n2-1)
  Y<-cbind(log((c1*tau1)/(c2*tau2))-coe*sqrt(est.var1+est.var2), log((c1*tau1)/(c2*tau2))+coe*sqrt(est.var1+est.var2))
  return(exp(Y))
}


## To see if CIS cover true_tau
coverage.bc<-function(alpha,n1,n2,m){
  true_tau1<-sqrt(1/1.57)
  true_tau2<-sqrt((5/3)/2)
  true_tau=true_tau1/true_tau2
  ans<-con.int.bc(alpha,n1,n2,m) 
  ## plug into the function to get confidence interval
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#setB and C # n=10 # rnorm rbeta
m=1000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.bc(0.1,10,10,m), alpha_0.5=coverage.bc(0.05,10,10,m),alpha_0.01=coverage.bc(0.01,10,10,m))
b<-data.frame(n1=25, n2=10,alpha_0.1=coverage.bc(0.1,25,10,m), alpha_0.5=coverage.bc(0.05,25,10,m),alpha_0.01=coverage.bc(0.01,25,10,m))
c<-data.frame(n1=10, n2=25,alpha_0.1=coverage.bc(0.1,10,25,m), alpha_0.5=coverage.bc(0.05,10,25,m),alpha_0.01=coverage.bc(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.bc(0.1,25,25,m), alpha_0.5=coverage.bc(0.05,25,25,m),alpha_0.01=coverage.bc(0.01,25,25,m))
e<-data.frame(n1=100, n2=100,alpha_0.1=coverage.bc(0.1,100,100,m), alpha_0.5=coverage.bc(0.05,100,100,m),alpha_0.01=coverage.bc(0.01,100,100,m))
setBC=rbind(a,b,c,d,e)


##########3 set B and Set D (N(0,1) ,Gamma(7,1))
con.int.bd<-function(alpha,n1,n2,m){
  set.seed(1990)
  est.var1 <- replicate(m,
                        {x <- rnorm(n1, 0, 1)
                         tau<-sum(abs(x-median(x)))/n1
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                         (delta^2+gamma-1)/n1})
  tau1 <- replicate(m,
                    {x <- rnorm(n1,0,1)
                     sum(abs(x-median(x)))/n1})
  est.var2 <- replicate(m,
                        {x <- rgamma(n2, 7,1)
                         tau<-sum(abs(x-median(x)))/n2
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n2})
  tau2 <- replicate(m,
                    {x <- rgamma(n2, 7,1)
                    sum(abs(x-median(x)))/n2})
                    coe<-qnorm(1-alpha/2)
c1<-n1/(n1-1)
c2<-n2/(n2-1)
Y<-cbind(log((c1*tau1)/(c2*tau2))-coe*sqrt(est.var1+est.var2), log((c1*tau1)/(c2*tau2))+coe*sqrt(est.var1+est.var2))
return(exp(Y))
}


## To see if CIS cover true_tau
coverage.bd<-function(alpha,n1,n2,m){
  true_tau1<-sqrt(1/1.57)
  true_tau2<-sqrt(7/1.63)
  true_tau=true_tau1/true_tau2
  ans<-con.int.bd(alpha,n1,n2,m) ## Confidence Interval
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#setB and D # n=10 # gamma and norm
m=1000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.bd(0.1,10,10,m), alpha_0.5=coverage.bd(0.05,10,10,m),alpha_0.01=coverage.bd(0.01,10,10,m))
b<-data.frame(n1=25, n2=10,alpha_0.1=coverage.bd(0.1,25,10,m), alpha_0.5=coverage.bd(0.05,25,10,m),alpha_0.01=coverage.bd(0.01,25,10,m))
c<-data.frame(n1=10, n2=25,alpha_0.1=coverage.bd(0.1,10,25,m), alpha_0.5=coverage.bd(0.05,10,25,m),alpha_0.01=coverage.bd(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.bd(0.1,25,25,m), alpha_0.5=coverage.bd(0.05,25,25,m),alpha_0.01=coverage.bd(0.01,25,25,m))
e<-data.frame(n1=100, n2=100,alpha_0.1=coverage.bd(0.1,100,100,m), alpha_0.5=coverage.bd(0.05,100,100,m),alpha_0.01=coverage.bd(0.01,100,100,m))
setBD=rbind(a,b,c,d,e)

##################SET D  Set E Gamma(7,1)& Chi-square(4)
con.int.de<-function(alpha,n1,n2,m){
  set.seed(1990)
  est.var1 <- replicate(m,
                        {x <- rgamma(n1, 7, 1)
                         tau<-sum(abs(x-median(x)))/n1
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n1})
  tau1 <- replicate(m,
                    {x <- rgamma(n1, 7, 1)
                     sum(abs(x-median(x)))/n1})
  est.var2 <- replicate(m,
                        {x <- rchisq(n2, df=4)
                         tau<-sum(abs(x-median(x)))/n2
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                         (delta^2+gamma-1)/n2})
  tau2 <- replicate(m,
                  {x <- rchisq(n2, df=4)
                   sum(abs(x-median(x)))/n2})
coe<-qnorm(1-alpha/2)
c1<-n1/(n1-1)
c2<-n2/(n2-1)
Y<-cbind(log((c1*tau1)/(c2*tau2))-coe*sqrt(est.var1+est.var2), log((c1*tau1)/(c2*tau2))+coe*sqrt(est.var1+est.var2))
return(exp(Y))
}


## To see if CIS cover true_tau
coverage.de<-function(alpha,n1,n2,m){
  true_tau1<-sqrt(7/1.63)
  true_tau2<-sqrt((2*4)/1.81)
  true_tau=true_tau1/true_tau2
  ans<-con.int.de(alpha,n1,n2,m) ## plug into the function to get confidence interval
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5))
}

m=50000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.de(0.1,10,10,m), alpha_0.05=coverage.de(0.05,10,10,m),alpha_0.01=coverage.de(0.01,10,10,m))
b<-data.frame(n1=25, n2=10,alpha_0.1=coverage.de(0.1,25,10,m), alpha_0.05=coverage.de(0.05,25,10,m),alpha_0.01=coverage.de(0.01,25,10,m))
c<-data.frame(n1=10, n2=25,alpha_0.1=coverage.de(0.1,10,25,m), alpha_0.05=coverage.de(0.05,10,25,m),alpha_0.01=coverage.de(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.de(0.1,25,25,m), alpha_0.05=coverage.de(0.05,25,25,m),alpha_0.01=coverage.de(0.01,25,25,m))
e<-data.frame(n1=100, n2=100,alpha_0.1=coverage.de(0.1,100,100,m), alpha_0.05=coverage.de(0.05,100,100,m),alpha_0.01=coverage.de(0.01,100,100,m))
setDE=rbind(a,b,c,d,e)                
```

Table2 :

```r
setA;setB;setC;setD;setE
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  94.00000   96.70000   99.30000
## Standard.Error   10   0.00751    0.00565    0.00264
## Coverage.Prob1   25  94.90000   96.80000   99.30000
## Standard.Error1  25   0.00696    0.00557    0.00264
## Coverage.Prob2   50  91.30000   95.50000   99.20000
## Standard.Error2  50   0.00891    0.00656    0.00282
## Coverage.Prob3  200  91.40000   95.50000   98.90000
## Standard.Error3 200   0.00887    0.00656    0.00330
```

```
##                   n alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  91.00000  96.10000   98.40000
## Standard.Error   10   0.00905   0.00612    0.00397
## Coverage.Prob1   25  90.60000  94.60000   98.80000
## Standard.Error1  25   0.00923   0.00715    0.00344
## Coverage.Prob2   50  91.40000  96.40000   99.30000
## Standard.Error2  50   0.00887   0.00589    0.00264
## Coverage.Prob3  200  89.40000  94.80000   98.90000
## Standard.Error3 200   0.00973   0.00702    0.00330
```

```
##                   n alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  88.20000  93.30000   97.50000
## Standard.Error   10   0.01020   0.00791    0.00494
## Coverage.Prob1   25  86.60000  92.70000   98.30000
## Standard.Error1  25   0.01077   0.00823    0.00409
## Coverage.Prob2   50  87.80000  93.40000   98.30000
## Standard.Error2  50   0.01035   0.00785    0.00409
## Coverage.Prob3  200  82.30000  89.40000   96.50000
## Standard.Error3 200   0.01207   0.00973    0.00581
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  90.60000   95.00000   98.70000
## Standard.Error   10   0.00923    0.00689    0.00358
## Coverage.Prob1   25  90.50000   94.80000   98.00000
## Standard.Error1  25   0.00927    0.00702    0.00443
## Coverage.Prob2   50  89.60000   94.60000   99.40000
## Standard.Error2  50   0.00965    0.00715    0.00244
## Coverage.Prob3  200  89.70000   94.50000   99.00000
## Standard.Error3 200   0.00961    0.00721    0.00315
```

```
##                   n alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  85.50000  91.90000   97.40000
## Standard.Error   10   0.01113   0.00863    0.00503
## Coverage.Prob1   25  87.90000  93.80000   98.40000
## Standard.Error1  25   0.01031   0.00763    0.00397
## Coverage.Prob2   50  87.50000  93.20000   98.70000
## Standard.Error2  50   0.01046   0.00796    0.00358
## Coverage.Prob3  200  89.80000  94.60000   99.20000
## Standard.Error3 200   0.00957   0.00715    0.00282
```

Table 3:

```r
setAB;setBC;setBD;setDE
```

```
##                  n1  n2 alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  10  93.10000  96.10000   98.90000
## Standard.Error   10  10   0.00801   0.00612    0.00330
## Coverage.Prob1   25  10  92.00000  95.70000   98.80000
## Standard.Error1  25  10   0.00858   0.00641    0.00344
## Coverage.Prob2   10  25  93.50000  96.20000   99.40000
## Standard.Error2  10  25   0.00780   0.00605    0.00244
## Coverage.Prob3   25  25  90.70000  94.60000   99.10000
## Standard.Error3  25  25   0.00918   0.00715    0.00299
## Coverage.Prob4  100 100  88.90000  94.60000   98.70000
## Standard.Error4 100 100   0.00993   0.00715    0.00358
```

```
##                  n1  n2 alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  10  90.90000  94.90000   98.50000
## Standard.Error   10  10   0.00909   0.00696    0.00384
## Coverage.Prob1   25  10  90.80000  94.80000   98.90000
## Standard.Error1  25  10   0.00914   0.00702    0.00330
## Coverage.Prob2   10  25  92.10000  95.00000   98.90000
## Standard.Error2  10  25   0.00853   0.00689    0.00330
## Coverage.Prob3   25  25  88.80000  94.90000   98.30000
## Standard.Error3  25  25   0.00997   0.00696    0.00409
## Coverage.Prob4  100 100  89.10000  94.30000   98.50000
## Standard.Error4 100 100   0.00985   0.00733    0.00384
```

```
##                  n1  n2 alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  10  92.20000  96.50000   98.90000
## Standard.Error   10  10   0.00848   0.00581    0.00330
## Coverage.Prob1   25  10  90.90000  95.60000   99.20000
## Standard.Error1  25  10   0.00909   0.00649    0.00282
## Coverage.Prob2   10  25  92.60000  96.40000   98.50000
## Standard.Error2  10  25   0.00828   0.00589    0.00384
## Coverage.Prob3   25  25  90.80000  95.60000   99.50000
## Standard.Error3  25  25   0.00914   0.00649    0.00223
## Coverage.Prob4  100 100  89.60000  94.80000   99.40000
## Standard.Error4 100 100   0.00965   0.00702    0.00244
```

```
##                  n1  n2 alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  10  89.38000   94.23800   98.41200
## Standard.Error   10  10   0.00138    0.00104    0.00056
## Coverage.Prob1   25  10  88.29600   93.30400   97.94200
## Standard.Error1  25  10   0.00144    0.00112    0.00063
## Coverage.Prob2   10  25  90.28600   94.68800   98.59200
## Standard.Error2  10  25   0.00132    0.00100    0.00053
## Coverage.Prob3   25  25  89.59200   94.59000   98.70200
## Standard.Error3  25  25   0.00137    0.00101    0.00051
## Coverage.Prob4  100 100  89.89800   94.69000   98.85800
## Standard.Error4 100 100   0.00135    0.00100    0.00048
```

Variance reduction: 


```r
## Set A
## uniform
# 1 group
####Vaiance reduction
con.int.a<-function(alpha,n,m){
  true_tau<-sqrt((1/12)/1.33)
  Y1<-matrix(rep(0,m), nrow=m/2); Y2<-matrix(rep(0,m), nrow=m/2)
  for (j in 1:(m/2)){
        u1<-runif(n)
        u2<-1-u1
        x1 <- qunif(u1, 0, 1)
        x2<-qunif(u2,0,1)
        tau1<-sum(abs(x1-median(x1)))/n 
        tau2<-sum(abs(x2-median(x2)))/n
        delta1<-(mean(x1)-median(x1))/tau1
        delta2<-(mean(x2)-median(x2))/tau2
        gamma1<-var(x1)/tau1^2
        gamma2<-var(x2)/tau2^2
        est.var1<-(delta1^2+gamma1-1)/n
        est.var2<-(delta2^2+gamma2-1)/n

  coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y1[j,]<-exp(cbind(log(c*tau1)-coe*sqrt(est.var1), log(c*tau1)+coe*sqrt(est.var1)))
  Y2[j,]<-exp(cbind(log(c*tau2)-coe*sqrt(est.var2), log(c*tau2)+coe*sqrt(est.var2)))
  
}
  L1<-(Y1[,1]<true_tau)*1
  L2<-(Y2[,1]<true_tau)*1
  U1<-(Y1[,2]>true_tau)*1
  U2<-(Y2[,2]>true_tau)*1
  count1<-L1*U1
  count2<-L2*U2
  count=(count1+count2)/2
  p <- sum(count)/(m/2)
  se <-sd(count)/sqrt(m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

m=1000
a<-data.frame(n=10,alpha_0.1=con.int.a(0.1,10,m), alpha_0.05=con.int.a(0.05,10,m),alpha_0.01=con.int.a(0.01,10,m))
a#set A #n=25 
```

```
##                 n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob  10  93.40000   97.80000   99.00000
## Standard.Error 10   0.00786    0.00464    0.00315
```

```r
b<-data.frame(n=25,alpha_0.1=con.int.a(0.1,25,m), alpha_0.05=con.int.a(0.05,25,m),alpha_0.01=con.int.a(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.a(0.1,50,m), alpha_0.05=con.int.a(0.05,50,m),alpha_0.01=con.int.a(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.a(0.1,200,m), alpha_0.05=con.int.a(0.05,200,m),alpha_0.01=con.int.a(0.01,200,m))
vrsetA=rbind(a,b,c,d)
################set B
con.int.b<-function(alpha,n,m){
  true_tau<-sqrt(1/1.57)
  Y1<-matrix(rep(0,m), nrow=m/2); Y2<-matrix(rep(0,m), nrow=m/2)
  for (j in 1:(m/2)){
    u1<-runif(n)
    u2<-1-u1
    x1 <- qnorm(u1, 0, 1)
    x2<-qnorm(u2,0,1)
    tau1<-sum(abs(x1-median(x1)))/n 
    tau2<-sum(abs(x2-median(x2)))/n
    delta1<-(mean(x1)-median(x1))/tau1
    delta2<-(mean(x2)-median(x2))/tau2
    gamma1<-var(x1)/tau1^2
    gamma2<-var(x2)/tau2^2
    est.var1<-(delta1^2+gamma1-1)/n
    est.var2<-(delta2^2+gamma2-1)/n
    
    coe<-qnorm(1-alpha/2)
    c<-n/(n-1)
    Y1[j,]<-exp(cbind(log(c*tau1)-coe*sqrt(est.var1), log(c*tau1)+coe*sqrt(est.var1)))
    Y2[j,]<-exp(cbind(log(c*tau2)-coe*sqrt(est.var2), log(c*tau2)+coe*sqrt(est.var2)))
    
  }
  L1<-(Y1[,1]<true_tau)*1
  L2<-(Y2[,1]<true_tau)*1
  U1<-(Y1[,2]>true_tau)*1
  U2<-(Y2[,2]>true_tau)*1
  count1<-L1*U1
  count2<-L2*U2
  count=(count1+count2)/2
  p <- sum(count)/(m/2)
  se <-sd(count)/sqrt(m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

m=1000
a<-data.frame(n=10,alpha_0.1=con.int.b(0.1,10,m), alpha_0.05=con.int.b(0.05,10,m),alpha_0.01=con.int.b(0.01,10,m))
a#set A #n=25 
```

```
##                 n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob  10  91.40000   94.80000   98.20000
## Standard.Error 10   0.00887    0.00703    0.00421
```

```r
b<-data.frame(n=25,alpha_0.1=con.int.b(0.1,25,m), alpha_0.05=con.int.b(0.05,25,m),alpha_0.01=con.int.b(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.b(0.1,50,m), alpha_0.05=con.int.b(0.05,50,m),alpha_0.01=con.int.b(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.b(0.1,200,m), alpha_0.05=con.int.b(0.05,200,m),alpha_0.01=con.int.b(0.01,200,m))
vrsetB=rbind(a,b,c,d)
############ set C # t.distrbution

con.int.c<-function(alpha,n,m){
  true_tau<-sqrt((5/3)/2)
  Y1<-matrix(rep(0,m), nrow=m/2); Y2<-matrix(rep(0,m), nrow=m/2)
  for (j in 1:(m/2)){
    u1<-runif(n)
    u2<-1-u1
    x1 <- qt(u1, 5)
    x2<-qt(u2, 5)
    tau1<-sum(abs(x1-median(x1)))/n 
    tau2<-sum(abs(x2-median(x2)))/n
    delta1<-(mean(x1)-median(x1))/tau1
    delta2<-(mean(x2)-median(x2))/tau2
    gamma1<-var(x1)/tau1^2
    gamma2<-var(x2)/tau2^2
    est.var1<-(delta1^2+gamma1-1)/n
    est.var2<-(delta2^2+gamma2-1)/n
    
    coe<-qnorm(1-alpha/2)
    c<-n/(n-1)
    Y1[j,]<-exp(cbind(log(c*tau1)-coe*sqrt(est.var1), log(c*tau1)+coe*sqrt(est.var1)))
    Y2[j,]<-exp(cbind(log(c*tau2)-coe*sqrt(est.var2), log(c*tau2)+coe*sqrt(est.var2)))
    
  }
  L1<-(Y1[,1]<true_tau)*1
  L2<-(Y2[,1]<true_tau)*1
  U1<-(Y1[,2]>true_tau)*1
  U2<-(Y2[,2]>true_tau)*1
  count1<-L1*U1
  count2<-L2*U2
  count=(count1+count2)/2
  p <- sum(count)/(m/2)
  se <-sd(count)/sqrt(m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

m=1000
a<-data.frame(n=10,alpha_0.1=con.int.c(0.1,10,m), alpha_0.05=con.int.c(0.05,10,m),alpha_0.01=con.int.c(0.01,10,m))
#set A #n=25 
b<-data.frame(n=25,alpha_0.1=con.int.c(0.1,25,m), alpha_0.05=con.int.c(0.05,25,m),alpha_0.01=con.int.c(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.c(0.1,50,m), alpha_0.05=con.int.c(0.05,50,m),alpha_0.01=con.int.c(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.c(0.1,200,m), alpha_0.05=con.int.c(0.05,200,m),alpha_0.01=con.int.c(0.01,200,m))
vrsetC=rbind(a,b,c,d)

########### set D # rgamma
con.int.d<-function(alpha,n,m){
  true_tau<-sqrt(7/1.63)
  Y1<-matrix(rep(0,m), nrow=m/2); Y2<-matrix(rep(0,m), nrow=m/2)
  for (j in 1:(m/2)){
    u1<-runif(n)
    u2<-1-u1
    x1 <- qgamma(u1, 7, 1)
    x2<-qgamma(u2,7,1)
    tau1<-sum(abs(x1-median(x1)))/n 
    tau2<-sum(abs(x2-median(x2)))/n
    delta1<-(mean(x1)-median(x1))/tau1
    delta2<-(mean(x2)-median(x2))/tau2
    gamma1<-var(x1)/tau1^2
    gamma2<-var(x2)/tau2^2
    est.var1<-(delta1^2+gamma1-1)/n
    est.var2<-(delta2^2+gamma2-1)/n
    
    coe<-qnorm(1-alpha/2)
    c<-n/(n-1)
    Y1[j,]<-exp(cbind(log(c*tau1)-coe*sqrt(est.var1), log(c*tau1)+coe*sqrt(est.var1)))
    Y2[j,]<-exp(cbind(log(c*tau2)-coe*sqrt(est.var2), log(c*tau2)+coe*sqrt(est.var2)))
    
  }
  L1<-(Y1[,1]<true_tau)*1
  L2<-(Y2[,1]<true_tau)*1
  U1<-(Y1[,2]>true_tau)*1
  U2<-(Y2[,2]>true_tau)*1
  count1<-L1*U1
  count2<-L2*U2
  count=(count1+count2)/2
  p <- sum(count)/(m/2)
  se <-sd(count)/sqrt(m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

m=1000
a<-data.frame(n=10,alpha_0.1=con.int.d(0.1,10,m), alpha_0.05=con.int.d(0.05,10,m),alpha_0.01=con.int.d(0.01,10,m))
a#set A #n=25 
```

```
##                 n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob  10  91.30000   94.10000   98.40000
## Standard.Error 10   0.00775    0.00633    0.00343
```

```r
b<-data.frame(n=25,alpha_0.1=con.int.d(0.1,25,m), alpha_0.05=con.int.d(0.05,25,m),alpha_0.01=con.int.d(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.d(0.1,50,m), alpha_0.05=con.int.d(0.05,50,m),alpha_0.01=con.int.d(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.d(0.1,200,m), alpha_0.05=con.int.d(0.05,200,m),alpha_0.01=con.int.d(0.01,200,m))
vrsetD=rbind(a,b,c,d)
########## set E # Chi-square

con.int.e<-function(alpha,n,m){
  true_tau<-sqrt((2*4)/1.81)
  Y1<-matrix(rep(0,m), nrow=m/2); Y2<-matrix(rep(0,m), nrow=m/2)
  for (j in 1:(m/2)){
    u1<-runif(n)
    u2<-1-u1
    x1 <- qchisq(u1, 4)
    x2<-qchisq(u2, 4)
    tau1<-sum(abs(x1-median(x1)))/n 
    tau2<-sum(abs(x2-median(x2)))/n
    delta1<-(mean(x1)-median(x1))/tau1
    delta2<-(mean(x2)-median(x2))/tau2
    gamma1<-var(x1)/tau1^2
    gamma2<-var(x2)/tau2^2
    est.var1<-(delta1^2+gamma1-1)/n
    est.var2<-(delta2^2+gamma2-1)/n
    
    coe<-qnorm(1-alpha/2)
    c<-n/(n-1)
    Y1[j,]<-exp(cbind(log(c*tau1)-coe*sqrt(est.var1), log(c*tau1)+coe*sqrt(est.var1)))
    Y2[j,]<-exp(cbind(log(c*tau2)-coe*sqrt(est.var2), log(c*tau2)+coe*sqrt(est.var2)))
    
  }
  L1<-(Y1[,1]<true_tau)*1
  L2<-(Y2[,1]<true_tau)*1
  U1<-(Y1[,2]>true_tau)*1
  U2<-(Y2[,2]>true_tau)*1
  count1<-L1*U1
  count2<-L2*U2
  count=(count1+count2)/2
  p <- sum(count)/(m/2)
  se <-sd(count)/sqrt(m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

m=1000
a<-data.frame(n=10,alpha_0.1=con.int.e(0.1,10,m), alpha_0.05=con.int.e(0.05,10,m),alpha_0.01=con.int.e(0.01,10,m))
#set A #n=25 
b<-data.frame(n=25,alpha_0.1=con.int.e(0.1,25,m), alpha_0.05=con.int.e(0.05,25,m),alpha_0.01=con.int.e(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.e(0.1,50,m), alpha_0.05=con.int.e(0.05,50,m),alpha_0.01=con.int.e(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.e(0.1,200,m), alpha_0.05=con.int.e(0.05,200,m),alpha_0.01=con.int.e(0.01,200,m))
vrsetE=rbind(a,b,c,d)

vrsetA;vrsetB;vrsetC;vrsetD;vrsetE
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  93.40000   97.80000   99.00000
## Standard.Error   10   0.00786    0.00464    0.00315
## Coverage.Prob1   25  94.40000   98.80000   99.60000
## Standard.Error1  25   0.00728    0.00345    0.00200
## Coverage.Prob2   50  92.20000   96.60000   99.80000
## Standard.Error2  50   0.00849    0.00574    0.00141
## Coverage.Prob3  200  92.20000   96.20000   99.20000
## Standard.Error3 200   0.00849    0.00605    0.00282
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  91.40000   94.80000   98.20000
## Standard.Error   10   0.00887    0.00703    0.00421
## Coverage.Prob1   25  91.60000   95.80000   99.00000
## Standard.Error1  25   0.00878    0.00635    0.00315
## Coverage.Prob2   50  87.80000   94.20000   99.00000
## Standard.Error2  50   0.01036    0.00740    0.00315
## Coverage.Prob3  200  91.20000   96.20000   98.80000
## Standard.Error3 200   0.00897    0.00605    0.00345
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  88.60000   94.60000   97.80000
## Standard.Error   10   0.01006    0.00715    0.00464
## Coverage.Prob1   25  91.20000   94.20000   98.20000
## Standard.Error1  25   0.00897    0.00740    0.00421
## Coverage.Prob2   50  89.40000   92.60000   99.00000
## Standard.Error2  50   0.00974    0.00829    0.00315
## Coverage.Prob3  200  85.60000   92.80000   97.80000
## Standard.Error3 200   0.01111    0.00818    0.00464
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  91.30000   94.10000   98.40000
## Standard.Error   10   0.00775    0.00633    0.00343
## Coverage.Prob1   25  86.70000   95.00000   97.80000
## Standard.Error1  25   0.00889    0.00562    0.00407
## Coverage.Prob2   50  90.40000   92.30000   99.10000
## Standard.Error2  50   0.00767    0.00690    0.00233
## Coverage.Prob3  200  90.90000   94.30000   98.70000
## Standard.Error3 200   0.00757    0.00577    0.00289
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  87.40000   92.00000   97.90000
## Standard.Error   10   0.00808    0.00614    0.00362
## Coverage.Prob1   25  87.90000   92.70000   97.20000
## Standard.Error1  25   0.00768    0.00602    0.00390
## Coverage.Prob2   50  88.00000   93.20000   98.20000
## Standard.Error2  50   0.00766    0.00595    0.00327
## Coverage.Prob3  200  89.10000   93.80000   98.70000
## Standard.Error3 200   0.00740    0.00559    0.00271
```

Importance Sampling for Chi-sqaure(4)

```r
vrunif<-function(alpha,n){
  true_tau<-sqrt((2*4)/1.81)
  ## proposed g(s)
  x<-rchisq(n,3.3)
  tau<-sum(abs(x-median(x)))/n
  delta<-(mean(x)-median(x))/tau
  gamma<-var(x)/tau^2

  est.var<-(delta^2+gamma-1)/n
  w<-exp(sum(dchisq(x,4,log=TRUE))-sum(dchisq(x,3.3,log=TRUE)))
   coe<-qnorm(1-alpha/2)
   c<-n/(n-1)
   Y<-exp(cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var)))

U<-1-(true_tau>Y[,2])*w
L<-1-(true_tau<Y[,1])*w
accept<-U*L
return(accept)
}
ant<-function(m, alpha,n){
  ans<-replicate(m, vrunif(alpha,n))
  p<-round(sum(ans)/m*100, digits=3)
  se<-round(sd(ans)/sqrt(m),digits=5)
  rbind(Prob=p,stanard.error=se)
}
cbind(ant(1000,0.1,10),ant(1000,0.05,10),ant(1000,0.01,10))
```

```
##                   [,1]     [,2]     [,3]
## Prob          85.82700 92.11000 96.46500
## stanard.error  0.01759  0.01232  0.00892
```

Control Variate for chi-square


```r
cvunif<-function(alpha,n){
  true_tau<-sqrt((2*4)/1.81)
  x<-rchisq(n,4)
  ss<-var(x)
  u<-mean(x)
  tau<-sum(abs(x-median(x)))/n
  delta<-(mean(x)-median(x))/tau
  gamma<-var(x)/tau^2
  est.var<-(delta^2+gamma-1)/n
  coe<-qnorm(1-alpha/2)
  c<-n/(n-1)
  Y<-exp(cbind(log(c*tau)-coe*sqrt(est.var), log(c*tau)+coe*sqrt(est.var)))
  
  U<-(true_tau<=Y[,2])
  L<-(true_tau>=Y[,1])
  accept<-U*L
  result<-c(accept,ss)
  return(result)
}

cv<-function(m, alpha,n){
  answ<-replicate(m, cvunif(alpha,n))
  a<-answ[1,]
  u<-answ[2,]
  model<-lm(a~u)
  beta1<--model$coefficients[2]
  ans<-a+beta1*(u-8)
  p<-round(sum(ans)/m*100, digits=3)
  se<-round(sd(ans)/sqrt(m),digits=5)
  rbind(p,se,summary(model)$r.squared)
}
cbind(cv(1000,0.1,10),cv(1000,0.05,10),cv(1000,0.01,10))
```

```
##         [,1]      [,2]      [,3]
## p  86.645000 93.441000 9.689e+01
## se  0.010760  0.007850 5.480e-03
##     0.004112  0.001757 2.414e-05
```
