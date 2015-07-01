project Status Report- Confidence Intervals for Mean Absolute Deviations. 

Instructor: Dr. Albert

Author:Douglas G. Bonett 

Student: Jing-Yi Wu

Date: 28 April 2015   

===============================================================================

My paper is to test Converge probability of Confident interval for the mean absolute deviation from the median. Approximate confidence intervals for mean absolute deviations in one-group and two-group designs are derived and are shown to have excellent small-sample properties under moderate non-normality.


I chose one distriution from one set. 
  
    1.Unifrom -> Set A
    2.Normal -> set B 
    3.Student's t(5) -> set C
    4.Gamma(1, 7) -> set D 
    5.Chi-suare(4) -> set E

The author using 50,000 MC sample size. I used 1000 to test because it takes so much to time to run 50,000 times.

True $\tau$ is calculated by the formula $\gamma=\sigma^2/\tau^2$, and the $\gamma$ is provided by the author.(Table.1 on the Article)



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
m=50000
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
m=50000
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



#### From 2 samples 

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
                         delta<-0.158
                         gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n1})
  tau1 <- replicate(m,
                    {x <- rgamma(n2, 7, 1)
                     sum(abs(x-median(x)))/n1})
  est.var2 <- replicate(m,
                        {x <- rchisq(n2, df=4)
                         tau<-sum(abs(x-median(x)))/n2
                         delta<-0.305
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

m=1000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.de(0.1,10,10,m), alpha_0.05=coverage.de(0.05,10,10,m),alpha_0.01=coverage.de(0.01,10,10,m))
b<-data.frame(n1=25, n2=10,alpha_0.1=coverage.de(0.1,25,10,m), alpha_0.05=coverage.de(0.05,25,10,m),alpha_0.01=coverage.de(0.01,25,10,m))
c<-data.frame(n1=10, n2=25,alpha_0.1=coverage.de(0.1,10,25,m), alpha_0.05=coverage.de(0.05,10,25,m),alpha_0.01=coverage.de(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.de(0.1,25,25,m), alpha_0.05=coverage.de(0.05,25,25,m),alpha_0.01=coverage.de(0.01,25,25,m))
e<-data.frame(n1=100, n2=100,alpha_0.1=coverage.de(0.1,100,100,m), alpha_0.05=coverage.de(0.05,100,100,m),alpha_0.01=coverage.de(0.01,100,100,m))
setDE=rbind(a,b,c,d,e)                
```
(I)One-group:

The result for Set A--U(0,1):

```r
setA
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

The result for Set B-N(0, 1):

```r
setB
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


The result for Set C--t(5):

```r
setC
```

```
##                   n alpha_0.1 alpha_0.5 alpha_0.01
## Coverage.Prob    10  87.92000  93.10000   97.75800
## Standard.Error   10   0.00146   0.00113    0.00066
## Coverage.Prob1   25  88.22000  93.49800   98.19600
## Standard.Error1  25   0.00144   0.00110    0.00060
## Coverage.Prob2   50  87.32800  92.84800   98.10200
## Standard.Error2  50   0.00149   0.00115    0.00061
## Coverage.Prob3  200  83.94600  90.62000   97.28000
## Standard.Error3 200   0.00164   0.00130    0.00073
```


The result for Set D--Gamma(7,1):

```r
setD
```

```
##                   n alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  89.92400   94.29000   98.16400
## Standard.Error   10   0.00135    0.00104    0.00060
## Coverage.Prob1   25  90.00600   94.62200   98.59400
## Standard.Error1  25   0.00134    0.00101    0.00053
## Coverage.Prob2   50  89.52000   94.45200   98.68400
## Standard.Error2  50   0.00137    0.00102    0.00051
## Coverage.Prob3  200  89.85200   94.87200   98.90200
## Standard.Error3 200   0.00135    0.00099    0.00047
```


The result for Set E--Chi-square(4):

```r
setE
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

I only set m=1000, so the results are smaller than the results of the author. 
Fro gamma distribution, it should be Gamma($\alpha$=7,$\beta$=1), At the beginning, I used Gamma($\alpha$=1,$\beta$=7) to simulate. It did not gengerate 
reasonable answers. When I checked the $\gamma$ for Gamma($\alpha$=1,$\beta$=7), it's not 1.63, which is provided by the table.1 on the article. Gamma($\alpha$=7,$\beta$=1) generated 1.63 for $\gamma$. I think the author should have marked $\alpha$ and $\beta$ on the paper.

(II)Two-group:

The result for Set A and set B ( N(0,1) & U(0,1)):

```r
setAB
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


The result for Set B and set C( N(0, 1)& t(5)):

```r
setBC
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

The result for Set B and set D ( N(0,1) & Gamma(7, 1)):

```r
setBD
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

The result for Set D and set E (Gamma(7,1) & Chi-square(4)):

```r
setDE
```

```
##                  n1  n2 alpha_0.1 alpha_0.05 alpha_0.01
## Coverage.Prob    10  10  90.90000   94.90000   98.30000
## Standard.Error   10  10   0.00909    0.00696    0.00409
## Coverage.Prob1   25  10  18.10000   25.90000   41.20000
## Standard.Error1  25  10   0.01218    0.01385    0.01556
## Coverage.Prob2   10  25   7.30000   15.30000   35.80000
## Standard.Error2  10  25   0.00823    0.01138    0.01516
## Coverage.Prob3   25  25  90.30000   94.90000   98.30000
## Standard.Error3  25  25   0.00936    0.00696    0.00409
## Coverage.Prob4  100 100  89.80000   94.90000   98.90000
## Standard.Error4 100 100   0.00957    0.00696    0.00330
```


The set for D&E is a weird. I don't know why the output is like this when n1=10, n2=25 or n1=25 n2=10. The coverage probability became so small. 

I have not done variance reduction. So far, I only replicated the author's result. For set D and E, the result is problematic.





