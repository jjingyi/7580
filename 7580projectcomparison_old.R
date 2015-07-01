# 7580 project problem 2


con.int.ab<-function(alpha,n1,n2,m){
  set.seed(1990)
  est.var1 <- replicate(m,
                       {x <- rbeta(n1, 2, 2)
                        tau<-sum(abs(x-median(x)))/n1
                        delta<-(mean(x)-median(x))/tau
                        gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n1})
  tau1 <- replicate(m,
                 {x <- rbeta(n1, 2, 2)
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
  va1r<-1/7^2
  g1<-1.63
  true_tau1<-sqrt(var1/g1)
  
  true_tau=true_tau1/true_tau2
  ans<-con.int.ab(alpha,n1,n2,m)
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#setA and B # n=10 # beta and rnorm

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
  x<-rnorm(100000,0,1)
  true_tau1<-sum(abs(x-median(x)))/100000;true_tau1
  
  y<-rt(100000,5)
  true_tau2<-sum(abs(y-median(y)))/100000;true_tau2
  true_tau=true_tau1/true_tau2
  ans<-con.int.bc(alpha,n1,n2,m) ## plug into the function to get confidence interval
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#setB and C # n=10 # rnorm rbeta
m=50000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.bc(0.1,10,10,m), alpha_0.5=coverage.bc(0.05,10,10,m),alpha_0.01=coverage.bc(0.01,10,10,m))
b<-data.frame(n1=25, n2=10,alpha_0.1=coverage.bc(0.1,25,10,m), alpha_0.5=coverage.bc(0.05,25,10,m),alpha_0.01=coverage.bc(0.01,25,10,m))
c<-data.frame(n1=10, n2=25,alpha_0.1=coverage.bc(0.1,10,25,m), alpha_0.5=coverage.bc(0.05,10,25,m),alpha_0.01=coverage.bc(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.bc(0.1,25,25,m), alpha_0.5=coverage.bc(0.05,25,25,m),alpha_0.01=coverage.bc(0.01,25,25,m))
e<-data.frame(n1=100, n2=100,alpha_0.1=coverage.bc(0.1,100,100,m), alpha_0.5=coverage.bc(0.05,100,100,m),alpha_0.01=coverage.bc(0.01,100,100,m))
setBC=rbind(a,b,c,d,e)


##########3 set B ans Set D (norm ,t-disttrbution)
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
                        {x <- rgamma(n2, 1,7)
                         tau<-sum(abs(x-median(x)))/n2
                         delta<-(mean(x)-median(x))/tau
                         gamma<-var(x)/tau^2
                        (delta^2+gamma-1)/n2})
  tau2 <- replicate(m,
                    {x <- rgamma(n2, 1,7)
                    sum(abs(x-median(x)))/n2})
                    coe<-qnorm(1-alpha/2)
c1<-n1/(n1-1)
c2<-n2/(n2-1)
Y<-cbind(log((c1*tau1)/(c2*tau2))-coe*sqrt(est.var1+est.var2), log((c1*tau1)/(c2*tau2))+coe*sqrt(est.var1+est.var2))
return(exp(Y))
}


## To see if CIS cover true_tau
coverage.bc<-function(alpha,n1,n2,m){
  x<-rnorm(100000,0,1)
  true_tau1<-sum(abs(x-median(x)))/100000;true_tau1
  
  y<-rgamma(100000,1,7)
  true_tau2<-sum(abs(y-median(y)))/100000;true_tau2
  true_tau=true_tau1/true_tau2
  ans<-con.int.bc(alpha,n1,n2,m) ## plug into the function to get confidence interval
  L<-(ans[,1]<=true_tau)*1
  U<-(ans[,2]>=true_tau)*1
  count=L*U
  p<-(sum(count)/m)
  se<-sqrt(p*(1-p)/m)
  rbind(Coverage.Prob=p*100, Standard.Error=round(se, digits=5) )
}

#setB and D # n=10 # rnorm rbeta
m=50000
a<-data.frame(n1=10, n2=10,alpha_0.1=coverage.bc(0.1,10,10,m), alpha_0.5=coverage.bc(0.05,10,10,m),alpha_0.01=coverage.bc(0.01,10,10,m))
b<-data.frame(n1=25, n2=10,alpha_0.1=coverage.bc(0.1,25,10,m), alpha_0.5=coverage.bc(0.05,25,10,m),alpha_0.01=coverage.bc(0.01,25,10,m))
c<-data.frame(n1=10, n2=25,alpha_0.1=coverage.bc(0.1,10,25,m), alpha_0.5=coverage.bc(0.05,10,25,m),alpha_0.01=coverage.bc(0.01,10,25,m))
d<-data.frame(n1=25, n2=25,alpha_0.1=coverage.bc(0.1,25,25,m), alpha_0.5=coverage.bc(0.05,25,25,m),alpha_0.01=coverage.bc(0.01,25,25,m))
e<-data.frame(n1=100, n2=100,alpha_0.1=coverage.bc(0.1,100,100,m), alpha_0.5=coverage.bc(0.05,100,100,m),alpha_0.01=coverage.bc(0.01,100,100,m))
setBD=rbind(a,b,c,d,e)


