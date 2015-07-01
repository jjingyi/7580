## 7580 project

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

m=50000
a<-data.frame(n=10,alpha_0.1=con.int.a(0.1,10,m), alpha_0.05=con.int.a(0.05,10,m),alpha_0.01=con.int.a(0.01,10,m))
a#set A #n=25 
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

m=50000
a<-data.frame(n=10,alpha_0.1=con.int.b(0.1,10,m), alpha_0.05=con.int.b(0.05,10,m),alpha_0.01=con.int.b(0.01,10,m))
a#set A #n=25 
b<-data.frame(n=25,alpha_0.1=con.int.b(0.1,25,m), alpha_0.05=con.int.b(0.05,25,m),alpha_0.01=con.int.b(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.b(0.1,50,m), alpha_0.05=con.int.b(0.05,50,m),alpha_0.01=con.int.b(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.b(0.1,200,m), alpha_0.05=con.int.b(0.05,200,m),alpha_0.01=con.int.b(0.01,200,m))
vrsetB=rbind(a,b,c,d);vrsetB
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

m=50000
a<-data.frame(n=10,alpha_0.1=con.int.c(0.1,10,m), alpha_0.05=con.int.c(0.05,10,m),alpha_0.01=con.int.c(0.01,10,m))
#set A #n=25 
b<-data.frame(n=25,alpha_0.1=con.int.c(0.1,25,m), alpha_0.05=con.int.c(0.05,25,m),alpha_0.01=con.int.c(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.c(0.1,50,m), alpha_0.05=con.int.c(0.05,50,m),alpha_0.01=con.int.c(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.c(0.1,200,m), alpha_0.05=con.int.c(0.05,200,m),alpha_0.01=con.int.c(0.01,200,m))
vrsetC=rbind(a,b,c,d);vrsetC

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

m=50000
a<-data.frame(n=10,alpha_0.1=con.int.d(0.1,10,m), alpha_0.05=con.int.d(0.05,10,m),alpha_0.01=con.int.d(0.01,10,m))
a#set A #n=25 
b<-data.frame(n=25,alpha_0.1=con.int.d(0.1,25,m), alpha_0.05=con.int.d(0.05,25,m),alpha_0.01=con.int.d(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.d(0.1,50,m), alpha_0.05=con.int.d(0.05,50,m),alpha_0.01=con.int.d(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.d(0.1,200,m), alpha_0.05=con.int.d(0.05,200,m),alpha_0.01=con.int.d(0.01,200,m))
vrsetD=rbind(a,b,c,d);vrsetD
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

m=50000
a<-data.frame(n=10,alpha_0.1=con.int.e(0.1,10,m), alpha_0.05=con.int.e(0.05,10,m),alpha_0.01=con.int.e(0.01,10,m))
#set A #n=25 
b<-data.frame(n=25,alpha_0.1=con.int.e(0.1,25,m), alpha_0.05=con.int.e(0.05,25,m),alpha_0.01=con.int.e(0.01,10,m))
# setA # n=50 
c<-data.frame(n=50, alpha_0.1=con.int.e(0.1,50,m), alpha_0.05=con.int.e(0.05,50,m),alpha_0.01=con.int.e(0.01,50,m))
# set A n=200 
d<-data.frame(n=200,alpha_0.1=con.int.e(0.1,200,m), alpha_0.05=con.int.e(0.05,200,m),alpha_0.01=con.int.e(0.01,200,m))
vrsetE=rbind(a,b,c,d);vrsetE

vrsetA;vrsetB;vrsetC;vrsetD;vrsetE
