## variance reduction for two-group
con.int.ab<-function(alpha,n1,n2, m){
  true_tau1<-sqrt((1/12)/1.33)
  true_tau2<-sqrt(1/1.57)
  true_tau<-true_tau1/true_tau2
  Y1<-matrix(rep(0,m), nrow=m/2); Y2<-matrix(rep(0,m), nrow=m/2)
  for (j in 1:(m/2)){
    u1.x<-runif(n1)
    u2.x<-1-u1.x
    x1 <- qunif(u1.x, 0, 1)
    x2<-qunif(u2.x,0,1)
    tau1.x<-sum(abs(x1-median(x1)))/n1 
    tau2.x<-sum(abs(x2-median(x2)))/n1
    delta1.x<-(mean(x1)-median(x1))/tau1.x
    delta2.x<-(mean(x2)-median(x2))/tau2.x
    gamma1.x<-var(x1)/tau1.x^2
    gamma2.x<-var(x2)/tau2.x^2
    est.var1.x<-(delta1.x^2+gamma1.x-1)/n1
    est.var2.x<-(delta2.x^2+gamma2.x-1)/n1
    
    u1.y<-runif(n2)
    u2.y<-1-u1.y
    y1 <- qnorm(u1.y, 0, 1)
    y2<-qnorm(u2.y,0,1)
    tau1.y<-sum(abs(y1-median(y1)))/n2 
    tau2.y<-sum(abs(y2-median(y2)))/n2
    delta1.y<-(mean(y1)-median(y1))/tau1.y
    delta2.y<-(mean(y2)-median(y2))/tau2.y
    gamma1.y<-var(y1)/tau1.y^2
    gamma2.y<-var(y2)/tau2.y^2
    est.var1.y<-(delta1.y^2+gamma1.y-1)/n2
    est.var2.y<-(delta2.y^2+gamma2.y-1)/n2
    
   
    c.x<-n1/(n1-1)
    c.y<-n2/(n2-1)
    coe<-qnorm(1-alpha/2)
    Y1[j,]<-exp(cbind(log( (c.x*tau1.x)/(c.y*tau1.y))-coe*sqrt(est.var1.x+est.var1.y) , log( (c.x*tau1.x)/(c.y*tau1.y))+coe*sqrt(est.var1.x+est.var1.y)))
    Y2[j,]<-exp(cbind(log( (c.x*tau2.x)/(c.y*tau2.y))-coe*sqrt(est.var2.x+est.var2.y) , log( (c.x*tau2.x)/(c.y*tau2.y))+coe*sqrt(est.var2.x+est.var2.y)))
    
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
a<-data.frame(n1=10,n2=10,alpha_0.1=con.int.ab(0.1,10,10,m), alpha_0.05=con.int.ab(0.05,10,10,m),alpha_0.01=con.int.ab(0.01,10,10,m)) 
b<-data.frame(n1=25,n2=10,alpha_0.1=con.int.ab(0.1,25,10,m), alpha_0.05=con.int.ab(0.05,25,10,m),alpha_0.01=con.int.ab(0.01,25,10,m))
c<-data.frame(n1=10,n2=25, alpha_0.1=con.int.ab(0.1,10,25,m), alpha_0.05=con.int.ab(0.05,10,25,m),alpha_0.01=con.int.ab(0.01,10,25,m))
d<-data.frame(n1=25,n2=25, alpha_0.1=con.int.ab(0.1,25,25,m), alpha_0.05=con.int.ab(0.05,25,25,m),alpha_0.01=con.int.ab(0.01,25,25,m))
e<-data.frame(n1=100,n2=100,alpha_0.1=con.int.ab(0.1,100,100,m), alpha_0.05=con.int.ab(0.05,100,100,m),alpha_0.01=con.int.ab(0.01,100,100,m))
vrsetAB=rbind(a,b,c,d,e)

