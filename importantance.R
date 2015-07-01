

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

M=3000
ans<-replicate(M, vrunif(0.1,10))
p<-round(sum(ans)/M*100, digits=3)
se<-round(sd(ans)/sqrt(M),digits=5)
rbind(p,se)

length(subset(ans,ans==1))





weight<-function(s){
  x<-runif(s,0,0.5)
  exp(sum(dunif(x,0,1,log=TRUE))-sum(dunif(x,0,0.5,log=TRUE)))
}

weight(11100)
weight(50)
weight(2)
weight(0.5)
s<-(0:100)
s
plot(s,weight(s))
