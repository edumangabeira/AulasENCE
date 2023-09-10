beta0.v=0.3
beta1.v=-0.186
n=30
x=rnorm(n)

pi.v=exp(beta0.v+beta1.v*x)/(1+exp(beta0.v+beta1.v*x))

y=rbinom(30,1,pi.v)

summary(y)

summary(glm(y~x,family="binomial"))

####################

M=10000
K=10000

beta0=-10
beta1=10

delta.beta0=2.5
delta.beta1=2.5

cont.beta0=0
cont.beta1=0

for (j in 2:M){
    
    #para beta0
    
    beta0.p=rnorm(1,beta0[j-1],sqrt(delta.beta0))
    
    num= prod(dbinom(y,1,exp(beta0.p+beta1[j-1]*x)/(1+exp(beta0.p+beta1[j-1]*x))))*dnorm(beta0.p,0,sqrt(K))
    
    den=prod(dbinom(y,1,exp(beta0[j-1]+beta1[j-1]*x)/(1+exp(beta0[j-1]+beta1[j-1]*x))))*dnorm(beta0[j-1],0,sqrt(K))
    
    alpha=num/den
    
    teste=runif(1,0,1)
    if (teste<=alpha){beta0[j]=beta0.p;cont.beta0=cont.beta0+1} else{beta0[j]=beta0[j-1]}
    
    
    #para beta1
    
    
    beta1.p=rnorm(1,beta1[j-1],sqrt(delta.beta1))
    
    num= prod(dbinom(y,1,exp(beta0[j]+beta1.p*x)/(1+exp(beta0[j]+beta1.p*x))))*dnorm(beta1.p,0,sqrt(K))
    
    den=prod(dbinom(y,1,exp(beta0[j]+beta1[j-1]*x)/(1+exp(beta0[j]+beta1[j-1]*x))))*dnorm(beta1[j-1],0,sqrt(K))
    
    alpha=num/den
    
    teste=runif(1,0,1)
    if (teste<=alpha){beta1[j]=beta1.p;cont.beta1=cont.beta1+1} else{beta1[j]=beta1[j-1]}
    
}

cont.beta0/M
cont.beta1/M

burn=1000

par(mfrow=c(1,2))

plot(beta0[burn:M],type="l")
plot(beta1[burn:M],type="l")

hist(beta0[burn:M],prob=T)
hist(beta1[burn:M],prob=T)

summary(beta0[burn:M])
summary(beta1[burn:M])


#previsão para x=1.3

#posteriori do pi
pi=exp(beta0+beta1*1.3)/(1+exp(beta0+beta1*1.3))

y.pred=rbinom(M,1,pi)

summary(y.pred[burn:M])
