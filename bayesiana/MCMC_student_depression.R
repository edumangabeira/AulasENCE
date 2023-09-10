
# #######################################
# Matéria: Inferênica Bayesiana         #
# Professor: Gustavo                    #
# Grupo: Sofia Brito e Julia Araujo     #          
#########################################


setwd('C:\\Users\\Windows10\\OneDrive\\Documentos\\Inferencia Bayesiana\\mcmc')
dados = read.csv2("Student Mental health.csv",header=T, dec= ".")
y = dados$Do.you.have.Depression.
x1 = dados$Age
x2 = dados$Your.current.year.of.Study
# Variável resposta = se a pessoa tem depressao (1) ou nao (0)
# Variavel dependente 1 = idade da pessoa
# Variavel dependente 2 = ano que a pessoa estuda

# Probabilidade de Sucesso = Ter depressao = exp(beta0+beta1*x1+beta2*x2)/(1+exp(beta0+beta1*x1+beta2*x2))

########## Estatística Clássica: Estimação por máxima verossimilhança ##########

modelo_classico = glm(y~x1+x2,family="binomial")
summary(modelo_classico)
# Beta0 =  0.64234
# Beta1 = -0.05944
# Beta2 = -0.02263


########## Estatística Bayesiana: Estimação por MCMC ##########

iteracoes = 200000
K=10000

beta0=2
beta1=-2
beta2=-2

var0 = 1
var1 = 0.002
var2 = 0.1

contador0=0
contador1=0
contador2=0


for (i in 2:iteracoes){
  
  # Beta0 proposto no log
  beta0_proposto = rnorm(1,beta0[i-1],sqrt(var0))
  numerador0 = sum(dbinom(y,1,exp(beta0_proposto+beta1[i-1]*x1+beta2[i-1]*x2)/
                            (1+exp(beta0_proposto+beta1[i-1]*x1+beta2[i-1]*x2)),log = TRUE)) + dnorm(beta0_proposto,0,sqrt(K),log = TRUE)
  
  denominador0 = sum(dbinom(y,1,exp(beta0[i-1]+beta1[i-1]*x1+beta2[i-1]*x2)/
                              (1+exp(beta0[i-1]+beta1[i-1]*x1+beta2[i-1]*x2)),log = TRUE)) + dnorm(beta0[i-1],0,sqrt(K),log = TRUE)
  
  # Calculando a Razão de Metropolis: 
  log_razao0 = numerador0 - denominador0
  # Calculando a probabilidade alfa: seria min {1,razao}, porém aplicamos o log
  log_alpha0 = min(0,log_razao0)
  
  # Verificando se vamos rejeitar ou não o Beta0 proposto
  valor_unfiforme0 = runif(1,0,1)
  if (log_alpha0 >= log(valor_unfiforme0)){
    beta0[i] = beta0_proposto
    contador0 = contador0+1
  } else{beta0[i] = beta0[i-1]}
  
  
  
  # Beta1 proposto no log
  beta1_proposto = rnorm(1,beta1[i-1],sqrt(var1))
  numerador1 = sum(dbinom(y,1,exp(beta0[i]+beta1_proposto*x1+beta2[i-1]*x2)/
                            (1+exp(beta0[i]+beta1_proposto*x1+beta2[i-1]*x2)),log = TRUE)) + dnorm(beta1_proposto,0,sqrt(K),log = TRUE)
  
  denominador1 = sum(dbinom(y,1,exp(beta0[i]+beta1[i-1]*x1+beta2[i-1]*x2)/
                              (1+exp(beta0[i]+beta1[i-1]*x1+beta2[i-1]*x2)),log = TRUE)) + dnorm(beta1[i-1],0,sqrt(K),log = TRUE)
  
  log_razao1 = numerador1 - denominador1
  log_alpha1 = min(0,log_razao1)
  
  valor_unfiforme1 = runif(1,0,1)
  if (log_alpha1 >= log(valor_unfiforme1)){
    beta1[i] = beta1_proposto
    contador1 = contador1+1
  } else{beta1[i] = beta1[i-1]}
  
  
  
  # Beta2 proposto no log
  beta2_proposto = rnorm(1,beta2[i-1],sqrt(var2))
  numerador2 = sum(dbinom(y,1,exp(beta0[i]+beta1[i]*x1+beta2_proposto*x2)/
                            (1+exp(beta0[i]+beta1[i]*x1+beta2_proposto*x2)),log = TRUE)) + dnorm(beta2_proposto,0,sqrt(K),log = TRUE)
  
  denominador2 = sum(dbinom(y,1,exp(beta0[i]+beta1[i]*x1+beta2[i-1]*x2)/
                              (1+exp(beta0[i]+beta1[i]*x1+beta2[i-1]*x2)),log = TRUE)) + dnorm(beta2[i-1],0,sqrt(K),log = TRUE)
  
  log_razao2 = numerador2 - denominador2
  log_alpha2 = min(0,log_razao2)
  
  valor_unfiforme2 = runif(1,0,1)
  if (log_alpha2 >= log(valor_unfiforme2)){
    beta2[i] = beta2_proposto
    contador2 = contador2+1
  } else{beta2[i] = beta2[i-1]}
  
}

# Taxa de aceitacao
contador0/iteracoes
contador1/iteracoes
contador2/iteracoes

par(mfrow=c(1,3))
plot(beta0,type="l",col=("#10430a"),
     main=expression(beta[0]),ylab = expression(beta[0]),xlab="Iterações")
plot(beta1,type="l",col=("#10430a"),
     main=expression(beta[1]),ylab = expression(beta[1]),xlab="Iterações")
plot(beta2,type="l",col=("#10430a"),
     main=expression(beta[2]),ylab = expression(beta[2]),xlab="Iterações")

burn=1000

par(mfrow=c(1,3))
plot(beta0[burn:iteracoes],type="l",col=("#10430a"),
     main=expression(beta[0]),ylab = expression(beta[0]),xlab="Iterações")
plot(beta1[burn:iteracoes],type="l",col=("#10430a"),
     main=expression(beta[1]),ylab = expression(beta[1]),xlab="Iterações")
plot(beta2[burn:iteracoes],type="l",col=("#10430a"),
     main=expression(beta[2]),ylab = expression(beta[2]),xlab="Iterações")

summary(beta0[burn:iteracoes])
summary(beta1[burn:iteracoes])
summary(beta2[burn:iteracoes])

par(mfrow=c(1,3))
hist(beta0[burn:iteracoes],prob=T,main=expression(beta[0]),col=("#10430a"),xlab=" ")
hist(beta1[burn:iteracoes],prob=T,main=expression(beta[1]),col=("#10430a"),xlab=" ")
hist(beta2[burn:iteracoes],prob=T,main=expression(beta[2]),col=("#10430a"),xlab=" ")


priori = rnorm(length(beta0[burn:iteracoes]),0,sqrt(K)) 

par(mfrow = c(1,3))
plot(density(beta0[burn:iteracoes]),type = "l",lwd = 1,main = expression(beta[0]))
polygon(density(beta0[burn:iteracoes]),col = "#10430a")
lines(density(priori),type = "l",lwd = 4, col = "chartreuse")

plot(density(beta1[burn:iteracoes]),type = "l",lwd = 1,main = expression(beta[1]))
polygon(density(beta1[burn:iteracoes]),col = "#10430a")
lines(density(priori),type = "l",lwd = 4, col = "chartreuse")

plot(density(beta2[burn:iteracoes]),type = "l",lwd = 1,main = expression(beta[2]))
polygon(density(beta2[burn:iteracoes]),col = "#10430a")
lines(density(priori),type = "l",lwd = 4, col = "chartreuse")


####### Preditiva

par(mfrow = c(1,1))
pi=exp(beta0+beta1*25+beta2*5)/(1+exp(beta0+beta1*25+beta2*5))
y.pred=rbinom(iteracoes,1,pi)
summary(y.pred[burn:iteracoes])
hist(y.pred[burn:iteracoes],
     prob=T,main="Preditiva para um estudante de 25 anos no quinto ano da universidade",
     col=("#10430a"),xlab=" ")

# Proporcao
1400/20






