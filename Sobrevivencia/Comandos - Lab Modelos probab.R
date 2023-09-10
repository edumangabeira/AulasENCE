#######################################################################################################################################
### Escola Nacional de Ciencias Estatisticas - IBGE                                                                                 ###
### Graduacao em Estatistica                                                      ###
### Disciplina: Analise de Sobrevivencia                                                                   ###
### Profa. Maria Luiza Toledo                                                                                       ###
### Roteiro - Aula de Laboratorio - Modelos probabilisticos                                                                                                  ###
#######################################################################################################################################

### Dados - Pacientes com cancer de bexiga (Fonte: Colosimo e Giolo, 2006)
# Sao considerados os tempos de reincidencia, em meses, de um grupo de 20 pacientes com cancer de bexiga que foram submetidos a um procedimento
# cirurgico feito por laser. Os tempos obtidos foram: 3, 5, 6, 7, 8, 9, 10, 10+, 12, 15, 15+, 18, 19, 20, 22, 25, 28, 30, 40, 45+, em que o 
# simbolo + indica censura. 

# Entrando com os dados no R:
tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)

# Executando os pacotes necessarios:
require(survival)
require(flexsurv)

# Visualizando os dados em formato de falha e censura (opcional):
Surv(tempos,cens)

#######################################################################################################################################
### Estimacao usando a distribuicao Exponencial:
ajuste_exp<-survreg(Surv(tempos,cens)~1,dist='exponential')
ajuste_exp
alfa_exp<-exp(ajuste_exp$coefficients[1]) # EMV do parametro de escala da Exponencial
alfa_exp
varalfa_exp<-ajuste_exp$var[1, 1]*(exp(ajuste_exp$coefficient[1]))^2 # EMV da variancia do parametro de escala
IC<-c(alfa_exp-qnorm(0.975)*sqrt(varalfa_exp),alfa_exp+qnorm(0.975)*sqrt(varalfa_exp)) # IC de 95% para o parametro de escala
IC

# Estimando a funcao de taxa de falha:
lambda_exp<-1/alfa_exp
lambda_exp
lambda_exp<-function(t){1/alfa_exp*(t^0)} #criando uma funcao no R para a taxa de falha 
plot(lambda_exp,ylab=expression(lambda(t)),xlab="t (em meses)",xlim=c(0,45)) #plotando a funcao de taxa de falha

# Estimando o percentil 100p%:
perc_exp<-function(p){-alfa_exp*log(1-p)}#criando uma funcao no R para o percentil
perc_exp(p=0.05)#estimando o percentil 5%
perc_exp(p=0.5)#estimando o percentil 50% ou mediana
perc_exp(p=0.9)#estimando o percentil 90%
plot(perc_exp,ylab="tp (em meses)",xlab="p") #plotando o percentil versus p

#Estimando a funcao de sobrevivencia:
S_exp<-function(t){exp(-t/alfa_exp)}#criando uma funcao no R para a sobrevivencia
var_S_exp<-function(t){varalfa_exp*((t/alfa_exp^2)*exp(-t/alfa_exp))^2}#criando uma funcao no R para a variancia da funcao de sobrevivencia
S_exp(t=1)#estimando a funcao de sobrevivencia em 1 m?s
c(S_exp(t=1)-qnorm(0.975)*sqrt(var_S_exp(t=1)),S_exp(t=1)+qnorm(0.975)*sqrt(var_S_exp(t=1)))#IC 95% para S(1)
S_exp(t=20)#estimando a fun??o de sobrevivencia em 20 meses
c(S_exp(t=20)-qnorm(0.975)*sqrt(var_S_exp(t=20)),S_exp(t=20)+qnorm(0.975)*sqrt(var_S_exp(t=20)))#IC 95% para S(20)
S_exp(t=47)#estimando  a fun??o de sobrevivencia em 47 meses
c(S_exp(t=47)-qnorm(0.975)*sqrt(var_S_exp(t=47)),S_exp(t=47)+qnorm(0.975)*sqrt(var_S_exp(t=47)))#IC 95% para S(47)
plot(S_exp,ylab="S(t)",xlab="t (em meses)",xlim=c(0,100)) #plotando a funcao de sobreviv?ncia

#######################################################################################################################################
### Estimacao usando a distribuicao Weibull:
ajuste_wei<-survreg(Surv(tempos,cens)~1,dist='weibull')
ajuste_wei
alfa_wei<-exp(ajuste_wei$coefficients[1]) # EMV do parametro de escala da Weibull
gama_wei<-1/ajuste_wei$scale# EMV do parametro de forma da Weibull
cbind(gama_wei,alfa_wei)
varalfa_wei<-ajuste_wei$var[1,1]*exp(ajuste_wei$coefficients[1])^2
varalfa_wei
IC_alfa<-c(alfa_wei-qnorm(0.975)*sqrt(varalfa_wei),alfa_wei+qnorm(0.975)*sqrt(varalfa_wei)) # IC de 95% para o parametro de escala
IC_alfa
vargama_wei<-ajuste_wei$var[2,2]*(-1/ajuste_wei$scale^2)^2
vargama_wei
IC_gama<-c(gama_wei-qnorm(0.975)*sqrt(vargama_wei),gama_wei+qnorm(0.975)*sqrt(vargama_wei)) # IC de 95% para o parametro de escala
IC_gama

#Estimando o tempo medio ate a reincidencia:
E_T=alfa_wei*gamma(1+1/gama_wei)
E_T

# Estimando a funcao de taxa de falha:
lambda_wei<-function(t){gama_wei/alfa_wei^gama_wei*t^(gama_wei-1)}#criando uma funcao no R para a taxa de falha 
lambda_wei(t=1)#estimando a funcao de taxa de falha em 1 mes
lambda_wei(t=20)#estimando a funcao de taxa de falha em 20 meses
lambda_wei(47)#estimando a funcao de taxa de falha em 47 meses
plot(lambda_wei,ylab=expression(lambda(t)),xlab="t (em meses)",xlim=c(0,45)) #plotando a funcao de taxa de falha

# Estimando o percentil 100p%:
perc_wei<-function(p){alfa_wei*(-log(1-p))^(1/gama_wei)}#criando uma funcao no R para o percentil
perc_wei(p=0.05)#estimando o percentil 5%
perc_wei(p=0.5)#estimando o percentil 50% ou mediana
perc_wei(p=0.9)#estimando o percentil 90%
plot(perc_wei,ylab="tp (em meses)",xlab="p") #plotando o percentil versus p

#Estimando a fun??o de sobreviv?ncia:
S_wei<-function(t){exp(-(t/alfa_wei)^gama_wei)}#criando uma funcao no R para a sobreviv?ncia
S_wei(t=1)#estimando a fun??o de sobrevivencia em 1 mes
S_wei(t=20)#estimando a fun??o de sobrevivencia em 20 meses
S_wei(t=47)#estimando  a fun??o de sobrevivencia em 47 meses
plot(S_wei,ylab="S(t)",xlab="t (em meses)",xlim=c(0,100)) #plotando a funcao de sobreviv?ncia

#######################################################################################################################################
### Estimacao usando a distribuicao Log-normal:
ajuste_logn<-survreg(Surv(tempos,cens)~1,dist='lognorm')
ajuste_logn
mu_logn<-ajuste_logn$coefficients[1] # EMV do parametro de locacao da Log-normal
sigma_logn<-ajuste_logn$scale# EMV do parametro de escala da Log-normal
cbind(mu_logn,sigma_logn)
var_mu<-ajuste_logn$var[1,1]# variancia da estimativa do parametro de locacao
var_mu
IC_mu<-c(mu_logn-qnorm(0.975)*sqrt(var_mu),mu_logn+qnorm(0.975)*sqrt(var_mu)) # IC de 95% para o parametro de locacao
IC_mu
var_sigma<-ajuste_logn$var[2,2]
var_sigma # variancia da estimativa do parametro de escala
IC_sigma<-c(sigma_logn-qnorm(0.975)*sqrt(var_sigma),sigma_logn+qnorm(0.975)*sqrt(var_sigma)) # IC de 95% para o parametro de escala
IC_sigma

#Estimando o tempo medio ate a reincidencia:
E_T=exp(mu_logn+(sigma_logn^2)/2)
E_T

# Estimando o percentil 100p%:
perc_logn<-function(p){exp(qnorm(p)*sigma_logn+mu_logn)}#criando uma funcao no R para o percentil
perc_logn(p=0.05)#estimando o percentil 5%
perc_logn(p=0.5)#estimando o percentil 50% ou mediana
perc_logn(p=0.9)#estimando o percentil 90%
plot(perc_logn,ylab="tp (em meses)",xlab="p") #plotando o percentil versus p

#Estimando a funcao de sobrevivencia:
S_logn<-function(t){pnorm((-log(t)+mu_logn)/sigma_logn)}#criando uma funcao no R para a sobrevivencia
S_logn(t=1)#estimando a funcao de sobrevivencia em 1 mes
S_logn(t=20)#estimando a funcao de sobrevivencia em 20 meses
S_logn(t=47)#estimando  a funcao de sobrevivencia em 47 meses
plot(S_logn,ylab="S(t)",xlab="t (em meses)",xlim=c(0,100)) #plotando a funcao de sobrevivencia

# Estimando a fun??o de taxa de falha:
f_logn<-function(t){(1/(sqrt(2*pi)*t*sigma_logn))*exp(-0.5*((log(t)-mu_logn)/sigma_logn)^2)}#criando uma fun?ao no R para a densidade
lambda_logn<-function(t){f_logn(t)/S_logn(t)}#criando uma fun??o no R para a taxa de falha 
lambda_logn(t=1)#estimando a fun??o de taxa de falha em 1 m?s
lambda_logn(t=20)#estimando a fun??o de taxa de falha em 20 meses
lambda_logn(47)#estimando a fun??o de taxa de falha em 47 meses
plot(lambda_logn,ylab=expression(lambda(t)),xlab="t (em meses)",xlim=c(0,45)) #plotando a fun??o de taxa de falha

#########################################################################################################################################
### Plotando as fun??es das tr?s distribui??es conjuntamente:

#Fun??o densidade:
f_exp<-function(t){(1/alfa_exp)*exp(-t/alfa_exp)} #fun??o densidade da Exponencial
f_wei<-function(t){(gama_wei/alfa_wei^gama_wei)*t^(gama_wei-1)*exp(-(t/alfa_wei)^gama_wei)} #fun??o densidade da Weibull

plot(f_exp,type="l",col=4,ylab="f(t)",xlab="t",xlim=c(0,80))#plotando as fun??es
t=seq(0,80,0.01)
lines(t,f_wei(t),type="l",col=6)
lines(t,f_logn(t),type="l",col=9)
Legenda<-c(expression(Exponencial),expression(Weibull),expression(Log-normal))
legend("topright", Legenda,lwd=c(1,1,1),cex=1.2,inset=0.00,col=c(4,6,9),bty="n")

#Fun??o de taxa de falha:
plot(lambda_exp,type="l",col=4,ylab=expression(lambda(t)),xlab="t",xlim=c(0,80),ylim=c(0,0.12))#plotando as fun??es
t=seq(0,80,0.01)
lines(t,lambda_wei(t),type="l",col=6)
lines(t,lambda_logn(t),type="l",col=9)
Legenda<-c(expression(Exponencial),expression(Weibull),expression(Log-normal))
legend("topright", Legenda,lwd=c(1,1,1),cex=1.2,inset=0.00,col=c(4,6,9),bty="n")

#Fun??o de sobreviv?ncia:
plot(S_exp,type="l",col=4,ylab="S(t)",xlab="t",xlim=c(0,80))#plotando as fun??es
t=seq(0,80,0.01)
lines(t,S_wei(t),type="l",col=6)
lines(t,S_logn(t),type="l",col=9)
Legenda<-c(expression(Exponencial),expression(Weibull),expression(Log-normal))
legend("topright", Legenda,lwd=c(1,1,1),cex=1.2,inset=0.00,col=c(4,6,9),bty="n")


#########################################################################################################################################
### Escolha do modelo probabil?stico: Teste da raz?o de verossimilhan?as

ajuste_exp$loglik[2]#armazenando o m?ximo da log-verossimilhan?a para o ajuste pela Exponencial
ajuste_wei$loglik[2]#armazenando o m?ximo da log-verossimilhan?a para o ajuste pela Weibull
ajuste_logn$loglik[2]#armazenando o m?ximo da log-verossimilhan?a para o ajuste pela Log-normal
ajuste_gamg<-flexsurvreg(Surv(tempos,cens)~1,dist="gengamma")
ajuste_gamg$loglik #armazenando o m?ximo da log-verossimilhan?a para o ajuste pela Gama generalizada (refer?ncia)

#Calculando as estatisticas TRV: 
TRV_exp=2*(ajuste_gamg$loglik-ajuste_exp$loglik[2])
TRV_exp
TRV_wei=2*(ajuste_gamg$loglik-ajuste_wei$loglik[2])
TRV_wei
TRV_logn=2*(ajuste_gamg$loglik-ajuste_logn$loglik[2])
TRV_logn

#Calculando os valores-p:
1-pchisq(TRV_exp,df=2)
1-pchisq(TRV_wei,df=1)
1-pchisq(TRV_logn,df=1)


