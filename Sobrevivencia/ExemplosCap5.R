#####################################################################
### Escola Nacional de Ciencias Estatisticas - IBGE               ###
### Graduacao em Estatistica                                      ###
### Disciplina: Analise de Sobrevivencia                          ###    
### Exemplos - Modelos de Regressão Paramétricos                  ###  
### Cap. 5 - Livro Colosimo & Giolo                               ###
#####################################################################

### Dados de um Estudo sobre Cancer de laringe
# Estudo realizado com 90 pacientes do sexo masculino diagnosticados no periodo de 
# 1970 a 1978 com cancer de laringe e que foram acompanhados ate 01/01/1983. Para 
# cada paciente, foram registrados, no diagnostico, a idade (em anos) e o estagio da
# doenca (1 = tumor primario, 2 = envolvimento de nodulos, 3 = metastase e 4 = 
# combinacoes dos 3 estagios anteriores), bem como seus respectivos tempos de morte
# ou censura (em meses). Os estagios encontram-se ordenados pelo grau de severidade
# da doenca (menos serio para mais serio).
###Ajustando o modelo de Cox para esses dados:

# Lendo os dados no R:
laringe<-read.table("/Users/malutoledo/Google Drive/2020_1/Analise de Sobrevivencia/Aulas/Aula 14/laringe.txt",h=T)
attach(laringe)

# Executando os pacotes necessarios:
require(survival)

###Analise descritiva:
#Kaplan-Meier pela variavel estagio:
ekm_estagio<- survfit(Surv(tempos,cens)~estagio)
plot(ekm_estagio,lty=c(1,2,3,4),lwd=2,xlab="Tempo (meses)",ylab="S(t)_KM",main="S(t)_KM por estagio")
legend("topright",lty=c(1,2,3,4),c("Estagio 1","Estagio 2","Estagio 3","Estagio 4"),lwd=2,bty="n")
#Teste logrank para comparar as curvas de sobrevivencia por estagio:
survdiff(Surv(tempos,cens)~estagio,rho=0)
#categorizando a variavel idade (<=Mediana e >Mediana):
idade_cat<-vector(length=length(idade))
for (i in 1:length(idade)){
  if (idade[i]<=median(idade)) {
    idade_cat[i]<-"<=Mediana"
  } else  {
    idade_cat[i]<-">Mediana"
  } 
}
#Kaplan-Meier pela variavel idade (categorizada):
ekm_idade<- survfit(Surv(tempos,cens)~idade_cat)
plot(ekm_idade,lty=c(2,1),lwd=2,xlab="Tempo (meses)",ylab="S(t)_KM",main="S(t)_KM por idade")
legend("topright",lty=c(2,1),c("<=Mediana",">Mediana"),lwd=2,bty="n")
#Teste logrank para comparar as curvas de sobrevivencia por idade:
survdiff(Surv(tempos,cens)~idade_cat,rho=0)

#Modelo apenas com covariavel estagio:
fit2<-coxph(Surv(tempos,cens)~factor(estagio),x=T,method="breslow")
summary(fit2)
fit2$loglik

#Modelo com covariaveis estagio e idade:
fit3<-coxph(Surv(tempos,cens)~factor(estagio)+idade,x=T,method="breslow")
summary(fit3)
fit3$loglik

#Modelo com interacao:
fit4<-coxph(Surv(tempos,cens)~factor(estagio)+idade+factor(estagio)*idade,x=T,method="breslow")
summary(fit4)
fit4$loglik

#Avaliando a significancia do termo de interacao via TRV:
TRV=2*(fit4$loglik-fit3$loglik)
p.TRV<-1-pchisq(TRV,df=3)
TRV
p.TRV

#No modelo com interacao, os valores de RR se apresentam na coluna exp(coef):
summary(fit4)

#Construindo estimativas para as funcoes de taxa da falha acumulada e sobrevivencia de base:
Ht<-basehaz(fit4,centered=F)
Ht
tempos<-Ht$time
H0<-Ht$hazard
S0<-exp(-H0)
round(cbind(tempos,S0,H0),digits=5)

#Curvas de sobrevivencia estimadas para pacientes com idades de 50 e 65 anos, em cada um dos 4 estagios da doenca:
tt<-sort(tempos)
aux1<-as.matrix(tt)
n<-nrow(aux1)
aux2<-as.matrix(cbind(tempos,S0))
S00<-rep(max(aux2[,2]),n)
for(i in 1:n){
  if(tt[i]> min(aux2[,1])){
    i1<- aux2[,1]<= tt[i]
    S00[i]<-min(aux2[i1,2])}}
ts0<-cbind(tt,S00)
ts0
b<-fit4$coefficients
id<-50
st1<- S00^(exp(b[4]*id))                # S(t|x) estagio I   e idade = 50 anos
st2<- S00^(exp(b[1]+((b[4]+b[5])*id)))  # S(t|x) estagio II  e idade = 50 anos
st3<- S00^(exp(b[2]+((b[4]+b[6])*id)))  # S(t|x) estagio III e idade = 50 anos
st4<- S00^(exp(b[3]+((b[4]+b[7])*id)))  # S(t|x) estagio IV  e idade = 50 anos
id<- 65
st11<- S00^(exp(b[4]*id))               # S(t|x) estagio I   e idade = 65 anos
st21<- S00^(exp(b[1]+((b[4]+b[5])*id))) # S(t|x) estagio II  e idade = 65 anos
st31<- S00^(exp(b[2]+((b[4]+b[6])*id))) # S(t|x) estagio III e idade = 65 anos
st41<- S00^(exp(b[3]+((b[4]+b[7])*id))) # S(t|x) estagio IV  e idade = 65 anos
par(mfrow=c(1,1))
plot(tt,st1,type="s",ylim=range(c(0,1)),xlab="Tempos",ylab="S(t|x)",lty=1)
lines(tt,st2,type="s",lty=2)
lines(tt,st3,type="s",lty=3)
lines(tt,st4,type="s",lty=4)
legend(0,0.2,lty=c(1,2,3,4),c("estagio I","estagio II","estagio III","estagio IV"),
       lwd=1,bty="n",cex=0.7)
title("Idade = 50 anos")
plot(tt,st11,type="s",ylim=range(c(0,1)),xlab="Tempos",ylab="S(t|x)",lty=1)
lines(tt,st21,type="s",lty=2)
lines(tt,st31,type="s",lty=3)
lines(tt,st41,type="s",lty=4)
legend(0,0.2,lty=c(1,2,3,4),c("estagio I","estagio II","estagio III","estagio IV"),
       lwd=1,bty="n",cex=0.7)
title("Idade = 65 anos")

#Curvas dos riscos acumulados estimados para pacientes com idades de 50 e 65 anos, em cada um dos 4 estagios da doenca:
Ht1<- -log(st1)
Ht2<- -log(st2)
Ht3<- -log(st3)
Ht4<- -log(st4)
Ht11<- -log(st11)
Ht21<- -log(st21)
Ht31<- -log(st31)
Ht41<- -log(st41)
par(mfrow=c(1,1))
plot(tt,Ht1,type="s",ylim=range(c(0,4)),xlab="Tempos",ylab="Risco Acumulado",lty=1)
lines(tt,Ht2,type="s",lty=2)
lines(tt,Ht3,type="s",lty=3)
lines(tt,Ht4,type="s",lty=4)
legend(0.5,3.5, lty=c(1,2,3,4),c("estagio I","estagio II","estagio III","estagio IV"),
       lwd=1,bty="n",cex=0.7)
title("Idade = 50 anos")
plot(tt,Ht11,type="s",ylim=range(c(0,4)),xlab="Tempos",ylab="Risco Acumulado",lty=1)
lines(tt,Ht21,type="s",lty=2)
lines(tt,Ht31,type="s",lty=3)
lines(tt,Ht41,type="s",lty=4)
legend(0.5,3.5, lty=c(1,2,3,4),c("estagio I","estagio II","estagio III","estagio IV"),
       lwd=1,bty="n",cex=0.7)
title("Idade = 65 anos")

###Avaliando a adequacao do modelo:

#Residuos de Shoenfeld (para avaliacao da proporcionalidade de riscos):
residuals(fit4,type="scho")
#Teste de proporcionalidade dos riscos:
#GLOBAL: H0: riscos proporcionais
# Para cada covariavel: H0: Riscos proporcionais para a q-esima variavel
cox.zph(fit4,transform="identity") ###g(t)=t
#Tanto o teste global quanto os testes para cada covariavel nao apresentaram evidencias
# para rejeicao da hipotese nula de riscos proporcionais.  
par(mfrow=c(2,2))
plot(cox.zph(fit4))
#Nos graficos, tendencias ao longo do tempo nao sao evidentes. Nao ha, portanto, 
#evidencias de violacao da suposicao de riscos proporcionais.

#Residuos Martingal (para avaliacao da forma funcional das covariaveis):
resid_marting<-residuals(fit4,type="martingale")
#plotando os residuos martingal versus a covariavel idade
library(ggplot2)
ggplot(data = laringe, mapping = aes(x = idade, y = resid_marting)) +
  geom_point() +
  geom_smooth() +
  labs(title = "idade") +
  theme_bw() + theme(legend.key = element_blank())

#Residuos de Cox-Snell (para avaliacao da qualidade geral do ajuste do modelo):
resid_coxsnell <- laringe$cens-resid_marting
#Ajustando o modelo de Cox nulo aos residuos (que têm dist Expo(1) aproximada, sob o modelo correto)
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, laringe$cens) ~ 1)
#Estimador de Nelson-Aalen-Breslow para a taxa de falha de base (todas as covariaveis zero)
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)
#grafico de Lambda(ei) versus ei - deve ser aproximadamente uma reta se a dist
#exponencial padrao for adequada
ggplot(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +
  scale_x_continuous(limit = c(0,3.2)) +
  scale_y_continuous(limit = c(0,3.2)) +
  labs(x = "Residuos de Cox-Snell",
    y = "Taxa de falha acumulada estimada") +
  theme_bw() + theme(legend.key = element_blank())

#Residuos Deviance (para deteccao de pontos atipicos):
resid_deviance<-residuals(fit4,type="deviance")
#residuos deviance versus preditor linear:
b1<-fit4$coefficients[1]
b2<-fit4$coefficients[2]
b3<-fit4$coefficients[3]
b4<-fit4$coefficients[4]
b5<-fit4$coefficients[5]
b6<-fit4$coefficients[6]
b7<-fit4$coefficients[7]

pred<-vector(length=90)

for (i in 1:90){
  if(laringe$estagio[i]==1){
    pred[i]<-b4*laringe$idade[i]
  }
  else if(laringe$estagio[i]==2){
    pred[i]<-b1+b4*laringe$idade[i]+b5*laringe$idade[i]    
  }
  else if(laringe$estagio[i]==3){
    pred[i]<-b2+b4*laringe$idade[i]+b6*laringe$idade[i]    
  }
  else{
    pred[i]<-b3+b4*laringe$idade[i]+b7*laringe$idade[i]    
  }
}

plot(pred,resid_deviance,xlab="Preditor linear",ylab="Residuos Deviance")

#Residuos dfbetas (para deteccao de pontos influentes):
resid_dfbetas<-residuals(fit4,type="dfbetas")
#"criando" os valores das covariaveis x1 a x7:
x1<-numeric(90) #indicador de estagio 2
x2<-numeric(90) #indicador de estagio 3
x3<-numeric(90) #indicador de estagio 4

for (i in 1:90){
  if(laringe$estagio[i]==2){
    x1[i]<-1
  }
  else if(laringe$estagio[i]==3){
    x2[i]<-1   
  }
  else if(laringe$estagio[i]==4){
    x3[i]<-1
  }
}

x4<-idade
x5<-x1*x4 #interacao entre idade e indicador de estagio 2
x6<-x2*x4 #interacao entre idade e indicador de estagio 3
x7<-x3*x4 #interacao entre idade e indicador de estagio 4

#grafico desses residuos para cada covariavel versus os valores da respectiva covariavel
plot(x1,resid_dfbetas[,1],xlab="x1",ylab="Dfbetas para x1")
plot(x2,resid_dfbetas[,2],xlab="x2",ylab="Dfbetas para x2")
plot(x3,resid_dfbetas[,3],xlab="x3",ylab="Dfbetas para x3")
plot(x4,resid_dfbetas[,4],xlab="x4",ylab="Dfbetas para x4")
plot(x5,resid_dfbetas[,5],xlab="x5",ylab="Dfbetas para x5")
plot(x6,resid_dfbetas[,6],xlab="x6",ylab="Dfbetas para x6")
plot(x7,resid_dfbetas[,7],xlab="x7",ylab="Dfbetas para x7")

###########################################################################################################################################
### Dados de aleitamento materno (Fonte: Gilardoni e Colosimo, 2006)
desmame<-read.table("/Users/malutoledo/Google Drive/2020_1/Analise de Sobrevivencia/Aulas/Aula 14/desmame.txt",h=T)
fit1<-coxph(Surv(tempo,cens)~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11,data=desmame,x=T,method="breslow")
summary(fit1)

#Prosseguindo com a reducao do modelo, temos o seguinte modelo final:
fit<-coxph(Surv(tempo,cens)~V1+V3+V4+V6,data=desmame,x=T,method="breslow")
summary(fit)
fit$loglik


#Metodo grafico descritivo para avaliacao da proporcionalidade de riscos 
# (Curvas nao paralelas significam desvios da suposicao de riscos proporcionais. 
# Situacoes extremas de violacao da suposicao ocorrem quando as curvas se cruzam):
attach(desmame)
fit1<-coxph(Surv(tempo[V1==0],cens[V1==0])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
par(mfrow=c(1,1))
plot(ss$time,log(H0),xlim=range(c(0,20)),xlab="Tempos",
     ylab=expression(log(Lambda[0]*(t))), bty="n",type="s")
fit2<-coxph(Surv(tempo[V1==1],cens[V1==1])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=2)
legend(10,-3,lty=c(2,1),c("V1 = 1 (Nao)","V1 = 0 (Sim)"),lwd=1,bty="n",cex=0.7)
title("V1: Experiencia Amamentacao")

fit1<-coxph(Surv(tempo[V3==0],cens[V3==0])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0),xlim=range(c(0,20)),xlab="Tempos",
     ylab=expression(log(Lambda[0]*(t))), bty="n",type="s")
fit2<-coxph(Surv(tempo[V3==1],cens[V3==1])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=2)
legend(10,-3,lty=c(2,1),c("V3 = 1 (<=6 meses)","V3 = 0 (> 6 meses)"),lwd=1,bty="n",cex=0.7)
title("V3: Conceito Amamentacao")

fit1<-coxph(Surv(tempo[V4==0],cens[V4==0])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0),xlim=range(c(0,20)),xlab="Tempos",
     ylab=expression(log(Lambda[0]*(t))), bty="n",type="s")
fit2<-coxph(Surv(tempo[V4==1],cens[V4==1])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=2)
legend(10,-3,lty=c(2,1),c("V4 = 1 (Sim)","V4 = 0 (Nao)"),lwd=1,bty="n",cex=0.7)
title("V4: Dificuldades amamentar")

fit1<-coxph(Surv(tempo[V6==0],cens[V6==0])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit1)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0),xlim=range(c(0,20)),xlab="Tempos",
     ylab=expression(log(Lambda[0]*(t))), bty="n",type="s")
fit2<-coxph(Surv(tempo[V6==1],cens[V6==1])~1,data=desmame,x=T,method="breslow")
ss<- survfit(fit2)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=2)
legend(10,-3,lty=c(2,1),c("V6 = 1 (Nao)","V6 = 0 (Sim)"),lwd=1,bty="n",cex=0.7)
title("V6: Leite Materno exclusivo na Maternidade")
#Obs: As curvas nao indicam violacao da suposicao de riscos proporcionais.

#Graficos dos residuos padronizados de Schoenfeld:
residuals(fit,type="schoe")
cox.zph(fit,transform="identity") ###g(t)=t
#Tanto o teste global quanto os testes para cada covariavel nao apresentaram evidencias
# para rejeico da hipotese nula de riscos proporcionais.  
par(mfrow=c(2,2))
plot(cox.zph(fit))

#Residuos de Cox-Snell (para avaliacao da qualidade geral do ajuste do modelo):
resid_coxsnell <- desmame$cens-residuals(fit,type="martingale")
#Ajustando o modelo de Cox nulo aos residuos (que têm dist Expo(1) aproximada, sob o modelo correto)
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, desmame$cens) ~ 1)
#Estimador de Nelson-Aalen-Breslow para a taxa de falha de base (todas as covariaveis zero)
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)
#grafico de Lambda(ei) versus ei - deve ser aproximadamente uma reta se a dist
#exponencial padrao for adequada
ggplot(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +
  scale_x_continuous(limit = c(0,3.2)) +
  scale_y_continuous(limit = c(0,3.2)) +
  labs(x = "Residuos de Cox-Snell",
    y = "Taxa de falha acumulada estimada") +
  theme_bw() + theme(legend.key = element_blank())

#Residuos Deviance (para deteccao de pontos atipicos):
resid_deviance<-residuals(fit,type="deviance")
#residuos deviance versus preditor linear:
b1<-fit$coefficients[1]
b2<-fit$coefficients[2]
b3<-fit$coefficients[3]
b4<-fit$coefficients[4]
pred<-b1*desmame$V1+b2*desmame$V3+b3*desmame$V4+b4*desmame$V6
plot(resid_deviance,pred,xlab="Residuos Deviance",ylab="Preditor linear")

#Residuos dfbetas (para deteccao de pontos influentes):
resid_dfbetas<-residuals(fit,type="dfbetas")
#grafico desses residuos para cada covariavel versus os valores da respectiva covariavel
plot(desmame$V1,resid_dfbetas[,1],xlab="V1",ylab="Dfbetas para V1")
plot(desmame$V3,resid_dfbetas[,2],xlab="V3",ylab="Dfbetas para V3")
plot(desmame$V4,resid_dfbetas[,3],xlab="V4",ylab="Dfbetas para V4")
plot(desmame$V6,resid_dfbetas[,4],xlab="V6",ylab="Dfbetas para V6")

#No modelo final, os valores de RR se apresentam na coluna exp(coef):
summary(fit)

############################################################
##### ### Dados de leucemia pediatrica
###########################################################

leuc<-read.table("/Users/malutoledo/Google Drive/2020_1/Analise de Sobrevivencia/Aulas/Aula 14/leucemia.txt", h=T)        #Obs: leucemia.txt no Ap?ndice A1
attach(leuc)
idadec<-ifelse(idade>96,1,0)
leuinic<-ifelse(leuini>75,1,0)
zpesoc<-ifelse(zpeso>-2,1,0)
zestc<-ifelse(zest>-2,1,0)
pasc<-ifelse(pas>0.05,1,0)
vacc<-ifelse(vac>15,1,0)
pasc<-ifelse(pas>5,1,0)
riskc<-ifelse(risk>1.7,1,0)
r6c<-r6
leucc<-as.data.frame(cbind(leuinic,tempos,cens,idadec,zpesoc,zestc,pasc,vacc,riskc,r6c))
detach(leuc)
attach(leucc)
fit<-coxph(Surv(tempos,cens)~leuinic+idadec+zpesoc+zestc+pasc+vacc+riskc+r6c,
  data=leucc, x = T, method="breslow")
summary(fit)

fit3<-coxph(Surv(tempos,cens)~leuinic+idadec+zpesoc+pasc+vacc,data=leucc,x = T,method="breslow")
summary(fit3)
-2*fit3$loglik[2]

#metodo grafico para verificar a suposicao de riscos proporcionais:
par(mfrow=c(2,3))
fit<-coxph(Surv(tempos[leuinic==1],cens[leuinic==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0), xlab="Tempos",ylim=range(c(-5,1)),ylab = expression(log(Lambda[0]* (t))),bty="n",type="s")
fit<-coxph(Surv(tempos[leuinic==0],cens[leuinic==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=4)
legend(1.5,-4,lty=c(4,1),c("< 75","> 75"),lwd=1,bty="n",cex=0.8)
title("LEUINI")

fit<-coxph(Surv(tempos[idadec==1],cens[idadec==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0), xlab="Tempos",ylim=range(c(-5,1)),ylab = expression(log(Lambda[0]* (t))),bty="n",type="s")
fit<-coxph(Surv(tempos[idadec==0],cens[idadec==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=4)
legend(1.5,-4,lty=c(4,1),c("<96 meses",">96 meses"),lwd=1,bty="n",cex=0.8)
title("IDADE")

fit<-coxph(Surv(tempos[zpesoc==1],cens[zpesoc==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0), xlab="Tempos",ylim=range(c(-5,1)),ylab = expression(log(Lambda[0]* (t))),bty="n",type="s")
fit<-coxph(Surv(tempos[zpesoc==0],cens[zpesoc==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=4)
legend(1.5,-4,lty=c(4,1),c("<-2",">-2"),lwd=1,bty="n",cex=0.8)
title("ZPESO")

fit<-coxph(Surv(tempos[pasc==1],cens[pasc==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0), xlab="Tempos",ylim=range(c(-5,1)),ylab = expression(log(Lambda[0]* (t))),bty="n",type="s")
fit<-coxph(Surv(tempos[pasc==0],cens[pasc==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=4)
legend(1.5,-4,lty=c(4,1),c("<5%",">5%"),lwd=1,bty="n",cex=0.8)
title("PAS")

fit<-coxph(Surv(tempos[vacc==1],cens[vacc==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
plot(ss$time,log(H0), xlab="Tempos",ylim=range(c(-5,1)),ylab = expression(log(Lambda[0]* (t))),bty="n",type="s")
fit<-coxph(Surv(tempos[vacc==0],cens[vacc==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv,digits=5)
H0<- -log(s0)
lines(ss$time,log(H0),type="s",lty=4)
legend(1.5,-4,lty=c(4,1),c("<15%",">15%"),lwd=1,bty="n",cex=0.8)
title("VAC")

#analise de residuos:
resid(fit3,type="scho")
cox.zph(fit3, transform="identity")   ## g(t) = t
par(mfrow=c(2,3))
plot(cox.zph(fit3))

par(mfrow=c(1,2))
rd<-resid(fit3,type="deviance")       # residuos deviance
rm<-resid(fit3,type="martingale")     # residuos martingal
pl<-fit3$linear.predictors
plot(pl,rm, xlab="Preditor linear", ylab="Res?duo martingal", pch=16)
plot(pl,rd,  xlab="Preditor linear", ylab="Res?duo deviance" , pch=16)


par(mfrow=c(2,3))
dfbetas<-resid(fit3,type="dfbeta")
plot(leuinic,dfbetas[,1], xlab="Leuini", ylab="Influ?ncia para Leuini")
plot(idadec, dfbetas[,2], xlab="Idade",  ylab="Influ?ncia para Idade")
plot(zpesoc, dfbetas[,3], xlab="Zpeso",  ylab="Influ?ncia para Zpeso")
plot(pasc,   dfbetas[,4], xlab="Pas",    ylab="Influ?ncia para Pas")
plot(vacc,   dfbetas[,5], xlab="Vac",    ylab="Influ?ncia para Vac")

