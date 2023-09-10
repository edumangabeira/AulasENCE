###############################################################
## Planejamento de Experimentos                               #
## Profa. Maria Luiza G. de Toledo                            #
## Analise de Variancia com um Fator                          #
## Dados: Resistencia de fibras sinteticas                    #
###############################################################

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(car)

resistencia=c(7,7,15,11,9,
  12,17,12,18,18,
  14,18,18,19,19,
  19,25,22,19,23,
  7,10,11,15,11)
perc_algodao=c(rep(15,5),rep(20,5),rep(25,5),rep(30,5),rep(35,5))
dados<-data.frame(resistencia,perc_algodao)
#Construindo graficos iniciais:
ggline(dados, x = 'perc_algodao', y = 'resistencia',
  add = c("mean_se", "jitter"),
  ylab = "Resistencia", xlab = "% de algodao",title="Resistencia da Fibra x % de algodao")

ggboxplot(dados, x = 'perc_algodao', y = 'resistencia',
  color = "perc_algodao", ylab = "Resistencia", xlab = "% de algodao")

#Modelo Anova:
modelo=aov(resistencia~factor(perc_algodao))
summary(modelo)

#Estimacao dos parametros do modelo:
perc_algodao<-as.factor(perc_algodao)
summary.lm(modelo) #referência: 15% de algodão
levels(perc_algodao)
perc_algodao <- relevel(perc_algodao, ref="30") #mudando o nível de referência
levels(perc_algodao)
modelo2=aov(resistencia~perc_algodao)
summary.lm(modelo2) #referência: 30% de algodão
#media geral:
fibra.mean <- mean(resistencia)
fibra.mean
# efeitos dos tratamentos:
model.tables(modelo)
# IC:
QMe <- summary(modelo)[[1]][2,3]
QMe
DP.comb <- sqrt(QMe/5)
t.crit <- c(-1,1)*qt(.975,16)
#Estimacao da media para o 4o nivel:
mean.4=tapply(dados$resistencia,dados$perc_algodao,mean)[[4]]
IC.mean.4=mean.4+t.crit*DP.comb
mean.4
IC.mean.4
#Comparando as medias dos niveis 1 e 4:
mean.14=tapply(dados$resistencia,dados$perc_algodao,mean)[[1]]-tapply(dados$resistencia,dados$perc_algodao,mean)[[4]]
IC.mean.14=mean.14+t.crit*sqrt((2*QMe)/5)
mean.14
IC.mean.14

##############################################
# Contrastes ortogonais:
#############################################
contrast1 <- c(0,0,0,-1,1)
contrast2 <- c(1,0,1,-1,-1)
contrast3 <- c(1,0,-1,-1,1)
contrast4 <- c(-1,4,-1,-1,-1)

dados$perc_algodao = as.factor(dados$perc_algodao)
contrasts(dados$perc_algodao) = cbind(contrast1, contrast2,contrast3, contrast4)
contrasts(dados$perc_algodao)

modelo2= aov(resistencia~perc_algodao, dados)
summary.aov(modelo2, split=list(perc_algodao=list("Contraste 1"=1, "Contraste 2"=2, "Contraste 3"=3,"Contraste 4"=4))) #

##############################################
# Testes de Tukey para comparacoes multiplas:
##############################################
TukeyHSD(modelo) #Teste de Tukey

#############################
# A Suposicao de Normalidade:
#############################
eij<-modelo$residuals
eij
qqnorm(eij) #grafico de probabilidade normal dos residuos
qqline(eij)

# residuos padronizados:
1-(2*pnorm(-1))
1-(2*pnorm(-2))
1-(2*pnorm(-3))
plot(modelo,2) #grafico de probabilidade normal dos residuos padronizados
QMe <- summary(modelo)[[1]][2,3]
dij<-eij/sqrt(QMe)
dij

################################
# Residuos vs. ordem de coleta:
################################
#informando a ordem de coleta dos dados:
ordem<- c(15,19,25,12,6,8,14,1,11,3,18,13,20,7,9,22,5,2,24,10,17,21,4,16,23)

# grafico dos residuos versus ordem de coleta:
dados_ordem<-data.frame(eij,ordem)
p <- ggplot(dados_ordem, aes(x=ordem, y=eij)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=0) +
  theme_ipsum()
p

##################################
# Residuos vs. valores ajustados:
##################################
plot(modelo,1)

##################################################
# Testes de Bartlett e Levene para igualdade de variancias:
##################################################
bartlett.test(resistencia~perc_algodao, dados) #assume normalidade
leveneTest(resistencia~perc_algodao, dados) #nao assume normalidade

