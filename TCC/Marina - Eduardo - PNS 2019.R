rm(list = ls())
setwd("C:\\Users\\bhypo\\BETH\\ENCE\\TCC\\Orienta��es TCC\\TCC Marina e Eduardo")
getwd()

#############################################################################################
#leitura dados com pacote PNSIBGE

#install.packages("PNSIBGE")
#install.packages("pracma")
#library(PNSIBGE)
library(survey)
library(dplyr)
library(questionr)
library(pracma)

#pns2019 <- get_pns(year=2019, selected=FALSE, anthropometry=FALSE, labels=TRUE, deflator=TRUE, design=TRUE, savedir=tempdir())
#save(pns2019, file="pns2019.Rdata")

load("pns2019.Rdata")

#Vari�veis MLG
#S115 Qual foi o tipo de parto? 
#V0001 Unidade da Federa��o(transformar em Regi�es) - Escolher estado do RJ por conta das varia��es entre regi�es
#V0026		Tipo de situa��o censit�ria
#V0031		Tipo de �rea
#C009 Cor ou ra�a
#C008 Idade (calculado pelo DMC)
#C011 Estado civil
#VDD004A	N�vel de instru��o mais elevado alcan�ado
#VDE001		Condi��o em rela��o � for�a de trabalho na semana de refer�ncia para pessoas de 14 anos ou mais de idade
#VDE002		Condi��o de ocupa��o na semana de refer�ncia para pessoas de 14 anos ou mais de idade
#VDF003		Rendimento domiciliar per capita (exclusive o rendimento das pessoas cuja condi��o na unidade domiciliar era pensionista, empregado dom�stico ou parente do empregado dom�stico)
#VDF004		Faixa de rendimento domiciliar per capita (exclusive o rendimento das pessoas cuja condi��o na unidade domiciliar era pensionista, empregado dom�stico ou parente do empregado dom�stico)
#S066 Quantos partos a Sra j� teve?
#S068 Quando estava gr�vida fez alguma consulta de pr�-natal?
#S111. Quem fez o parto? (Recodificar - M�dico e outros) - Tabela de contig�ncia vai ajudar a saber se essa vari�vel entra
#S112. Onde foi realizado o parto? (Recodificar - Lugar recomendado e outros) - Tabela de contig�ncia vai ajudar a saber se essa vari�vel entra
#S114. O parto foi feito atrav�s do Sistema �nico de Sa�de (SUS)?
#S1181. Quantas semanas de gravidez tinha no momento do parto?
#S125. Teve alguma complica��o durante o parto?
#S128. O parto foi realizado no estabelecimento de sa�de indicado no pr�-natal?
#S129. Quantos servi�os de sa�de procurou quando entrou em trabalho de parto para que seu(sua) filho(a) pudesse nascer?

#R032. Nos �ltimos doze meses, a Sra participou de grupo de planejamento familiar?
#R37. A Sra e/ou seu companheiro j� fizeram ou fazem algum tratamento para engravidar?
#S70. Quantas consultas de pr�-natal fez durante esta gravidez? 
#S73. As consultas do pr� natal foram feitas atrav�s do Sistema �nico de Sa�de (SUS)? 
#S74. Nesta gravidez, quem a atendeu na maioria das consultas?(S� serve para quem recebeu atendimento pr�-natal)
#S80. Durante o pr�-natal de ___ foi realizado teste / exame para s�filis?

#S117	Qual o principal motivo de ter tido parto ces�reo? 

#############################################################################################
#motico cesaria 

tab0.0=svymean(~S115, subset(pns2019,!is.na(S068)), na.rm=TRUE, vartype="cv")
tab0.1=svymean(~S114, subset(pns2019,!is.na(S068)), na.rm=TRUE, vartype="cv")
tab0.2=svyby(~S115, ~S114, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svymean, vartype="cv")

tab0.3=svymean(~S117, subset(pns2019,!is.na(S068)), na.rm=TRUE, vartype="cv")
tab0.4=svyby(~S117, ~S114, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svymean, vartype="cv")
tab0.5=svyby(~S114, ~S117, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svymean, vartype="cv")


#############################################################################################
#ajustes nas vari�veis 

table(pns2019$variables$S115)
sum(!is.na(pns2019$variables$S115))
table(pns2019$variables$S066)
sum(!is.na(pns2019$variables$S066))
table(pns2019$variables$S068)
prop.table(table(pns2019$variables$S068))
sum(!is.na(pns2019$variables$S068))
str(pns2019$variables$C008)
str(pns2019$variables$S066)
table(pns2019$variables$S068)
table(pns2019$variables$S068)
table(pns2019$variables$S068)
table(pns2019$variables$C009,useNA= "ifany")

pns2019$variables$nS115=as.integer(ifelse(is.na(pns2019$variables$S115), NA,
                                          ifelse(pns2019$variables$S115 %in% "Cesariana", 1, 0)))              
pns2019$variables$regiao=ifelse(pns2019$variables$V0001 %in% c("Acre","Amazonas","Roraima","Par�","Amap�","Tocantins"),"N",
                                ifelse(pns2019$variables$V0001 %in% c("Maranh�o","Piau�","Cear�","Rio Grande do Norte","Para�ba","Pernambuco","Alagoas","Sergipe","Bahia"),"NE",
                                       ifelse(pns2019$variables$V0001 %in% c("Minas Gerais","Esp�rito Santo","Rio de Janeiro","S�o Paulo"),"SE",
                                              ifelse(pns2019$variables$V0001 %in% c("Paran�","Santa Catarina","Rio Grande do Sul"), "S","CO"))))

pns2019$variables$cor=ifelse(pns2019$variables$C009 %in% c("Preta","Amarela","Parda", "Ind�gena"), "N�o Branca", 
                             ifelse(pns2019$variables$C009 %in% "Branca", "Branca", "Ignorado"))
table(pns2019$variables$cor,useNA= "ifany")
levels(pns2019$variables$S114)
pns2019$variables$S114=droplevels(pns2019$variables$S114)
levels(pns2019$variables$S114)
#############################################################
#descritivas
#############################################################

(tab0=svymean(~S115, subset(pns2019,!is.na(S068)), na.rm=TRUE))
#coef(tab0)
#100*cv(tab0)

(tab1.1=svyby(formula=~S115, by=~regiao, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svytotal , vartype="cv"))
(tab1.2=svyby(formula=~S115, by=~regiao, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svymean , vartype="cv"))

(tab2.1=svyby(formula=~S115, by=~C009, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svytotal , vartype="cv"))
(tab2.2=svyby(formula=~S115, by=~C009, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svymean , vartype="cv"))

(tab3.1=svyby(formula=~S115, by=~C011, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svytotal , vartype="cv"))
(tab3.2=svyby(formula=~S115, by=~C011, subset(pns2019,!is.na(S068)), na.rm=TRUE, FUN=svymean , vartype="cv"))

#############################################################
#modelos
#############################################################

#Calcula odds ratio e raz�o de preval�ncia
#https://rpubs.com/kaz_yos/poisson

svyglm.RR <- function(SVYGLM.RESULT, digits = 2) {
  if (SVYGLM.RESULT$family$family == "quasibinomial") {
    LABEL <- "OR"
  } else if (SVYGLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  COEF      <- stats::coef(SVYGLM.RESULT)
  CONFINT   <- stats::confint(SVYGLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}
# ATEN��O: verificar posteriormente se a fun��o svyglm.RR gera o resultado correto

mod1.1=svyglm(nS115~regiao, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.1)

mod1.2=svyglm(nS115~regiao+V0026, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.2)
anova(mod1.1,mod1.2, test = "Chisq")
AIC(mod1.1,mod1.2)

mod1.3=svyglm(nS115~regiao+V0031, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.3)
anova(mod1.1,mod1.3, test = "Chisq")
AIC(mod1.1,mod1.3)

mod1.4=svyglm(nS115~regiao+cor, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.4)
anova(mod1.1,mod1.4, test = "Chisq")
AIC(mod1.1,mod1.4)

mod1.5=svyglm(nS115~regiao+cor+C011, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.5)
anova(mod1.4,mod1.5, test = "Chisq")

mod1.6=svyglm(nS115~regiao+C011, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.6)
anova(mod1.1,mod1.6, test = "Chisq")
AIC(mod1.1,mod1.5,mod1.6)

mod1.7=svyglm(nS115~regiao+S114, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.7)
anova(mod1.1,mod1.7, test = "Chisq")

mod1.8=svyglm(nS115~regiao+VDD004A, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod1.8)

mod2=svyglm(nS115~regiao+cor+C011+S114,subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod)

mod2.1=svyglm(nS115~regiao+cor+C011, subset(pns2019,(!is.na(S068)&!is.na(S114)&(S114 %in% "N�o"))),family=quasibinomial)
summary(mod2.1)

mod2.2=svyglm(nS115~regiao+cor+C011, subset(pns2019,(!is.na(S068)&!is.na(S114)&(S114 %in% "Sim"))),family=quasibinomial)
summary(mod2.2)
svyglm.RR(mod2.2)

mod2.3=svyglm(nS115~regiao+cor+C011+S114+cor*S114, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod2.3)
svyglm.RR(mod2.3)

mod2.4=svyglm(nS115~regiao+cor+C011+S114+regiao*S114, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod2.4)

mod2.5=svyglm(nS115~regiao+cor+C011+S114+C011*S114, subset(pns2019,!is.na(S068)),family=quasibinomial)
summary(mod2.5)


#install.packages("interactions")
library("interactions")
cat_plot(mod2.3, pred = cor, modx = S114, interval = TRUE)
cat_plot(mod2.3, pred = regiao, modx = S114, interval = TRUE)
