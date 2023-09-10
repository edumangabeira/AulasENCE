peso=c("31","29","28","30","31","31","24","45","41","37","34","43")
marca=sort(rep(c("Yoki","Chinezinho"),6),decreasing = TRUE)
sabor=rep(sort(rep(c("Natural","Manteiga","Bacon"),2),decreasing = TRUE),2)
dados=cbind(peso,marca,sabor)
colnames(dados)=c("peso","marca","sabor")

dados = as.data.frame(dados)
dados$peso = as.numeric(dados$peso)
dados$marca = as.factor(dados$marca)
dados$sabor = as.factor(dados$sabor)


library(ggpubr)
ggline(dados, x = "sabor", y = "peso",
       add = c("mean_se", "jitter"),
       ylab = "Peso", xlab = "Sabor")

ggline(dados, x = "marca", y = "peso",
       add = c("mean_se", "jitter"),
       ylab = "Peso", xlab = "Marca")

ggboxplot(dados, x = "sabor", y = "peso", color = "marca", ylab = "Peso", xlab = "Sabor")

interaction.plot(x.factor=marca,trace.factor=sabor,response=peso,fun = mean,
                 col=1:3,fixed=T)

model=aov(peso~sabor+marca)
summary(model)
model=aov(peso~marca)
summary(model)

summary.lm(model) 

TukeyHSD(model)

plot(model,1)
plot(model,2)

library(lmtest)
bptest(model)

shapiro.test(model$residuals)

peso.media=mean(dados$peso)
peso.sd=sqrt(var(dados$peso))

sabor.medias=with(dados, tapply(peso,sabor,mean))
marca.medias=with(dados, tapply(peso,marca,mean))
model.tables(model)

require(pwr2)

sabor.desvios=sqrt(with(dados, tapply(peso,sabor,var)))
marca.desvios=sqrt(with(dados, tapply(peso,marca,var)))

pwr.2way(a=length(unique(dados$sabor)),
         b=length(unique(dados$marca)),
         alpha=0.05,
         sigma.A=sabor.desvios,
         sigma.B=marca.desvios,
         size.A=2,
         size.B=4,
         delta.A=21,
         delta.B=21)

n<-seq(2,18,1)
poder<-vector(length=length(n))
for (i in 1:length(n)){
  poder[i]<-pwr.2way(a=length(unique(dados$sabor)),
                     b=length(unique(dados$marca)),
                     alpha=0.1,
                     sigma.A=sabor.desvios,
                     sigma.B=marca.desvios,
                     size.A=n[i],
                     size.B=n[i],
                     delta.A=21,
                     delta.B=21)$power
}
plot(n,poder)

sigma_E<-seq(15,45,5)
poder<-vector(length=length(sigma_E))
for (i in 1:length(sigma_E)){
  poder[i]<-pwr.2way(a=length(unique(dados$sabor)),
                     b=length(unique(dados$marca)),
                     alpha=0.1,
                     sigma.A=sigma_E[i],
                     sigma.B=sigma_E[i],
                     size.A=12,
                     size.B=12,
                     delta.A=21,
                     delta.B=21)$power
}
plot(sigma_E,poder)


pwr.1way(k=2,
         n=length(unique(dados$marca)),
         alpha=0.1,
         delta=21,
         sigma=marca.desvios)

n<-seq(2,18,1)
poder<-vector(length=length(n))
for (i in 1:length(n)){
  poder[i]<-pwr.1way(k=2,
                     n=n[i],
                     alpha=0.1,
                     delta=21,
                     sigma=marca.desvios)$power
}
plot(n,poder)

sigma_E<-seq(15,45,5)
poder<-vector(length=length(sigma_E))
for (i in 1:length(sigma_E)){
  poder[i]<-pwr.1way(k=2,
                     n=length(unique(dados$marca)),
                     alpha=0.1,
                     delta=21,
                     sigma=sigma_E[i])$power
}
plot(sigma_E,poder)
