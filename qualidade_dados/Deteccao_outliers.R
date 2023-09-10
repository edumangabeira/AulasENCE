######################################################################

# Detecção de Outliers Univariados
# Pacote univOutl para outliers univariados

######################################################################

# Instalando o pacote univOutl
install.packages("univOutl", dependencies = TRUE)
library(univOutl)

# Leitura de dados com presença de outliers
dados = readRDS("dados_outliers.RDS")

histograma = hist(dados$y)


########################################################
# Método da distância relativa
########################################################

media = mean(dados$y)
desvio = sd(dados$y)


di = Mod((dados$y - media))/desvio

Cl = 1.5
Li = media - Cl*desvio
Ls = media + Cl*desvio

out_relativo_i = di[di < Ls]
out_relativo_s = di[di > Ls]

out_relativo_i
out_relativo_s

# Detecção de outliers baseados na média não são a melhor escolha

########################################################
# Método usando medidas robustas de localização e escala
########################################################

# [Q2 - k*sL; Q2 + k*sR]
# Usa a mediana como medida robusta de localização

#LocScaleB(x, k=3, method='MAD', weights=NULL, id=NULL, exclude=NA, logt=FALSE, return.dataframe=FALSE)

#method='IQR' Q3-Q1
#method='IDR' P90-P10
#method='MAD' 
#method='Gini' (seeGiniMd)
#method='ScaleTau2' Maronna and Zamar (2002) (see alsoscaleTau2);
#method='Qn' for using the Qn estimator proposed by Rousseeuw and Croux (1993) (see also Qn);
#method='Sn' for using the Sn

metodo_MAD = LocScaleB(dados$y, k=3, method='MAD', weights=NULL, id=NULL, exclude=NA, logt=FALSE, return.dataframe=TRUE)
metodo_MAD
outliers_MAD = dados[metodo_MAD$data$outliers==1,]
outliers_MAD

metodo_IQR = LocScaleB(dados$y, k=3, method='IQR', weights=NULL, id=NULL, exclude=NA, logt=FALSE, return.dataframe=TRUE)


metodo_IDR = LocScaleB(dados$y, k=3, method='IDR', weights=NULL, id=NULL, exclude=NA, logt=FALSE, return.dataframe=TRUE)



########################################################
# Método Boxplot
########################################################

# boxB(dados, k=1.5, method='asymmetric', weights=NULL, id=NULL, exclude=NA, logt=FALSE)  
# boxB(dados, k=1.5, method='adjbox', weights=NULL, id=NULL, exclude=NA, logt=FALSE)  


# "asymmetric" : [Q_1 - 2k * (Q_2-Q_1); Q_3 + 2k * (Q_3-Q_2)]
# "adjbox"- Hubert and Vandervieren (2008) : [Q_1-1.5 * e^{aM} * IQR; Q_3+1.5 * e^{bM}* IQR ]


metodo_boxplot = boxB(dados$y, k=1.5, method='resistant', weights=NULL, id=NULL, exclude=NA, logt=FALSE)

metodo_boxplot
metodo_boxplot$outliers
outliers_box = dados[metodo_boxplot$outliers,]
outliers_box

box = boxplot(dados$y)



