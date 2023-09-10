# ------------------------------ pacotes ---------------------------------------
library(readxl)
library(dplyr)
library(magrittr)
library(clipr)

# ----------------------------- leitura -----------------------------------------

Ocupacao<- read_csv("ocupacao_amostra.csv")
totais_amostrais <- read_csv("ocup_tot_pop.csv")
totais_populacionais <- read_csv("ocup_tot_amostra.csv")


# ------------------------------- questão 1 --------------------------------------------

# amostra inicial 
n <- 7427

# amostra de pessoas entrevistadas
m <- 5567 

# taxa de resposta 
tr <- m/n

# tamanho da população
N<-3206624

# peso amostral básico
Dados <- Ocupacao
Dados$di_1 <- N/n

# probabilidade de inclusao da unidade i na amostra s
Dados$pi_si_1 <- 1/Dados$di_1

# Criação de classes ou pós-estratos
Dados <- Dados %>% 
  mutate(classe = paste(sexo,gr_idade,sep = ' - '))

# Taxas de resposta nas classes (considerando somente a amostra de respondentes)
Dados$phi_hat_c_1 <- 1

# Peso ajustado por não-resposta (na realidade não há ajuste)
Dados$wi_1 <-Dados$di_1/Dados$phi_hat_c_1

# Estimativas dos totais de pessoas ocupadas, desocupadas e inativas por sexo e grupos de idade (C)
Q1C <- trunc(table(Dados$classe,Dados$situ_ocup)*Dados$wi_1[1]) # mesmo peso p/ todas as und


# Estimativas das porcentagens de pessoas ocupadas, desocupadas e inativas por sexo e grupos de idade (A)

total_linha_Q1A <- apply(as.matrix(trunc(table(Dados$classe,Dados$situ_ocup)*Dados$wi_1[1])),1,sum)
Q1A <- round((trunc(table(Dados$classe,Dados$situ_ocup)*Dados$wi_1[1])/total_linha_Q1A)*100,2) 


# Estimativas das taxas de desocupação (100 * desocupadas / [desocupadas +ocupadas]) nos mesmos domínios (B)

Q1B <- Dados %>% 
  select(classe,situ_ocup) %>% 
  group_by(classe) %>% 
  summarise(ocupados     = trunc(sum(situ_ocup == 'Ocupada')*(N/n)),
            desocupados  = trunc(sum(situ_ocup == 'Desocupada')*(N/n)),
            inativos     = trunc(sum(situ_ocup == 'Inativa')*(N/n))) %>% 
  mutate(Taxa_desocupacao  = 100*round(desocupados/(ocupados+desocupados),4))


# Q1B_V2 <- round(as.matrix(Q1C)[,1]*100 / apply(as.matrix(Q1C)[,c(1,3)],1,sum),2)

# write_clip(Q1A,dec=",")
# write_clip(Q1B,dec=",")
# write_clip(Q1C,dec=",")

# ------------------------------- questão 2 ------------------------------------

Peso <- N/n
Estimativas_populacionais <-
  Dados %>%
  select(classe) %>%
  group_by(classe) %>%
  mutate(N = trunc(n()*Peso)) %>%
  unique()


df_ponderacao_nao_resposta = left_join(
  x = Dados %>%
    select(classe) %>%
    group_by(classe) %>%
    mutate(N_respondentes = n()) %>%
    unique() %>%
    ungroup(),

  y = data.frame(classe   = Estimativas_populacionais$classe,
                 N_amostral = c(977,969,1812,2051,716,902)),

  by = 'classe') %>%
  mutate(vi = 1/round(N_respondentes/N_amostral, 4),
         wi = Peso*vi ) %>%
  arrange(classe)


# Est_pop <- trunc(table(Dados$classe)*(N/n))
# df_ponderacao_nao_resposta <- merge (x=data.frame(table(Dados$classe)),
#                                      y=data.frame(Var1=data.frame(table(Dados$classe))$Var1,sc = c(977,1812,716,969,2051,902)),
#                                      by= "Var1")
# df_ponderacao_nao_resposta$phi_hat_c_2 <- df_ponderacao_nao_resposta$Freq/df_ponderacao_nao_resposta$sc
# df_ponderacao_nao_resposta$wi_2 <- (N/n)/df_ponderacao_nao_resposta$phi_hat_c_2 





# Estimativas dos totais de pessoas ocupadas, desocupadas e inativas por sexo e grupos de idade (C)
Q2C <- trunc(table(Dados$classe,Dados$situ_ocup)*df_ponderacao_nao_resposta$wi) 
write_clip(Q2C,dec=",")

# Estimativas das porcentagens de pessoas ocupadas, desocupadas e inativas por sexo e grupos de idade (A)

total_linha_Q2A <- apply(as.matrix(trunc(table(Dados$classe,Dados$situ_ocup)*df_ponderacao_nao_resposta$wi)),1,sum)
Q2A <- round((trunc(table(Dados$classe,Dados$situ_ocup)*df_ponderacao_nao_resposta$wi) / total_linha_Q2A)*100,2)
write_clip(Q2A,dec=",")

# Estimativas das taxas de desocupação (100 * desocupadas / [desocupadas +ocupadas]) nos mesmos domínios (B)
Q2B <- Dados %>% 
  select(classe,situ_ocup) %>% 
  left_join(y=df_ponderacao_nao_resposta[,c(1,5)], by="classe") %>% 
  group_by(classe) %>% 
  summarise(ocupados     = trunc(sum(situ_ocup == 'Ocupada')*wi),
            desocupados  = trunc(sum(situ_ocup == 'Desocupada')*wi),
            inativos     = trunc(sum(situ_ocup == 'Inativa')*wi)) %>%
  mutate(Taxa_desocupacao  = 100*round(desocupados/(ocupados+desocupados),4)) %>%
  unique()

write_clip(Q2B,dec=",")


# ------------------------------- questão 3 ------------------------------------

colnames(Estimativas_populacionais)[2] <- "total_est"

df_ponderacao_populacional = left_join(
  x = Estimativas_populacionais,
  y = data.frame(classe   = unique(Estimativas_populacionais$classe),
                 N_Populacional = c(492052,491109,781878,865075,246906,329604)),
  by = 'classe'
) %>% 
  mutate(vi = round(N_Populacional/total_est, 4),
         di = N/n,
         wi = di*vi ) %>%
  arrange(classe)
names(df_ponderacao_populacional) = c('classe','N_estimado',
                                      'N_populacional','vi','di','wi')


# Estimativas dos totais de pessoas ocupadas, desocupadas e inativas por sexo e grupos de idade (C)
Q3C <- trunc(table(Dados$classe,Dados$situ_ocup)*df_ponderacao_populacional$wi) 
write_clip(Q3C,dec=",")

# Estimativas das porcentagens de pessoas ocupadas, desocupadas e inativas por sexo e grupos de idade (A)

total_linha_Q3A <- apply(as.matrix(trunc(table(Dados$classe,Dados$situ_ocup)*df_ponderacao_populacional$wi)),1,sum)
Q3A <- round((trunc(table(Dados$classe,Dados$situ_ocup)*df_ponderacao_populacional$wi) / total_linha_Q3A)*100,2)
write_clip(Q3A,dec=",")



# Estimativas das taxas de desocupação (100 * desocupadas / [desocupadas +ocupadas]) nos mesmos domínios (B)
Q3B <- Dados %>% 
  select(classe,situ_ocup) %>% 
  left_join(y=df_ponderacao_populacional[,c(1,6)], by="classe") %>% 
  group_by(classe) %>% 
  summarise(ocupados     = trunc(sum(situ_ocup == 'Ocupada')*wi),
            desocupados  = trunc(sum(situ_ocup == 'Desocupada')*wi),
            inativos     = trunc(sum(situ_ocup == 'Inativa')*wi)) %>%
  mutate(Taxa_desocupacao  = 100*round(desocupados/(ocupados+desocupados),4)) %>%
  unique()

write_clip(Q3B,dec=",")


