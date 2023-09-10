# Eduardo Freire Mangabeira
# 202010111-40

library(readr)

# QUESTÃO 1
# a função retorna uma lista com os coeficientes do modelo, 
# a matriz de informação e os vetores x e y.
metodo_escores <- function(
    dados,
    explicativas,
    num_coeficientes,
    tolerancia=10^(-11), 
    max_iter=1000
){
    y <- dados[,2]
    N <- length(y)
    x <- cbind(rep(1, N), explicativas)
    x <- as.matrix(x)
    colnames(x)[1] <- "Bo"
    b_atual <- matrix(c(1:num_coeficientes), num_coeficientes, 1)
    iteracoes <- 0
    
    criterio_parada <- 1000
    
    while(criterio_parada > tolerancia & iteracoes < max_iter){
        iteracoes <- iteracoes + 1
        b_anterior <- b_atual 
        eta <- x%*%b_anterior
        esperanca <- eta
        derivada_eta_esperanca <- rep(1, N)
        variancia_y <- esperanca^2
        w <- diag(c((1/derivada_eta_esperanca)^2/variancia_y))
        z <- eta + ((y-esperanca)*derivada_eta_esperanca)
        b_atual <- solve(t(x)%*%w%*%x)%*%t(x)%*%w%*%z
        criterio_parada <- sum(((b_atual - b_anterior)/b_anterior)^2)
        matriz_informacao <- t(x)%*%w%*%x
    }
    
    return(list(b_atual, matriz_informacao, y, x))
}

dados <- read.csv("dados.txt")


modelo1 <- metodo_escores(
    dados=dados,
    explicativas=dados[,-c(1,2)],
    num_coeficientes=ncol(dados[,-c(1,2)])+1,
)

coeficientes1 <- modelo1[[1]]
coeficientes1


# Com os coeficientes obtidos, o modelo ajustado é:
# y = -5.489 + 0.4092*x1 + 0.0005*x2 + 0.11594*x3 - 0.5435*x4 + 0.4666*x5 - 0.2027*x6

# ---------------------------------------------------------------------

# QUESTÃO 2

matriz_informacao <- modelo1[[2]]
raiz_inversa_informacao <- sqrt(diag(solve(matriz_informacao)))

indice <- 1
cat("------------------------------------\n")
cat("INTERVALOS DE CONFIANÇA\n")

for(coeficiente in coeficientes1){
    lim_inf <- coeficiente-1.96*raiz_inversa_informacao[indice]
    lim_sup <- coeficiente+1.96*raiz_inversa_informacao[indice]
    cat("Beta", indice, "[",lim_inf,",", lim_sup, "]", "\n",sep=" ")
    indice <- indice + 1
}

cat("------------------------------------\n")


# Observando os resultados dos intervalos de confiança pode-se confirmar que
# apenas x1(DOMIC) possui coeficiente significativo, pois o intervalo não contém 0.

# ---------------------------------------------------------------------
# QUESTÃO 3

# o procedimento é o mesmo da questão 1
modelo2 <- metodo_escores(
    dados=dados,
    explicativas=dados[,3],
    num_coeficientes=2,
)

coeficientes2 <- modelo2[[1]]
coeficientes2
# Com os coeficientes obtidos, o modelo ajustado é:
# y = 0.3916130 + 0.4155071*x1


# ---------------------------------------------------------------------
# QUESTÃO 4

# modelo 1
y1 <- modelo1[[3]]
x1 <- modelo1[[4]]
deviance1 <- 2*(-sum(log(y1))-40+ sum(log(x1%*%coeficientes1))+ sum(y1/(x1%*%coeficientes1)))

# modelo 2
y2 <- modelo2[[3]]
x2 <- modelo2[[4]]
deviance2 <- 2*(-sum(log(y2))-40+ sum(log(x2%*%coeficientes2))+ sum(y2/(x2%*%coeficientes2)))

c(deviance1, deviance2)

# ---------------------------------------------------------------------
# QUESTÃO 5

# Ho: O modelo de interesse é tão bom quanto o maximal.
# H1: O modelo de interesse não é tão bom quanto o maximal.

diferenca <- deviance2 - deviance1

test <- 1 - pchisq(diferenca, 6-1)
teste

# Ao nível de 5% de significancia não há evidencias para
# rejeitar que o  modelo de interesse é tão bom quanto o maximal.
