algoritmo_EM <- function(dados, tolerancia, erro){
    iteracao <- 0
    dados_1 <- dados[[1]]
    dados_2 <- dados[[2]]
    theta_inicial <- 1 / mean(dados_1)
    N <- length(dados_1) + length(dados_2)
    while((iteracao < 200) & (erro > tolerancia)){
        iteracao <- iteracao + 1
        theta <- N / (sum(dados_1) + sum(dados_2) + (3/theta_inicial))
        erro <- abs(theta - theta_inicial)
        theta_inicial <- theta
    }
    return(theta)
}

dados <- list(
    c(4,12,12,1,3,3,5,2,0,5,1,0,3,13,13,1,0),
    c(4,4,4)
)
tolerancia <- 10^(-11)
erro <- 1000


emv <- algoritmo_EM(dados, tolerancia, erro)
print(paste0("EMV de theta: ", emv))