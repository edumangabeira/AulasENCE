# Eduardo Freire Mangabeira
# 202010111-40

# Q1

amostra_aleatoria <- c(0.2144656,0.5626917,0.1085607,0.3227139,0.1823825,0.4169636,0.5286144,
                       
                       0.3750994,0.2580999,0.2729045,0.4124994,0.1923301,0.2482222,0.1554759,0.4615366,
                       
                       0.1278881,0.4310167,0.2878643,0.3466267,0.5031901,0.3029934,0.1099048,0.3605925,
                       
                       0.3549951,0.3175219,0.2681822,0.2694517,0.3603303,0.3252978,0.2194104)

n <- length(amostra_aleatoria)


matriz_hessiana <- function(alpha, beta){
    matrix(c(trigamma(alpha+beta)*n-trigamma(alpha)*n,
             trigamma(alpha+beta)*n,
             trigamma(alpha+beta)*n,
             trigamma(alpha+beta)*n-trigamma(beta)*n),
           byrow=T,
           ncol=2
    )
}


derivadas_gamma <- function(alpha, beta){
    matrix(c(sum(log(amostra_aleatoria))-digamma(alpha)*n+digamma(alpha+beta)*n,
             sum(log(1-amostra_aleatoria))-digamma(beta)*n+digamma(alpha+beta)*n),
           ncol=1)
}


newton_raphson <- function(x_0, norma, tolerancia, max_iteracoes){
    iteracao <- 0
    while(norma > tolerancia & iteracao < max_iteracoes){
        x_alpha <- x_0[1,1]
        x_beta <- x_0[2,1]
        x_novo <- x_0-solve(matriz_hessiana(x_alpha, x_beta))%*%
            derivadas_gamma(x_alpha, x_beta)
        norma <- norm(((x_novo-x_0)/x_0))
        x_0 <- x_novo
        iteracao <- iteracao + 1
    }
    return(list(alpha=x_alpha, beta=x_beta))
}

x_0 <- matrix(c(1,1),ncol=1)
tolerancia <- 10^(-11)
norma <- 1
max_iteracoes <- 450
emvs <- newton_raphson(x_0, norma, tolerancia, max_iteracoes)
emv_alpha <- emvs["alpha"]
emv_beta <- emvs["beta"]


# EMV alpha = 4.39407897360226
print(paste0("EMV alfa: ", emv_alpha))
# EMV beta = 9.80366326116931
print(paste0("EMV beta: ", emv_beta))
