# Eduardo Freire Mangabeira
# 202010111-40

# Q2
simpson <- function(f, a, b, n){
    h <- (b-a)/n
    x <- seq(a, b, h)
    soma <- 0
    for (i in 1:n){
        soma <- soma + ((f(x[i]) + f(x[i+1]))/2)*(x[i+1]-x[i])
    }
    return(soma)
}

# E[g(X)] = integral de -inf a inf de f(x)g(x)dx
f <- function(x){
    (exp(-((x-3)^2)/2) + exp((-(x-6)^2)/2))*sqrt(1/(2*pi))*exp(-(x^2)/2)
}
a <- -5
b <-  5
n <-  10000

simpson(f, a, b, n)
# 0.07461554
# O resultado é bem próximo com o obtido em 1, e é igual com 
# pelo menos 6 casas decimais de precisão.


# Q3
set.seed(20210201)

monte_carlo <- function(f, a, b, n, distribution){
    if(distribution == "uniforme"){
        distribution <- runif(n)
        x <- a + (b-a)*distribution
        soma <- f(x)
        return((b-a)*mean(soma))
    }
    
    if(distribution == "normal_padrao"){
        distribution <- gera_normal(n)
        return(mean(f(distribution)))
    }
    
}

monte_carlo(f, a, b, n, 'uniforme')
# 0.07611006
# O Resultado se afasta um pouco do valor da integral obtida em (1), 
# só tem duas casas decimais de precisão correspondentes.


# Q4

# Amostra aleatória da Normal Padrão gerada pelo TCL
gera_normal <- function(n){
    x <- c()
    m <- 100

    for(i in 1:n){
        u <- runif(m)
        x <- c(x, (mean(u) - 0.5)/sqrt(1/(12*m)))
    }
    
    return(x)
}


g <- function(x){
    exp(-((x-3)^2)/2) + exp((-(x-6)^2)/2)
}

monte_carlo(g, a, b, n, 'normal_padrao')
# 0.07493036
# O resultado ficou mais próximo de (1) se comparado a (3).



