# Eduardo Freire Mangabeira 202010111-40

set.seed(202010111)
# b)
jack_knife <- function(x, lambda, alpha){
    
    call <- match.call()
    n <- length(x)
    lambda_i <- c(0, n)
    for (i in 1:n) {
        lambda_i[i] <- lambda(x[-i])
    }
    lambda_hat <- lambda(x)
    lambdatil <- n * lambda_hat - (n-1) * lambda_i 
    lambda_jack <- mean(lambdatil)
    
    var_jack <- ((n-1)^2/n) * var(lambda_i)
    var_jack2 <- (1/n) * var(lambdatil)
    dp_jack <- sqrt(var_jack)
    bias_jack <- lambda_hat - lambda_jack
    
    lim_inf <- lambda_jack - qt(1 - (alpha/2), n - 1) * sqrt(var_jack)
    lim_sup <- lambda_jack + qt(1 - (alpha/2), n - 1) * sqrt(var_jack)
    
    return(
        list(
            call         =  call,
            estimativa   = lambda_jack,
            vies         = bias_jack,
            variancia    = var_jack,
            ic           = c(lim_inf,lim_sup)
            )
        )
    
}

amostra <- c(10.216, 8.909, 9.301, 8.858, 2.341, 8.759, 9.740, 0.296, 8.744, 8.458, 17.202, 0.658, 1.537, 1.581, 1.703, 0.897, 4.822, 0.180, 0.535, 23.701, 3.648, 2.491, 0.204, 10.523, 8.568, 7.472, 1.536, 1.539, 1.950, 1.670, 5.761, 1.930, 2.124, 8.401, 4.433, 6.983, 0.677, 29.320, 1.299, 0.077)
envumv <- function(amostra){(length(amostra) - 1)/sum(amostra)}
alpha = 0.05
jack_knife(amostra, envumv, alpha)

# ---------------------------------------------------------------------------
# c)


bootstrap_nao_parametrico <- function(amostra, lambda){
    call <- match.call()
    n <- length(amostra)
    B  <-  200
    b <- 0
    nparthetab <- rep(0,B)
    theta_hat <- lambda(amostra)
    
    set.seed(202101054)
    
    for (b in 1:B){
        i <- trunc(runif(n)*n,0)+1
        nparthetab[b] <- lambda(amostra[i])
    }
    
    nparpont <- mean(nparthetab)
    nparbiasboot <- nparpont-theta_hat
    nparsdboot <- var(nparthetab)
    
    return(
        list(
            call       = call,
            estimativa = nparpont,
            variancia  = nparsdboot,
            vies       = nparbiasboot,
            IC         = quantile(nparthetab, c(0.025, 0.975))
        )
    )
    
}

amostra <- c(10.216, 8.909, 9.301, 8.858, 2.341, 8.759, 9.740, 0.296, 8.744, 8.458, 17.202, 0.658, 1.537, 1.581, 1.703, 0.897, 4.822, 0.180, 0.535, 23.701, 3.648, 2.491, 0.204, 10.523, 8.568, 7.472, 1.536, 1.539, 1.950, 1.670, 5.761, 1.930, 2.124, 8.401, 4.433, 6.983, 0.677, 29.320, 1.299, 0.077)
envumv <- function(amostra){(length(amostra) - 1)/sum(amostra)}
bootstrap_nao_parametrico(amostra, envumv)

# ---------------------------------------------------------------------------
# d)

bootstrap_parametrico <- function(amostra, lambda){
    call <- match.call()
    n <- length(amostra)
    alfa <- 0.05
    B <- 200 
    b <- 0
    theta_hat <- lambda(amostra)
    parthetab <- rep(0,B)
    
    
    for(b in 1:B){
        i <- runif(n,0,theta_hat)
        parthetab[b] <- max(i)
    }
    
    parpont <- mean(parthetab)
    paremp <- ecdf(parthetab)
    parbiasboot <- mean(parthetab)-theta_hat
    parvar <- var(parthetab)
    paric <- quantile(parthetab,c(alfa/2,1-(alfa/2)),type=6)
    
    
    return(
        list(
            call       = call,
            estimativa = parpont,
            variancia  = parvar,
            vies       = parbiasboot,
            IC         = quantile(parthetab, c(alfa/2, 1 - (alfa/2)))
        )
    )
    
}

amostra <- c(10.216, 8.909, 9.301, 8.858, 2.341, 8.759, 9.740, 0.296, 8.744, 8.458, 17.202, 0.658, 1.537, 1.581, 1.703, 0.897, 4.822, 0.180, 0.535, 23.701, 3.648, 2.491, 0.204, 10.523, 8.568, 7.472, 1.536, 1.539, 1.950, 1.670, 5.761, 1.930, 2.124, 8.401, 4.433, 6.983, 0.677, 29.320, 1.299, 0.077)
envumv <- function(amostra){(length(amostra) - 1)/sum(amostra)}
bootstrap_parametrico(amostra, envumv)


# ---------------------------------------------------------------------------
# e)

# Dentre os três, o método Jackknife apresenta o viés mais próximo de zero: 0.001158301.

# O bootstrap possui a menor variância, o que resulta num intervalo de confiança
# bem mais preciso se comparado aos outros métodos.
