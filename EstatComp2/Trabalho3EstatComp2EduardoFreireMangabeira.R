# Eduardo Freire Mangabeira

# Método do Trapézio
area_do_trapezio = function(a, b, funcao){
    return(0.5*(b-a)*(funcao(a)+funcao(b)))
}


trapezio <- function(a, b, funcao, intervalo){
    soma <- 0
    x <- seq(a, b, l = intervalo + 1)
    for (i in 1:intervalo){
        soma <- soma + area_do_trapezio(x[i], x[i+1], funcao)
    }
    return(soma)
}

# Método de Simpson
area_simpson = function(a, b, funcao){
    return((b-a)/6*(funcao(a) + funcao(b) + 4*funcao((a+b)/2)))
}


simpson <- function(a, b, funcao, sub){
    soma <- 0
    x <- seq(a, b, l=sub+1)
    for (i in 1:sub){
        soma <- soma +
            area_simpson(x[i], x[i+1], funcao)
    }
    return(soma)
}


erro <- function(teorico, estimado){
    return(abs(teorico-estimado))
}


executa_metodo_de_quadratura <- function(metodo, subintervalo){
    f <- function(x){(1/16)*(x^2)*exp(-0.5*x)}
    valor_teorico <- pgamma(13,3,0.5) - pgamma(8,3,0.5)
    valor_estimado <- metodo(8, 13, f, subintervalo)
    print(valor_estimado)
    print(erro(valor_teorico, valor_estimado))
}

# 3 subintervalos
executa_metodo_de_quadratura(trapezio, 3)
executa_metodo_de_quadratura(simpson, 3)

# 8 subintervalos
executa_metodo_de_quadratura(trapezio, 8)
executa_metodo_de_quadratura(simpson, 8)



# N tal que o erro seja menor que 10^(-8)

calcula_erro_metodo <- function(metodo, tolerancia){
    f <- function(x){(1/16)*(x^2)*exp(-0.5*x)}
    valor_teorico <- pgamma(13,3,0.5) - pgamma(8,3,0.5)
    err <-  1
    n <-  0
    while(err >= tolerancia){
        n <- n + 1
        valor_estimado <- metodo(8, 13, f, n)
        err <- erro(valor_teorico,valor_estimado)
    }
    print(n)
}

calcula_erro_metodo(trapezio, 10^(-8))
calcula_erro_metodo(simpson, 10^(-8))

