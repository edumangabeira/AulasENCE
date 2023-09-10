options(digits=14)

metodo_jacobi <- function(a, b, x_inicial, precisao=0.01){
    solucao <- c()
    iteracao_atual <- 1
    while(TRUE){
        
        for(linha in 1:nrow(a)) {
            soma <-  0 
            for(coluna in 1:ncol(a)){
                if (linha != coluna){
                    soma <- soma + (a[linha, coluna]*x_inicial[coluna])/a[linha,linha]
                }
            }
            solucao[linha] <- (b[linha]/a[linha,linha]) - soma
        }
        
        print(solucao)
        print(paste0(c("Iteração ", as.character(iteracao_atual))))
        
        if(max(abs(solucao - x_inicial)) <= precisao){
            break
        }
        iteracao_atual <- iteracao_atual + 1
        x_inicial <- solucao
    }
    
    print("A solução aproximada é:")
    print(solucao)
}

# Para o método de Gauss-Seidel, usarei decomposição matricial.
decompoe_A <- function(matriz_A){
    tamanho_coluna <- ncol(matriz_A)
    D <- matrix(rep(0, I(tamanho_coluna*tamanho_coluna)),ncol=tamanho_coluna)
    diag(D) <- diag(matriz_A)
    L <- matriz_A
    U <- matriz_A
    L[lower.tri(matriz_A, diag=F)==F] <- 0
    U[upper.tri(matriz_A, diag=F)==F] <- 0

    return(list(D=D, L=L, U=U))
}


# método de Gauss-Seidel
metodo_seidel=function(a, b, x_inicial, precisao=0.01){
    solucao <- c()
    iteracao_atual <-  1
    tamanho_b <- length(b)
    matriz_decomposta <- decompoe_A(a)
    D <- matriz_decomposta$D
    L <- matriz_decomposta$L
    U <- matriz_decomposta$U
    solucao_parcial <- matrix(rep(0, I(tamanho_b*tamanho_b)),ncol=tamanho_b)
    transposta_L_D <- t(L+D)
    while(TRUE){
        
        for (i in tamanho_b:1) {
            solucao_parcial[i,i] <- 1/transposta_L_D[i,i]
            if(i < tamanho_b){
                for (j in I(i+1):tamanho_b){
                    ld_num <- transposta_L_D[i, I(i+1):j]
                    ld_denom <- transposta_L_D[i,i]
                    solucao_parcial[i,j] <- -sum(solucao_parcial[I(i+1):j,j]*ld_num)/ld_denom
                }
            }
        }
        identidade_L_D <- t(solucao_parcial)
        solucao <-  -identidade_L_D%*%U%*%x_inicial+identidade_L_D%*%b
        print(solucao)
        print(paste0(c("Iteração ", as.character(iteracao_atual))))
        
        if(max(abs(solucao - x_inicial)) <= precisao){
            break
        }
        iteracao_atual <- iteracao_atual + 1
        x_inicial <- solucao
    }
    print("A solução aproximada é:")
    print(solucao)
}

x_inicial <- c(1, 1, 1, 1)
precisao <- 10^(-12)

matriz_A <- matrix(
    c(2, -1, 0, 0, -1, 4, -1, 0, 0, -1, 4, -1, 0, 0, -1, 2), 
    nrow=4, 
    byrow=TRUE)
b <- c(3, 5, -15, 7)
metodo_jacobi(matriz_A, b, x_inicial, precisao)
metodo_seidel(matriz_A, b, x_inicial, precisao)







