library(matrixcalc)
matriz_A <- matrix(
    c(
         2,-1, 0,
        -1, 2,-1,
         0,-1, 2
        ),
    ncol = 3,
    byrow = T)

matrixcalc::is.positive.definite(matriz_A)


tamanho_matriz <- ncol(matriz_A)


exibe_matriz_u <- function(matriz_A, tamanho_matriz){
    matriz_u <- diag(0,ncol=tamanho_matriz, nrow = tamanho_matriz)
        for (i in 1:tamanho_matriz) {
            matriz_u[i,i]=(matriz_A[i,i]-sum(matriz_u[1:I(i-1),i]^2))^0.5
            for (j in I(i+1):tamanho_matriz) {
                if(i<tamanho_matriz){
                    matriz_u[i,j]=(matriz_A[i,j]-sum(matriz_u[1:I(i-1),i]*matriz_u[1:I(i-1),j]))/matriz_u[i,i]
                }
            }
        }

    return(matriz_u)

}

exibe_matriz_u(matriz_A, tamanho_matriz)

exibe_decomp_cholesky <- function(matriz_l, matriz_u){
    return(matriz_l %*% matriz_u)
}

matriz_u <- exibe_matriz_u(matriz_A, tamanho_matriz)
matriz_l <- t(matriz_u)

exibe_decomp_cholesky(matriz_l, matriz_u)



vetor_solucao <- c(0,2,1)
tamanho_solucao <- length(vetor_solucao)
x <- rep(0,tamanho_solucao)
z <- rep(0,tamanho_solucao)


i <- 0
j <- 0

for (i in 1:tamanho_matriz) {
    z[i]<- (1/matriz_l[i,i])*(vetor_solucao[i]-sum(matriz_l[i,1:I(i-1)]*z[1:I(i-1)]))
    
    
}

x[tamanho_solucao] <- (1/matriz_u[tamanho_solucao,tamanho_solucao])*(z[tamanho_solucao])
for(j in I(tamanho_solucao-1):1) {
    x[j]<-(1/matriz_u[j,j])*(z[j]-sum(matriz_u[j,I(j+1):tamanho_solucao]*x[I(j+1):tamanho_solucao]))
    
}

z
x
solve(matriz_l)%*%vetor_solucao

solve(matriz_u)%*%z