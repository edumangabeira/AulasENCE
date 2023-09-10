
fatoracao_L_U <- function(matriz_A){
    l <- matrix(0, nrow(matriz_A), ncol(matriz_A))
    u <- diag(ncol(matriz_A))
    for(i in 1:ncol(matriz_A)){
        for(j in 1:ncol(matriz_A)){
            soma_lu <- 0
            if (i > 1){
                for (k in 1:i-1){
                    soma_lu <- soma_lu + (l[j, k])*(u[k, i])
                }
            }
            l[j,i] <- matriz_A[j,i] - soma_lu
        }
        
        for(w in i+1:ncol(matriz_A)){
            soma_lu <- 0
            if (i > 1){
                for (k in 1:i-1){
                    soma_lu <- soma_lu + (l[i, k])*(u[k, w])
                }
            }
            u[i, w] <- (matriz_A[i, w] - soma_lu)/l[i, i]
        }
    }
    
    return(list(l, u))
}

matriz_A <- matrix(
    c(2, -1, 1, -1, 2, -1, 1, -1, 2), 
    nrow=3, 
    byrow=TRUE)
fatoracao_L_U(matriz_A)

