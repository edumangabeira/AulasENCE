newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
    x <- x0
    fx <- ftn(x)
    iter <- 0
    
    while ((abs(fx[1]) > tol) && (iter < max.iter)) {
        x <- x - fx[1]/fx[2]
        fx <- ftn(x)
        iter <- iter + 1
        cat("Na iteração", iter, "o valor de x foi de", x, "\n")
    }
    # A saida depende do que acontece
    if (abs(fx[1]) > tol) {
        cat("O algoritimo falhou em convergir\n")
        return(NULL)
    } else {
        cat("O algoritimo convergiu\n")
        return(x)
    }
}



func <- function() {
    x=c(50,35,35,40,55,65,35,60,90,35,90,80,60,60,60,40,55,50,65,50)
    y=c(53,41,61,56,68,36,11,70,79,59,54,91,48,71,71,47,53,68,57,79)
    sx = sum(x)S
    sy = sum(y)
    sxx = sum(x^2)
    syy = sum(y^2)
    sxy = sum(x*y)
    model = lm(y~x)
    b0 = coefficients(model)[0]
    b1 = coefficients(model)[1]
    n = length(20)
    return(-(n/2)*log(2*pi*sigma^2)-(1/(2*sigma^2))*(syy - 2*b0*sx - 2*b0*sy-2*b1*sxy+n*(b0^2) +
        2*b0*b1*sx+(b1^2)*sxx))
}

newtonraphson(func, 0)
