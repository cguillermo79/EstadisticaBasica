#' Identificar números pares e impares
#'
#' Clasifica los números de un vector en pares e impares
#' y muestra sus cantidades y valores.
#'
#' @param A Vector de números enteros.
#' @return Muestra los resultados en la consola.
#' @export
#'
#' @examples
#' visualizar_par_impar(c(2, 5, 8, 11, 14))

visualizar_par_impar <- function(A) {
    P <- c()
    I <- c()
    k <- 1
    q <- 1

    for (i in 1:length(A)) {
        if (A[i] %% 2 == 0) {
            P[k] <- A[i]
            k <- k + 1
        } else {
            I[q] <- A[i]
            q <- q + 1
        }
    }

    cat("La cantidad de valores pares es: ", k-1, "\n")
    cat("Los valores pares son: \n")
    print(P)

    cat("La cantidad de valores impares es: ", q-1, "\n")
    cat("Los valores impares son: \n")
    print(I)
}
