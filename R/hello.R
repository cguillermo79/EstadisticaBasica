# Función para calcular estadísticas descriptivas básicas

est_basica <- function(A) {

    if (!is.numeric(A)) {
        stop("El argumento debe ser numérico")
    }

    if (length(A) == 0 || all(is.na(A))) {
        stop("El vector no contiene datos válidos")
    }

    c(
        media = mean(A, na.rm = TRUE),
        mediana = median(A, na.rm = TRUE),
        desviacion = sd(A, na.rm = TRUE)
    )
}
