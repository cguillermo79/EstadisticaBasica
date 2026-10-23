# Función para calcular estadísticas descriptivas básicas
#' Estadísticas descriptivas básicas
#'
#' Calcula la media, mediana y desviación estándar
#' de un vector numérico.
#'
#' @param A Vector numérico.
#' @return Vector con la media, mediana y desviación estándar.
#' @export
#'
#' @examples
#' est_basica(c(10, 12, 15, 18, 20))
#'
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
