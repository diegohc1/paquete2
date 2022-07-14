#' Tabla de frecuencias con varias columnas (Frequency table with many columns)
#'
#' Aplica [tabla_freq()] a varias columnas
#'
#' @param data frame
#' @param nomvar vector con el nombre de la variable categórica (categorical variable)
#' @param peso en caso hayan pesos (if there are weights)
#' @param starts TRUE si se quiere aplicar sobre columnas que comparten los mismos carácteres al inicio
#' @param na en caso se consideren valores perdidos ()
#'
#' @return data frame con la cantidad de casos (n) y porcentaje (prop)
#' @export
#'
#' @examples
#' tabla_freq_columnas(iris, "Sepal", starts = TRUE)
#'
tabla_freq_columnas <- function(data, nomvar, peso = NULL, starts = NULL, na = NULL){

  if(!is.null(starts)){
    nom <- names(data[, grep(paste0("^", nomvar), names(data), value = TRUE)])
    names(nom) <- nom
  }else{
    nom <- nomvar
    names(nom) <- nom
  }

  df2 <- lapply(nom, function(x) tabla_freq(data, x, peso = peso, na = na))

  pega_lista(df2, "var")

}
