#' Tabla de frecuencias segun estrato (Frequency table by group)
#'
#' Aplica [tabla_freq()] según estrato
#'
#' @param data data frame
#' @param var variable categórica (categorical variable)
#' @param grupo grupo o estrato
#' @param peso en caso hayan pesos (if there are weights)
#' @param compara útil para comparar los porcentajes según el grupo
#' @param na en caso se consideren valores perdidos ()
#'
#' @return data frame con la cantidad de casos (n) y porcentaje (prop)
#' @export
#'
#' @examples
#' tabla_freq_estrato(mtcars, var = "gear", grupo = "carb")
#'
#'
tabla_freq_estrato <- function(data, var, grupo, peso = NULL, compara = NULL, na = NULL){

  if(length(grupo) == 1){
    names(data)[names(data) == grupo] <- "group_temp"
  }else{
    data$group_temp <- apply(data[, grupo], 1, paste, collapse = "-")
  }

  df2 <- lapply(split(data, data[["group_temp"]]), function(x) tabla_freq(x, var, peso = peso, na = na))
  df3 <- pega_lista(df2, "estrato")

  if(length(grupo) == 1){
    names(df3)[names(df3) == 'estrato'] <- grupo
  }else{
    df3 <- data.frame(df3[1:3], do.call(rbind, strsplit(df3[["estrato"]], "-")))
    names(df3) <- c(names(df3[1:3]), grupo)
  }

  if(length(grupo) == 1 & !is.null(compara)){ #util para comparar los %
    df3 <- reshape(df3[-3], idvar = "opcion", timevar = grupo, direction = "wide")
  }

  return(df3)

}
