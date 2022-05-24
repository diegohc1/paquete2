
#' Tabla de frecuencias (Frequency table)
#'
#' Lo mismo que table() pero devuelve un data frame y acepta pesos (Same as table() but returns a dataframe and accepts weights)
#'
#' @param data data frame
#' @param x variable categórica (categorical variable)
#' @param peso en caso hayan pesos (if there are weights)
#'
#' @return data frame con la cantidad de casos (n) y porcentaje (prop)
#' @export
#'
#' @examples
#' tabla_freq(mtcars, "gear")
#'
#'

tabla_freq <- function(data, x, peso = NULL){

  if(!is.null(peso)){
    f <- paste0(peso, "~", x)
    df1 <- aggregate(formula = as.formula(f), data = data, FUN = sum)
    names(df1)[names(df1) == peso] <- "Freq"
  }else{
    df1 <- as.data.frame(table(data[x]))
  }

  df2 <- within(df1, {prop = round(prop.table(Freq)*100, 1)})
  names(df2) <- c("opcion", "n", "prop")

  return(df2)

}




