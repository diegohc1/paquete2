
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

tabla_freq <- function(data, x, peso = NULL, na = NULL){

  data[[x]] <- ifelse(is.na(data[[x]]), "NA", data[[x]]) # para que lo tome en cuenta en aggregate

  if(!is.null(peso)){

    if(!sum(is.na(data[[peso]])) == 0) stop("La variable de pesos tiene missing")

    v <- tapply(data[[peso]], data[[x]], sum, simplify = TRUE)
    df1 <- data.frame(Var1 = names(v), Freq = v)
    rownames(df1) <- NULL

  }else{
    df1 <- as.data.frame(table(data[x]))
  }

  if(is.null(na)){ #consideramos NA?
    df1 <- df1[which(df1[["Var1"]] != "NA"), ]
  }

  df2 <- within(df1, {prop = round(prop.table(Freq)*100, 1)})
  names(df2) <- c("opcion", "n", "prop")

  return(df2)

}




