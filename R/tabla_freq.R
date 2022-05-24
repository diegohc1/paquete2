
#' tabla freq
#'
#' Lo mismo que table() pero devuelve un dataframe y acepta pesos
#'
#' @param data Dataframe
#' @param x Variable categórica
#' @param peso En caso hayan pesos
#'
#' @return Dataframe
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




