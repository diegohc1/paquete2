
#' Combina dataframes que están en listas (Combine dataframes that are in lists)
#'
#' Primero agrega una columna al dataframe con el nombre de la lista, despues pega los dataframe
#'
#' @param data lista de dataframes
#' @param nc nombre de la columna que incluirá el nombre de la lista
#'
#' @return data frame
#' @export
#'
#' @examples
#' lista1 <- list(a = data.frame(v = 1:3), b = data.frame(v = 4:6))
#' pega_lista(lista1, "nueva_columna")
#'
#'
pega_lista <- function(data, nc){

  if(!inherits(data, "list")) stop("Se aplica sobre una lista")
  if(!all(lapply(data, inherits, "data.frame") == TRUE)) stop("Debe ser una lista de data.frames")
  if(length(unique(sapply(data, ncol))) != 1) stop("Hay una cantidad diferente de columnas")
  if(length(unique(sapply(data, names))) != 1) stop("Las columnas tienen nombres diferentes")

  if(is.null(names(data))){
    warning("La lista no tiene nombres, se usará la posición")
    temp <- Map(cbind, data, ncol = 1:length(data))

  } else{
    temp <- Map(cbind, data, ncol = names(data))
  }

  temp <- do.call("rbind.data.frame", temp)
  #temp <- dplyr::bind_rows(temp) #bind_rows tambien pega si es que no son las mismas columnas
  names(temp)[names(temp) == 'ncol'] <- nc
  rownames(temp) <- NULL
  return(temp)

}
