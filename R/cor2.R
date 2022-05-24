
#' Correlaciones pairwise
#'
#' @param data Dataframe con los variables a correlacionar
#' @param round Redondear, por default = 2
#'
#' @return Matriz de correlaciones
#' @export
#'
#' @examples
#' cor2(mtcars)

cor2 <- function(data, round = 2){
  round(cor(data, use = "pairwise.complete.obs"), round)
}



