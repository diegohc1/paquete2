
#' Correlaciones pairwise
#'
#' @param data data frame
#' @param round redondear, por default 2 decimales (round, by default 2 digits)
#'
#' @return matriz de correlaciones (matrix correlations)
#' @export
#'
#' @examples
#' cor2(mtcars)

cor2 <- function(data, round = 2){
  round(cor(data, use = "pairwise.complete.obs"), round)
}
