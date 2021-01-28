#' Title
#' @param X mama
#' @param N0 lala
#' @param m0 hha
#' @param a0 kaka
#' @param c0 uaua
#' @param B0 yaya
#' @return hjeke
#'
#' @importFrom reticulate source_python
#' @examples
#' @export clusterIndividual
clusterIndividual <- function(X, N0, m0, a0, c0, B0) {

  path <- paste(system.file(package="ensMAPDP"), "mapdp.py", sep="/")

  source_python(path)
  output <- mapdp(X, N0, m0, a0, c0, B0)
  return(output)
}
