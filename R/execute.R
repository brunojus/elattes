#' @title Execute
#'
#' 
#'
#' @param ids a list of Lattes CV XML files
#' @param dado
#' @param conf
#' @param tipo
#' @param doTipo
#' @param levenshtein
#' @export Execute

Execute<-function (ids, dado, conf, tipo, doTipo,levenshtein){

 r<-Filter(ids, dado,  conf, doTipo)
 r<-SeparateByYear(ids, r, conf ,doTipo, tipo)
 x <-GetTypeProductionNoDuplicate(r ,levenshtein, doTipo , tipo)

 x

}
