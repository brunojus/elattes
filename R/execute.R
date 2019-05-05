#' @title Execute
#'
#' 
#'
#' @param ids a list of Lattes CV XML files.
#' @param dado field a list of Lattes CV XML files.
#' @param conf field a list of Lattes CV XML files.
#' @param tipo field a list of Lattes CV XML files.
#' @param doTipo field a list of Lattes CV XML files.
#' @param levenshtein field a list of Lattes CV XML files.
#' @export Execute

Execute<-function (ids, dado, conf, tipo, doTipo,levenshtein){

 r<-Filter(ids, dado,  conf, doTipo)
 r<-SeparateByYear(ids, r, conf ,doTipo, tipo)
 x <-GetTypeProductionNoDuplicate(r ,levenshtein, doTipo , tipo)

 x

}
