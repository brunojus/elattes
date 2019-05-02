#' @title Validate
#'
#' 
#'
#' @param element, field a list of Lattes CV XML files
#' @param field
#' 
#'
#'
#'
#' @export Validate

Validate <- function (element, field){

 result<-NULL
 
 tryCatch( result<-xmlGetAttr(element,field)  ,
       	   error = function(e) {
		
	   }  
         )
 if(field=='TEXTO-RESUMO-CV-RH' && is.null(result) ){
  result<-"" 
 }

 result
}
