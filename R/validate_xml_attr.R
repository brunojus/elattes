
#' @title ValidateXmlAttr
#'
#' 
#'
#' @param attr, field a list of Lattes CV XML files.
#' @param field field a list of Lattes CV XML files.
#' 
#'
#'
#'
#' @export ValidateXmlAttr

ValidateXmlAttr <- function(attr, field){
 result<-NULL

	
 tryCatch( result<- attr[[field]]  ,
       	   error = function(e) {
		
	   }  
         )
 result



}
