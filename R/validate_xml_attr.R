ValidateXmlAttr <- function(attr, field){
 result<-NULL

	
 tryCatch( result<- attr[[field]]  ,
       	   error = function(e) {
		
	   }  
         )
 result



}
