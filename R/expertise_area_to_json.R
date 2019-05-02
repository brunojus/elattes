#' @title ExpertiseAreaToJson
#'
#' 
#'
#' @param pessoa a list of Lattes CV XML files
#'
#' 
#'
#'
#'
#' @export ExpertiseAreaToJson

ExpertiseAreaToJson <- function(pessoa) {
	
	
  	s <- paste("", sep="[", " ")

	for(item in pessoa$AREAS_DE_ATUACAO){
		
		element <- sprintf('{"grande_area":"%s", "area":"%s", "sub_area":"%s", "especialidade":"%s" },', Escape(item$NOME_GRANDE_AREA_DO_CONHECIMENTO), Escape(item$NOME_DA_AREA_DO_CONHECIMENTO), Escape(item$NOME_DA_SUB_AREA_DO_CONHECIMENTO), Escape(item$NOME_DA_ESPECIALIDADE) )
		
	
       		s <- paste(s, sep="", element)
	}

	s<-substr(s, 1, nchar(s)-1)
 	s <- paste(s ,sep="", "]")
  	s

}
