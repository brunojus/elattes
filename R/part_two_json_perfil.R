#' @title PartTwoJsonPerfil
#'
#' 
#'
#' @param publicacoes,tipo a list of Lattes CV XML files
#' @param tipo
#' 
#'
#'
#'
#' @export PartTwoJsonPerfil

PartTwoJsonPerfil<-function(publicacoes,tipo){

	s<-""
	s <- paste(s, sep="[ ", " ") 
	for (item in publicacoes[[tipo]] ){
		if(!is.null(item)){
			s <- paste(s ,sep="", ToJson(item,tipo))
		}
	}

	s<-substr(s, 1, nchar(s)-1)
        s <- paste(s ,sep=" ", "],")
	s	

}
