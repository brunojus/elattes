#' @title BUSCAR_AREAS_DE_ATUACAO
#'
#' 
#'
#' @param doc a document
#'
#' 
#'
#'
#'
#' @export BUSCAR_AREAS_DE_ATUACAO

BUSCAR_AREAS_DE_ATUACAO<-function(doc){

areas<-list()
Resultado<-list()
AREAS_DE_ATUACAO<-list()

tryCatch(areas<-xmlRoot(doc)[["DADOS-GERAIS"]][["AREAS-DE-ATUACAO"]][c("AREA-DE-ATUACAO")]


	,

	error = function(e) {
	
	}  )



if(length(areas) > 0) { 
	for(area in areas){

		AREA_DE_ATUACAO <-new.env(parent=emptyenv())
			AREA_DE_ATUACAO$NOME_GRANDE_AREA_DO_CONHECIMENTO<-xmlGetAttr(area, "NOME-GRANDE-AREA-DO-CONHECIMENTO")
			AREA_DE_ATUACAO$NOME_DA_AREA_DO_CONHECIMENTO<-xmlGetAttr(area, "NOME-DA-AREA-DO-CONHECIMENTO")
			AREA_DE_ATUACAO$NOME_DA_SUB_AREA_DO_CONHECIMENTO<-xmlGetAttr(area, "NOME-DA-SUB-AREA-DO-CONHECIMENTO")
			AREA_DE_ATUACAO$NOME_DA_ESPECIALIDADE<-xmlGetAttr(area, "NOME-DA-ESPECIALIDADE")

		AREAS_DE_ATUACAO<-c(AREAS_DE_ATUACAO,list(AREA_DE_ATUACAO))
	  
	  }
	  Resultado<-AREAS_DE_ATUACAO
 } else {

 AREA_DE_ATUACAO <-new.env(parent=emptyenv())
			AREA_DE_ATUACAO$NOME_GRANDE_AREA_DO_CONHECIMENTO<-""
			AREA_DE_ATUACAO$NOME_DA_AREA_DO_CONHECIMENTO<-""
			AREA_DE_ATUACAO$NOME_DA_SUB_AREA_DO_CONHECIMENTO<-""
			AREA_DE_ATUACAO$NOME_DA_ESPECIALIDADE<-""

 Resultado<-AREAS_DE_ATUACAO<-c(AREAS_DE_ATUACAO,list(AREA_DE_ATUACAO))
}

Resultado

}
