#' @title BuildJson
#'
#' 
#'
#' @param pessoa  field a list of Lattes CV XML files.
#' @param publicacoes  field a list of Lattes CV XML files.
#' @param orientacoes  field a list of Lattes CV XML files.
#' @param senioridade  field a list of Lattes CV XML files.
#'
#'
#' @export BuildJson

BuildJson <- function(pessoa, publicacoes, orientacoes, senioridade) {


  	s <- NULL
	publicacao<-" "
	orientacao<-" "

	if(length(publicacoes)>0){
            publicacao <- PartOneJsonPerfil(publicacoes)
        }else{
	    publicacao <- paste(publicacao ,sep=" ", "[ ],")
        }

	if(length(orientacoes)>0){
            orientacao <- PartOneJsonPerfil(orientacoes)
        }else{
	    orientacao <- paste(orientacao ,sep=" ", "[ ],")
        }
 	
	NOME_COMPLETO<-NullToString(pessoa$NOME_COMPLETO)
	TEXTO_RESUMO_CV_RH<-NullToString(pessoa$TEXTO_RESUMO_CV_RH)
	NOME_INSTITUICAO_EMPRESA<-NullToString(pessoa$ENDERECO_PROFISSIONAL$NOME_INSTITUICAO_EMPRESA)
	NOME_ORGAO <-NullToString(pessoa$ENDERECO_PROFISSIONAL$NOME_ORGAO)

	NOME_UNIDADE<-NullToString(pessoa$ENDERECO_PROFISSIONAL$NOME_UNIDADE)
	DDD<-NullToString(pessoa$ENDERECO_PROFISSIONAL$DDD)
	TELEFONE<-NullToString(pessoa$ENDERECO_PROFISSIONAL$TELEFONE)
	BAIRRO<-NullToString(pessoa$ENDERECO_PROFISSIONAL$BAIRRO)
	CIDADE<-NullToString(pessoa$ENDERECO_PROFISSIONAL$CIDADE)
	CEP<-NullToString(pessoa$ENDERECO_PROFISSIONAL$CEP)

	

 
	element <- sprintf('{ "nome":"%s", "resumo_cv":"%s",  "areas_de_atuacao": %s, "endereco_profissional": { "instituicao": "%s", "orgao": "%s", "unidade": "%s", "DDD": "%s",  "telefone": "%s", "bairro": "%s", "cep": "%s", "cidade": "%s" }, "producao_bibiografica":%s, "orientacoes_academicas":%s, "senioridade": "%s" },', Escape(NOME_COMPLETO), Escape(TEXTO_RESUMO_CV_RH),  ExpertiseAreaToJson(pessoa), Escape(NOME_INSTITUICAO_EMPRESA), Escape(NOME_ORGAO), Escape(NOME_UNIDADE), Escape(DDD), Escape(TELEFONE), Escape(BAIRRO), CEP, Escape(CIDADE), publicacao, orientacao, senioridade)
        s <- paste(element)
	

	
	s 

}

NullToString<-function(x){
 
 if (is.null(x)){
	x=" "
 }
 x
}
