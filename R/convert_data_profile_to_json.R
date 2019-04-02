ConvertDataProfileToJson <- function(dadoBasico, publicacoesPorAno, orientacoesPorAno) {
 
  s <- paste("{", sep=" ", " ")
 

  ids<-keys(dadoBasico) 
  for(id in ids){
	  senioridade<-CalcularSenioridade(dadoBasico,id)
	  p<-PublicaoesDestaPessoa(id,publicacoesPorAno)
	  o<-OrientacoesDestaPessoa(id,orientacoesPorAno)	  

          pessoa<-dadoBasico[[id]]$DADOS_GERAIS
	  
		
          s <- paste(s, sep="\"", id)
          s <- paste(s, sep="\": ", " ")  
	  s <-  paste(s ,sep=" ", BuildJson(pessoa, p, o, senioridade)) 
	 
  }

  s<-substr(s, 1, nchar(s)-1)
  s <- paste(s ,sep=" ", "}")
  s
}


CalcularSenioridade <- function(dadoBasico, id){

	result <- 0
	ano_da_primeira_publicacao <- 0
	ano_atual <- as.numeric( format(Sys.time(), "%Y"))

	if (!is.null(dadoBasico[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS)){
	 
		x<-NULL
          
		tryCatch( x<-dadoBasico[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS[[1]]$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO
	,

	error = function(e) {
	
	}  )
		if(!is.null(x)){
		  

		   ano_da_primeira_publicacao <- as.numeric(x)


		   

		   for (paper in dadoBasico[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS){
			if(!is.null(paper$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO)){
				ano <- as.numeric(paper$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO)
			

				if (ano_da_primeira_publicacao > ano){
			    		ano_da_primeira_publicacao <- ano
				}
			}
		   }

		   result <- ((ano_atual - ano_da_primeira_publicacao)+1)
		}
	  
      }
      result
  
}
