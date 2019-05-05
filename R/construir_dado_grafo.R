#' @title ConstruirDadoGrafo
#'
#' 
#'
#' @param dadosBasicos a list of Lattes CV XML files.
#' @param publicacoesPorAno field a list of Lattes CV XML files.
#' @param configuracao field a list of Lattes CV XML files.
#' @param default field a list of Lattes CV XML files.
#' 
#'
#' @export ConstruirDadoGrafo

ConstruirDadoGrafo <- function(dadosBasicos, publicacoesPorAno, configuracao, default) {

	s <- paste('{ \"label\": \"FiocruzNetwork\", \"nodes\": [', sep=" ", " ")
 

  
	ids<-keys(dadosBasicos)

	coautores <-hash()
	i<-1
	for(id in ids){
		#primeira parte
		pessoa<-dadosBasicos[[id]]$DADOS_GERAIS
		s <- paste(s, sep=" ", ParteUmGrafo(pessoa, id, i)) 
		
		#segunda parte
		publicacoes<-PublicaoesDestaPessoa(id,publicacoesPorAno)

		dadoscoautores<-GetDadosCoautores(publicacoes, id, configuracao, default)
	
		
		.set(coautores, id , dadoscoautores)
		i<-i+1
		
	
	}


	s<-substr(s, 1, nchar(s)-1)
	s <- paste(s, sep=" ", "]")   
	
	

	tabela <- EliminarLinksDuplicados( ids ,coautores)
	
	
	s1 <- paste(', \"links\": [ ', sep=" ")
 
if(!is.null(tabela) ){
	for(id in ids){

	   for (item in tabela[[id]]){
		s1<- paste(s1, sep="", ParteDoisGrafo(id, item, coautores[[id]][[item]])) 
	   }

	
	}

	s1<-substr(s1, 1, nchar(s1)-1)

}

	s1 <- paste(s1, sep=" ", "] ")	 

	
	s <- paste(s, sep="", s1)
	s <- paste(s, sep="", "}")	
 	s 

}


ParteUmGrafo<-function(pessoa, id,i){

	s <- paste("{")
 
	element <- sprintf(' "id": "%s",   "label": "%s", "properties": { "name": "%s"} },', id, i ,pessoa$NOME_COMPLETO)


	s <- paste(s ,sep=" ", element)
       


}


ParteDoisGrafo<-function(id, id1, numeroDePublicacoes){

	s <- paste("")
 
	element <- sprintf(' { "source": "%s",   "target": "%s", "weigth": "%s" },', id, id1, numeroDePublicacoes)


	s <- paste(s ,sep="", element)
       
	s

}

GetDadosCoautores <- function(publicacoes, id, configuracao, default){
	dados <- hash()

	temp <- hash()
	.set(temp, "incluir_artigo_em_periodico","PERIODICO")
	.set(temp, "incluir_livro_publicado","LIVRO")
	.set(temp, "incluir_capitulo_de_livro_publicado" ,"CAPITULO_DE_LIVRO")
	.set(temp, "incluir_trabalho_em_congresso" ,"EVENTO")
	.set(temp, "incluir_texto_em_jornal_de_noticia" ,"TEXTO_EM_JORNAIS")
	.set(temp, "incluir_outro_tipo_de_producao_bibliografica" ,"DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA")
	.set(temp, "incluir_artigo_aceito_para_publicacao" ,"ARTIGO_ACEITO")

	
	tipos<-names(configuracao$gerais$grafo_de_coautoria)

aux<-list()	
for (tipo in tipos){

  if(default[[tipo]]=="sim"){
      aux<-c(aux, list(temp[[tipo]]) )
  }
  	
}

tipos<-NULL
tipos<-c(tipos, aux)
	##tipos<-list()

	##tipos<-c(tipos, list('EVENTO', 'PERIODICO', 'CAPITULO_DE_LIVRO'))
	
for(tipo in tipos){
  		
	for(p in publicacoes[[tipo]]){

		for(idCoautor in p$authorsEndogenous){		

			if(id != idCoautor){

			   qtd<-GetQtdPubCoautoradas(publicacoes, idCoautor, tipos)
                          .set(dados,idCoautor, qtd)
				   
				
			}
		}
	}
  
	
}

	dados	
}

GetQtdPubCoautoradas <- function(publicacoes, idCoautor, tipos){

cont<-0

for(tipo in tipos){
 for( p in publicacoes[[tipo]]){
	for(id in p$authorsEndogenous){

		if(idCoautor==id){

			cont<-cont+1
		}
	}
 }
}
cont

}

EliminarLinksDuplicados <- function(ids,  coautores){


tabela<-hash()

for(id in ids){
	aux<-list()
	for(id1 in ids){
		if(id != id1 && !is.null(coautores[[id]][[id1]]) ){
					
			aux<-c(aux,list(id1))		
			
		}
		
	}
	if(length(aux)>0){
	.set(tabela, id, aux)
	}
	
}


if(!is.null(tabela)){
 for(id in ids){
  for(i in tabela[[id]]){
	tam<-1
	for (item in tabela[[i]]){
	  if(item == id ){
		tabela[[i]][[tam]]<-NULL
	  }
	  tam<-tam+1
	}
			
  }
 }
}



tabela

}

