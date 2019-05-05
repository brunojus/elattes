#' @title ConvertDataToJson
#'
#' 
#'
#' @param conf.JSON, type.production, default, production.by.year.and.type a list of Lattes CV XML files.
#' @param type.production field a list of Lattes CV XML files.
#' @param default field a list of Lattes CV XML files.
#' @param production.by.year.and.type field a list of Lattes CV XML files.
#' 
#'
#' @export ConvertDataToJson
ConvertDataToJson<-function(conf.JSON, type.production, default, production.by.year.and.type){

types<-NULL

if(type.production=="ORIENTACAO"){
  types<-names(conf.JSON$gerais$orientacoes)
}else if(type.production=="BIBLIOGRAFICA"){

  types<-names(conf.JSON$gerais$publicacoes)
}

s <- paste("{", sep="")

	
for (type in types){

  if(type.production=="BIBLIOGRAFICA"){




     if(default[[type]]=="sim"){


	if(type=='incluir_artigo_em_periodico'){

		s <- paste(s ,sep="\"PERIODICO\":", DataToJson(production.by.year.and.type[[type]],'PERIODICO')) 
	
	} else  if(type=='incluir_trabalho_em_congresso'){

                s <- paste(s ,sep="\"EVENTO\":", DataToJson(production.by.year.and.type[[type]],'EVENTO')) 	

        }else if(type=='incluir_capitulo_de_livro_publicado') {

		s <- paste(s ,sep="\"CAPITULO_DE_LIVRO\":", DataToJson(production.by.year.and.type[[type]],'CAPITULO_DE_LIVRO')) 
	
        } else if(type=='incluir_livro_publicado') {

		s <- paste(s ,sep="\"LIVRO\":", DataToJson(production.by.year.and.type[[type]],'LIVRO')) 
	
        } else if(type=='incluir_texto_em_jornal_de_noticia') {

		s <- paste(s ,sep="\"TEXTO_EM_JORNAIS\":", DataToJson(production.by.year.and.type[[type]],'TEXTO_EM_JORNAIS')) 
	
        } else if(type=='incluir_artigo_aceito_para_publicacao') {

		s <- paste(s ,sep="\"ARTIGO_ACEITO\":", DataToJson(production.by.year.and.type[[type]],'ARTIGO_ACEITO')) 
	
        } else if(type=='incluir_outro_tipo_de_producao_bibliografica') {

		s <- paste(s ,sep="\"DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA\":", DataToJson(production.by.year.and.type[[type]],'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA')) 
	
        }




        }



      } else {

 
          if (default[[type]]=="sim"){



		if(type=='incluir_orientacao_em_andamento_mestrado'){

			s <- paste(s ,sep="\"ORIENTACAO_EM_ANDAMENTO_MESTRADO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'))


		}else if (type=='incluir_orientacao_concluida_mestrado'){

			s <- paste(s ,sep="\"ORIENTACAO_CONCLUIDA_MESTRADO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_CONCLUIDA_MESTRADO'))

		}else if (type=='incluir_orientacao_em_andamento_pos_doutorado'){

			s <- paste(s ,sep="\"ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'))

		}else if (type=='incluir_orientacao_concluida_pos_doutorado'){

			s <- paste(s ,sep="\"ORIENTACAO_CONCLUIDA_POS_DOUTORADO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO'))

		}else if (type=='incluir_orientacao_em_andamento_tcc'){

			s <- paste(s ,sep="\"ORIENTACAO_EM_ANDAMENTO_GRADUACAO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'))

		}else if (type=='incluir_orientacao_em_andamento_iniciacao_cientifica'){

			s <- paste(s ,sep="\"ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'))

		}else if (type=='incluir_orientacao_em_andamento_doutorado'){

			s <- paste(s ,sep="\"ORIENTACAO_EM_ANDAMENTO_DOUTORADO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'))

		} else if (type=='incluir_outras_orientacoes_concluidas'){

			s <- paste(s ,sep="\"OUTRAS_ORIENTACOES_CONCLUIDAS\":", DataToJson(production.by.year.and.type[[type]], 'OUTRAS_ORIENTACOES_CONCLUIDAS'))

		} else if (type=='incluir_orientacao_concluida_doutorado'){

			s <- paste(s ,sep="\"ORIENTACAO_CONCLUIDA_DOUTORADO\":", DataToJson(production.by.year.and.type[[type]], 'ORIENTACAO_CONCLUIDA_DOUTORADO'))

		}  
	           	  
         } 





      }

}


if(s!="{"){ 
   s<-substr(s, 1, nchar(s)-1)
}

s <- paste(s ,sep=" ", "}")

return (s)

	
}



 DataToJson <- function( publicacoesPorAno, tipo) {
 
  s <- paste("{", sep = " ")
  
  anos<-keys(publicacoesPorAno) 

  for(ano in anos){
          s <- paste(s, sep="\"", ano)
          s <- paste(s, sep="\":[ ", " ")  

	  for (item in publicacoesPorAno[[ano]] ){
                if(!is.null(item)){
		s <- paste(s ,sep="", ToJson(item,tipo))
		}
	  }

	  
         s<-substr(s, 1, nchar(s)-1)

         s <- paste(s ,sep=" ", "],")

  }

  s<-substr(s, 1, nchar(s)-1)
  s <- paste(s ,sep=" ", "},")
  s
}


ToJson <- function( publicacao , tipo) {

  	s <- NULL
	
	if(tipo == 'PERIODICO'){
	   element <- sprintf('{ "natureza":"%s", "titulo":"%s", "periodico":"%s", "ano": "%s", "volume": "%s", "issn": "%s", "paginas": "%s - %s", "doi": "%s", "autores": %s, "autores-endogeno":%s},', Escape(publicacao$NATUREZA), Escape(publicacao$TITULO_DO_ARTIGO), Escape(publicacao$TITULO_DO_PERIODICO_OU_REVISTA), Escape(publicacao$ANO_DO_ARTIGO), Escape(publicacao$VOLUME), Escape(publicacao$ISSN) , Escape(publicacao$PAGINA_INICIAL), Escape(publicacao$PAGINA_FINAL) , Escape(publicacao$DOI), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1))
           s <- paste(element)
	
	}else if(tipo == 'ARTIGO_ACEITO'){

	   element <- sprintf('{ "natureza":"%s", "titulo":"%s", "periodico":"%s", "ano": "%s", "volume": "%s", "issn": "%s", "paginas": "%s - %s", "doi": "%s", "autores": %s, "autores-endogeno":%s},', Escape(publicacao$NATUREZA), Escape(publicacao$TITULO_DO_ARTIGO), Escape(publicacao$TITULO_DO_PERIODICO_OU_REVISTA), Escape(publicacao$ANO_DO_ARTIGO), Escape(publicacao$VOLUME), Escape(publicacao$ISSN) , Escape(publicacao$PAGINA_INICIAL), Escape(publicacao$PAGINA_FINAL) , Escape(publicacao$DOI), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1))
           s <- paste(element)


        }else if (tipo == 'TEXTO_EM_JORNAIS'){

	   element <- sprintf('{ "natureza":"%s", "titulo":"%s", "periodico":"%s", "ano": "%s", "issn": "%s", "paginas": "%s - %s", "doi": "%s", "autores": %s, "autores-endogeno":%s},',Escape(publicacao$NATUREZA), Escape(publicacao$TITULO_DO_TEXTO), Escape(publicacao$TITULO_DO_JORNAL_OU_REVISTA), Escape(publicacao$ANO_DO_TEXTO),  Escape(publicacao$ISSN), Escape(publicacao$PAGINA_INICIAL), Escape(publicacao$PAGINA_FINAL), Escape(publicacao$DOI), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1))
           s <- paste(element)


	}else if (tipo == 'EVENTO'){

	   element <- sprintf('{ "natureza":"%s", "titulo":"%s", "nome_do_evento":"%s", "ano_do_trabalho": "%s", "pais_do_evento": "%s", "cidade_do_evento": "%s",  "doi": "%s", "classificacao": "%s", "paginas": "%s - %s", "autores": %s, "autores-endogeno":%s},', Escape(publicacao$NATUREZA), Escape(publicacao$TITULO_DO_TRABALHO), Escape(publicacao$NOME_DO_EVENTO), Escape(publicacao$ANO_DO_TRABALHO), Escape(publicacao$PAIS_DO_EVENTO), Escape(publicacao$CIDADE_DO_EVENTO), Escape(publicacao$DOI), Escape(publicacao$CLASSIFICACAO_DO_EVENTO),   Escape(publicacao$PAGINA_INICIAL), Escape(publicacao$PAGINA_FINAL), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1))
           s <- paste(element)

        }else if(tipo == 'LIVRO'){
	   element <- sprintf('{ "titulo":"%s", "ano":"%s", "tipo":"%s", "natureza":"%s", "pais_de_publicacao":"%s", "isbn":"%s", "doi":"%s", "nome_da_editora":"%s", "numero_da_edicao_revisao":"%s", "numero_de_paginas":"%s", "numero_de_volumes":"%s",  "autores": %s, "autores-endogeno":%s },',Escape(publicacao$TITULO_DO_LIVRO), Escape(publicacao$ANO),
Escape(publicacao$TIPO), Escape(publicacao$NATUREZA), Escape(publicacao$PAIS_DE_PUBLICACAO), Escape(publicacao$ISBN), Escape(publicacao$DOI), Escape(publicacao$NOME_DA_EDITORA), Escape(publicacao$NUMERO_DA_EDICAO_REVISAO), Escape(publicacao$NUMERO_DE_PAGINAS), Escape(publicacao$NUMERO_DE_VOLUMES), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1))
           s <- paste(element)
	} else if (tipo == 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'){

	   element <- sprintf('{ "natureza":"%s", "titulo":"%s", "ano":"%s", "pais_de_publicacao":"%s", "editora":"%s", "doi":"%s", "numero_de_paginas":"%s", "autores": %s, "autores-endogeno":%s},', Escape(publicacao$NATUREZA), Escape(publicacao$TITULO), Escape(publicacao$ANO), Escape(publicacao$PAIS_DE_PUBLICACAO), Escape(publicacao$EDITORA), Escape(publicacao$DOI), Escape(publicacao$NUMERO_DE_PAGINAS), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1) )
           s <- paste(element)
	} else if (tipo == 'CAPITULO_DE_LIVRO'){
	   element <- sprintf('{ "tipo":"%s", "titulo_do_capitulo":"%s", "titulo_do_livro": "%s", "ano":"%s", "doi": "%s", "pais_de_publicacao":"%s", "isbn":"%s", "nome_da_editora":"%s", "numero_da_edicao_revisao":"%s", "organizadores":"%s", "paginas": "%s - %s",  "autores": %s, "autores-endogeno":%s },',Escape(publicacao$TIPO), Escape(publicacao$TITULO_DO_CAPITULO_DO_LIVRO), Escape(publicacao$TITULO_DO_LIVRO),  Escape(publicacao$ANO), Escape(publicacao$DOI), Escape(publicacao$PAIS_DE_PUBLICACAO), Escape(publicacao$ISBN), Escape(publicacao$NOME_DA_EDITORA), Escape(publicacao$NUMERO_DA_EDICAO_REVISAO), Escape(publicacao$ORGANIZADORES), Escape(publicacao$PAGINA_INICIAL), Escape(publicacao$PAGINA_FINAL), ListToJson(publicacao, 2) ,ListToJson(publicacao, 1))
           s <- paste(element)
	} else if (tipo =='ORIENTACAO_CONCLUIDA_MESTRADO'||
		   tipo =='ORIENTACAO_CONCLUIDA_DOUTORADO'||
		   tipo =='OUTRAS_ORIENTACOES_CONCLUIDAS'||
		   tipo =='ORIENTACAO_CONCLUIDA_POS_DOUTORADO'||
		   tipo =='ORIENTACAO_EM_ANDAMENTO_MESTRADO' || 
		   tipo =='ORIENTACAO_EM_ANDAMENTO_DOUTORADO'||
		   tipo =='ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'||
		   tipo =='ORIENTACAO_EM_ANDAMENTO_GRADUACAO'||
		   tipo =='ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'){
           
	   element <- sprintf('{ "natureza":"%s",  "titulo":"%s", "ano":"%s", "id_lattes_aluno":"%s", "nome_aluno":"%s", "instituicao": "%s", "curso":"%s", "codigo_do_curso":"%s",  "bolsa":"%s", "agencia_financiadora":"%s",  "codigo_agencia_financiadora":"%s", "nome_orientadores": %s, "id_lattes_orientadores":%s },', Escape(publicacao$NATUREZA), Escape(publicacao$TITULO_DO_TRABALHO), Escape(publicacao$ANO) ,Escape(publicacao$NUMERO_ID_ORIENTADO), Escape(publicacao$NOME_DO_ORIENTADO),  Escape(publicacao$NOME_INSTITUICAO),  Escape(publicacao$NOME_CURSO), Escape(publicacao$CODIGO_CURSO),  Escape(publicacao$FLAG_BOLSA), Escape(publicacao$NOME_DA_AGENCIA), Escape(publicacao$CODIGO_AGENCIA_FINANCIADORA), ListToJson(publicacao,3), ListToJson(publicacao, 1))
           s <- paste(element)
                
	} 

	s

}

ListToJson<-function(producao, tipo){
	
	if(tipo==1){
		lista<-producao$authorsEndogenous
	}else if(tipo==2){
		lista<-producao$authors
	}else if (tipo==3){
		lista<-producao$orientadores
	}


	s <- paste("[", sep=" ", "")

	for(item in lista){ 
                              
		s <- paste(s ,sep="\"", Escape(item) )
		s <- paste(s ,sep="\",", "")
	}
	s<-substr(s, 1, nchar(s)-1)
	s <- paste(s, sep=" ", "]")
	s
}
