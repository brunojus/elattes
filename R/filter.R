#' @title Filter
#'
#' 
#'
#' @param ids tipo a list of Lattes CV XML files.
#' @param h field a list of Lattes CV XML files.
#' @param parametros field a list of Lattes CV XML files.
#' @param tipo field a list of Lattes CV XML files.
#'
#'
#' @export Filter
Filter <- function(ids, h, parametros, tipo) {
 
	initial<-parametros$gerais$global$itens_desde_o_ano
	final<-parametros$gerais$global$itens_ate_o_ano

        h2<-GetPeriod(parametros$especificos)

	h<-AddIdToPapers(ids, h, tipo) 

	for (id in ids) {
	    i <- 1

		if(tipo == 'PERIODICO'){

		    
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS) {
 
			      ANO_DO_ARTIGO <- paper$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO	

			      if(!is.null(ANO_DO_ARTIGO)){

				if (  (ANO_DO_ARTIGO < initial ||
				   ANO_DO_ARTIGO > final) || (ANO_DO_ARTIGO < h2[[id]]$initialDate || ANO_DO_ARTIGO > h2[[id]]$finalDate )  ) {
		
				h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS[[i]] <- NULL
		
			        }else {
				  i=i+1
			        }

			     }

			      
		    } 
		}else if(tipo == 'EVENTO'){

		    
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$TRABALHOS_EM_EVENTOS) {

	              ANO_DO_TRABALHO <- paper$DADOS_BASICOS_DO_TRABALHO$ANO_DO_TRABALHO
		      if ( (ANO_DO_TRABALHO < initial ||
			   ANO_DO_TRABALHO > final) || (ANO_DO_TRABALHO < h2[[id]]$initialDate || ANO_DO_TRABALHO > h2[[id]]$finalDate ) ) {
		
			h[[id]]$PRODUCAO_BIBLIOGRAFICA$TRABALHOS_EM_EVENTOS[[i]] <- NULL
		
		      } else {
			i=i+1
		      }
		    }

		}else if(tipo == 'CAPITULO_DE_LIVRO'){
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$CAPITULOS_DE_LIVROS_PUBLICADOS) {

		      ANO <- paper$DADOS_BASICOS_DO_CAPITULO$ANO
		      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate )   ) {
		
			h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$CAPITULOS_DE_LIVROS_PUBLICADOS[[i]] <- NULL
		
		      } else {
			i=i+1
		      }
		    }
		}else if(tipo == 'LIVRO'){
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$LIVROS_PUBLICADOS_OU_ORGANIZADOS) {

		      ANO <- paper$DADOS_BASICOS_DO_LIVRO$ANO
                                           
		      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate )  ) {
		
			h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$LIVROS_PUBLICADOS_OU_ORGANIZADOS[[i]] <- NULL
		
		      } else {
			i=i+1
		      }
                     
		    }

		} else if(tipo == 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'){
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA) {

		      ANO <- paper$DADOS_BASICOS_DE_OUTRA_PRODUCAO$ANO

		      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate )  ) {
		
			h[[id]]$PRODUCAO_BIBLIOGRAFICA$DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA[[i]] <- NULL
		
		      } else {
			i=i+1
		      }
                     
		    }

		} else if(tipo == 'TEXTO_EM_JORNAIS'){
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$TEXTOS_EM_JORNAIS_OU_REVISTAS) {

		      ANO_DO_TEXTO <- paper$DADOS_BASICOS_DO_TEXTO$ANO_DO_TEXTO
		      if ( (ANO_DO_TEXTO < initial || ANO_DO_TEXTO > final) || (ANO_DO_TEXTO < h2[[id]]$initialDate || ANO_DO_TEXTO > h2[[id]]$finalDate )   ) {
		
			h[[id]]$PRODUCAO_BIBLIOGRAFICA$TEXTOS_EM_JORNAIS_OU_REVISTAS[[i]] <- NULL
		
		      } else {
			i=i+1
		      }
		    }

		}else if(tipo == 'ARTIGO_ACEITO'){
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_ACEITOS_PARA_PUBLICACAO) {

		      ANO_DO_ARTIGO <- paper$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO
		      if ( ANO_DO_ARTIGO < h2[[id]]$initialDate || ANO_DO_ARTIGO > h2[[id]]$finalDate  ) {
		
			h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_ACEITOS_PARA_PUBLICACAO[[i]] <- NULL
		
		      } else {
			i=i+1
		      }
		    }
		}else if(tipo == 'ORIENTACAO_CONCLUIDA_MESTRADO'){
		    for (paper in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_MESTRADO) {

		      ANO <- paper$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_MESTRADO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO'){
		    for (paper in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO) {

		      ANO <- paper$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'){
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_POS_DOUTORADO) {

		      ANO <- orientacao$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_POS_DOUTORADO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'){
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_GRADUACAO) {

		      ANO <- orientacao$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_GRADUACAO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'){
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_MESTRADO) {

		      ANO <- orientacao$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_MESTRADO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'){
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_INICIACAO_CIENTIFICA) {

		      ANO <- orientacao$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_INICIACAO_CIENTIFICA[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'){
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DOUTORADO) {

		      ANO <- orientacao$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DOUTORADO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		       }
		    }


		} else if(tipo == 'ORIENTACAO_CONCLUIDA_DOUTORADO'){
		    for (paper in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_DOUTORADO) {

		      ANO <- paper$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
			      if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
				h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_DOUTORADO[[i]] <- NULL
		
			      } else {
				i=i+1
			      }
		      }
		    }


		} else if(tipo == 'OUTRAS_ORIENTACOES_CONCLUIDAS'){
		    for (paper in h[[id]]$ORIENTACOES$OUTRAS_ORIENTACOES_CONCLUIDAS) {

		      ANO <- paper$DADOS_BASICOS$ANO
                      if(!is.null(h2[[id]]$initialDate) && !is.null(h2[[id]]$finalDate)){
		        if ( (ANO < initial || ANO > final) || (ANO < h2[[id]]$initialDate || ANO > h2[[id]]$finalDate) ) {
		
			  h[[id]]$ORIENTACOES$OUTRAS_ORIENTACOES_CONCLUIDAS[[i]] <- NULL
		
		        } else {
			i=i+1
		        }
                      }
		    }


		}
	  }
           


  	h
}




AddIdToPapers <- function(ids, h, tipo) {

	 if(tipo == 'PERIODICO'){
		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS) {
		      paper$IDLATTES <- id
		    }
		  }
	 }else if(tipo == 'ARTIGO_ACEITO'){
		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_ACEITOS_PARA_PUBLICACAO) {
		      paper$IDLATTES <- id
		    }
		  }
	}else if(tipo == 'EVENTO'){

 		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$TRABALHOS_EM_EVENTOS) {
		      paper$IDLATTES <- id
		    }
		  }

	 }else if(tipo == 'TEXTO_EM_JORNAIS'){

		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$TEXTOS_EM_JORNAIS_OU_REVISTAS) {
		      paper$IDLATTES <- id
		    }
		  }

	}else if (tipo == 'LIVRO'){
 		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$LIVROS_PUBLICADOS_OU_ORGANIZADOS) {
		      paper$IDLATTES <- id
		    }
		  }

        }else if (tipo == 'CAPITULO_DE_LIVRO'){
 		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$CAPITULOS_DE_LIVROS_PUBLICADOS) {
		      paper$IDLATTES <- id
		    }
		  }

        } else if (tipo == 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'){
 		  for (id in ids) {
		    for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA) {
		      paper$IDLATTES <- id
		    }
		  }

        } else if(tipo == 'ORIENTACAO_CONCLUIDA_MESTRADO'){
 		  for (id in ids) {
		    for (paper in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_MESTRADO) {
		      paper$IDLATTES <- id
		      paper$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	} else if(tipo == 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO'){
 		  for (id in ids) {
		    for (paper in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO) {
		      paper$IDLATTES <- id
		      paper$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'){
 		  for (id in ids) {
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_MESTRADO) {
		      orientacao$IDLATTES <- id
		      orientacao$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	}  else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'){
 		  for (id in ids) {
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_GRADUACAO) {
		      orientacao$IDLATTES <- id
		      orientacao$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	}  else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'){
 		  for (id in ids) {
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_INICIACAO_CIENTIFICA) {
		      orientacao$IDLATTES <- id
		      orientacao$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'){
 		  for (id in ids) {
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DOUTORADO) {
		      orientacao$IDLATTES <- id
		      orientacao$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'){
 		  for (id in ids) {
		    for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_POS_DOUTORADO) {
		      orientacao$IDLATTES <- id
		      orientacao$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	} else if(tipo == 'ORIENTACAO_CONCLUIDA_DOUTORADO'){
 		  for (id in ids) {
		    for (paper in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_DOUTORADO) {
		      paper$IDLATTES <- id
		      paper$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		    }
		  }

	}else if(tipo == 'OUTRAS_ORIENTACOES_CONCLUIDAS'){
		 for (id in ids) {
		   for (paper in h[[id]]$ORIENTACOES$OUTRAS_ORIENTACOES_CONCLUIDAS) {
		     paper$IDLATTES <- id
		     paper$NOME_COMPLETO<-h[[id]]$DADOS_GERAIS$NOME_COMPLETO
		   }
		 }
      
	}
 	
	 h
}

GetPeriod <-function( x ){
  h2 <- hash()

  tam<-length(x)

  for(i in 1:tam){
  
   for(u in x[[i]]){
     employee<-new.env(parent=emptyenv())
     for (j in 1:2){
       if(j==1){
          employee$initialDate<-u$periodo[[j]]
       }else{
         employee$finalDate<-u$periodo[[j]]
       }
    }
     .set(h2,u$id, employee)
   }
 }
h2
}
