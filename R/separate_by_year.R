SeparateByYear <- function (ids, h, parametros, tipo, tipoDaProducao ) {

	
        year1 <- parametros$gerais$global$itens_desde_o_ano
        year2 <- parametros$gerais$global$itens_ate_o_ano

	years <- list()
	h3 <- hash()

	for (year in year1:year2) {

		years <- c(years, year)
		    
		producao<-new.env(parent=emptyenv())

		if(tipoDaProducao=='BIBLIOGRAFICA'){
		   producao$publications<-list()
		}else if (tipoDaProducao=='ORIENTACAO'){
			producao$orientacoes<-list()
		}
		  .set(h3,year,producao)
	}
	  


	for (id in ids) {
		if(tipo=='PERIODICO'){
		  for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_PUBLICADOS) {
		    ANO_DO_ARTIGO <- paper$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO
		    if (!is.null(ANO_DO_ARTIGO)){
			    for (ANO_DE_ANALISE in years){
			      if (ANO_DO_ARTIGO == ANO_DE_ANALISE) {
				 h3[[ANO_DO_ARTIGO]]$publications <- c( h3[[ANO_DO_ARTIGO]]$publications, list(paper) )
			      }
		    	    }
		    }
		  }
		}else if(tipo=='EVENTO'){
		  for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$TRABALHOS_EM_EVENTOS) {
		   ANO_DO_TRABALHO<-paper$DADOS_BASICOS_DO_TRABALHO$ANO_DO_TRABALHO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_DO_TRABALHO == ANO_DE_ANALISE) {
		      h3[[ANO_DO_TRABALHO]]$publications <- c( h3[[ANO_DO_TRABALHO]]$publications, list(paper) )
		    }
		   }
		  }
		}else if(tipo=='LIVRO'){
		  for (livro in h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$LIVROS_PUBLICADOS_OU_ORGANIZADOS) {
		   ANO <- livro$DADOS_BASICOS_DO_LIVRO$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO == ANO_DE_ANALISE) {
		      h3[[ANO]]$publications <- c( h3[[ANO]]$publications, list(livro) )
		    }
		   }
		  }
		}else if(tipo=='DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'){
		  for (outro in h[[id]]$PRODUCAO_BIBLIOGRAFICA$DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA) {
		   ANO <- outro$DADOS_BASICOS_DE_OUTRA_PRODUCAO$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO == ANO_DE_ANALISE) {
		      h3[[ANO]]$publications <- c( h3[[ANO]]$publications, list(outro) )
		    }
		   }
		  }
		}else if(tipo=='CAPITULO_DE_LIVRO'){
		  for (capitulo in h[[id]]$PRODUCAO_BIBLIOGRAFICA$LIVROS_E_CAPITULOS$CAPITULOS_DE_LIVROS_PUBLICADOS) {
                   ANO <- capitulo$DADOS_BASICOS_DO_CAPITULO$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO == ANO_DE_ANALISE) {
		      h3[[ANO]]$publications <- c( h3[[ANO]]$publications, list(capitulo) )
		    }
		   }
		  }

		}else if(tipo=='TEXTO_EM_JORNAIS'){
		  for (texto in h[[id]]$PRODUCAO_BIBLIOGRAFICA$TEXTOS_EM_JORNAIS_OU_REVISTAS) {
		   ANO_DO_TEXTO<-texto$DADOS_BASICOS_DO_TEXTO$ANO_DO_TEXTO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_DO_TEXTO == ANO_DE_ANALISE) {
		      h3[[ANO_DO_TEXTO]]$publications <- c( h3[[ANO_DO_TEXTO]]$publications, list(texto) )
		    }
		   }
		  }
		}else if(tipo=='ARTIGO_ACEITO'){
		  for (paper in h[[id]]$PRODUCAO_BIBLIOGRAFICA$ARTIGOS_ACEITOS_PARA_PUBLICACAO) {
		   ANO_DO_ARTIGO<-paper$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO 
		   for (year in years){
		    if (ANO_DO_ARTIGO == year) {
		      h3[[ANO_DO_ARTIGO]]$publications <- c( h3[[ANO_DO_ARTIGO]]$publications, list(paper) )
		    }
		   }
		  }
		}else if(tipo=='ORIENTACAO_CONCLUIDA_MESTRADO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_MESTRADO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		} else if(tipo=='ORIENTACAO_EM_ANDAMENTO_MESTRADO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_MESTRADO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		}  else if(tipo=='ORIENTACAO_EM_ANDAMENTO_GRADUACAO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_GRADUACAO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		}   else if(tipo=='ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_INICIACAO_CIENTIFICA) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		}  else if(tipo=='ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DE_POS_DOUTORADO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		} else if(tipo=='ORIENTACAO_EM_ANDAMENTO_DOUTORADO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_EM_ANDAMENTO_DOUTORADO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		} else if(tipo=='ORIENTACAO_CONCLUIDA_DOUTORADO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_DE_DOUTORADO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		} else if(tipo=='ORIENTACAO_CONCLUIDA_POS_DOUTORADO'){
		  for (orientacao in h[[id]]$ORIENTACOES$ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		} else if(tipo=='OUTRAS_ORIENTACOES_CONCLUIDAS'){
		  for (orientacao in h[[id]]$ORIENTACOES$OUTRAS_ORIENTACOES_CONCLUIDAS) {
		   ANO_CONCLUSAO <- orientacao$DADOS_BASICOS$ANO
		   for (ANO_DE_ANALISE in years){
		    if (ANO_CONCLUSAO == ANO_DE_ANALISE) {
		      h3[[ANO_CONCLUSAO]]$orientacoes <- c( h3[[ANO_CONCLUSAO]]$orientacoes, list(orientacao) )
		    }
		   }
		  }
		}
	} 

	h3
}
