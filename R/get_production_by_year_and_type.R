#' @title GetProductionByYearAndType
#'
#' 
#'
#' @param conf.JSON a list of Lattes CV XML files.
#' @param type.production field a list of Lattes CV XML files.
#' @param default field a list of Lattes CV XML files.
#' @param ids field a list of Lattes CV XML files.
#' @param data field a list of Lattes CV XML files.
#' @param levenshtein field a list of Lattes CV XML files.
#' @export GetProductionByYearAndType


GetProductionByYearAndType<-function(conf.JSON, type.production, default, ids, data, levenshtein){

	if (type.production=='ORIENTACAO'){
		types<-names(conf.JSON$gerais$orientacoes)

	}else if(type.production=='BIBLIOGRAFICA'){
		types<-names(conf.JSON$gerais$publicacoes)
	}


ProductionByYearAndType <- hash()


 for (type in types){


  if (type.production =='BIBLIOGRAFICA'){


    if(default[[type]]=="sim"){

        if(type == 'incluir_artigo_em_periodico'){

		periodics.by.year<-Execute(ids, data, conf.JSON, type.production , 'PERIODICO',levenshtein)
	       .set(ProductionByYearAndType, type, periodics.by.year)

        }else if(type == 'incluir_trabalho_em_congresso') {

		events.by.year <- Execute(ids, data, conf.JSON, type.production, 'EVENTO' ,levenshtein)
      		.set(ProductionByYearAndType, type, events.by.year)
	
	}else if(type == 'incluir_capitulo_de_livro_publicado') {

		chapters.by.year <- Execute(ids, data, conf.JSON, type.production, 'CAPITULO_DE_LIVRO' ,levenshtein)
      		.set(ProductionByYearAndType, type, chapters.by.year)
	
	}else if(type == 'incluir_livro_publicado') {

		books.by.year <- Execute(ids, data, conf.JSON, type.production, 'LIVRO' ,levenshtein)
      		.set(ProductionByYearAndType, type, books.by.year)

 	} else if (type == 'incluir_texto_em_jornal_de_noticia') {

		texts.by.year <- Execute(ids, data, conf.JSON, type.production, 'TEXTO_EM_JORNAIS' ,levenshtein)
      		.set(ProductionByYearAndType, type, texts.by.year)

 	} else if (type == 'incluir_artigo_aceito_para_publicacao') {

		texts.by.year <- Execute(ids, data, conf.JSON, type.production, 'ARTIGO_ACEITO' ,levenshtein)
      		.set(ProductionByYearAndType, type, texts.by.year)

 	}else if (type == 'incluir_outro_tipo_de_producao_bibliografica') {

		texts.by.year <- Execute(ids, data, conf.JSON, type.production, 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA' ,levenshtein)
      		.set(ProductionByYearAndType, type, texts.by.year)

 	}

    
    }


  } else {

    if (type=='incluir_orientacao_concluida_mestrado' && default[[type]]=="sim"){
   
      advised.dissertations.by.year <- Execute(ids, data, conf.JSON, type.production,'ORIENTACAO_CONCLUIDA_MESTRADO' , levenshtein)
     .set(ProductionByYearAndType, type, advised.dissertations.by.year)

     } else if (type=='incluir_orientacao_em_andamento_pos_doutorado' && default[[type]]=="sim"){

      postdoctoral.on.going.works.by.year <- Execute(ids, data, conf.JSON, type.production,'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO' , levenshtein)
     .set(ProductionByYearAndType, type, postdoctoral.on.going.works.by.year)

    } else if (type=='incluir_orientacao_concluida_pos_doutorado' && default[[type]]=="sim"){

      postdoctoral.advised.works.by.year <- Execute(ids, data, conf.JSON, type.production,'ORIENTACAO_CONCLUIDA_POS_DOUTORADO' , levenshtein)
      .set(ProductionByYearAndType, type, postdoctoral.advised.works.by.year)

    } else if (type=='incluir_orientacao_em_andamento_mestrado' && default[[type]]=="sim"){

      on.going.dissertations.by.year <- Execute(ids, data, conf.JSON, type.production,'ORIENTACAO_EM_ANDAMENTO_MESTRADO' , levenshtein)
      .set(ProductionByYearAndType, type,  on.going.dissertations.by.year)

    } else if (type=='incluir_orientacao_em_andamento_tcc' && default[[type]]=="sim"){
      
        on.going.monograph.by.year <- Execute(ids, data, conf.JSON, type.production,'ORIENTACAO_EM_ANDAMENTO_GRADUACAO' , levenshtein)
        .set(ProductionByYearAndType, type, on.going.monograph.by.year)

    } else if (type =='incluir_orientacao_em_andamento_iniciacao_cientifica' && default[[type]]=="sim"){

        on.going.scientific.initiation.by.year <- Execute(ids, data, conf.JSON, type.production ,'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA' , levenshtein)
        .set(ProductionByYearAndType, type, on.going.scientific.initiation.by.year)

    } else if (type=='incluir_orientacao_em_andamento_doutorado' && default[[type]]=="sim"){

        on.going.theses.by.year <- Execute(ids, data, conf.JSON, type.production,'ORIENTACAO_EM_ANDAMENTO_DOUTORADO' , levenshtein)
        .set(ProductionByYearAndType, type, on.going.theses.by.year)

    } else if (type == 'incluir_outras_orientacoes_concluidas' && default[[type]]=="sim"){

       advised.others.by.year <- Execute(ids, data, conf.JSON, type.production ,'OUTRAS_ORIENTACOES_CONCLUIDAS' , levenshtein)
       .set(ProductionByYearAndType, type, advised.others.by.year)

   } else if (type=='incluir_orientacao_concluida_doutorado' && default[[type]]=="sim"){

     advised.theses.by.year <- Execute(ids, data, conf.JSON, type.production ,'ORIENTACAO_CONCLUIDA_DOUTORADO' , levenshtein)
     .set(ProductionByYearAndType, type, advised.theses.by.year )
     }

   }

 }
     

ProductionByYearAndType


}
