#' @title BUSCAR_TRABALHOS_EM_EVENTOS
#'
#' 
#'
#' @param doc a list of Lattes CV XML files
#'
#' 
#'
#'
#'
#' @export BUSCAR_TRABALHOS_EM_EVENTOS

BUSCAR_TRABALHOS_EM_EVENTOS<-function(doc){
	

TRABALHOS_EM_EVENTOS<-list()


TrabalhosEmEventos=xmlRoot(doc)[["PRODUCAO-BIBLIOGRAFICA"]][["TRABALHOS-EM-EVENTOS"]][c("TRABALHO-EM-EVENTOS") ]

if(length(TrabalhosEmEventos) != 0) { # Se Houver trabalhos em eventos, então os adicione a lista, caso contrário retorne NULL
	for(trabalho in TrabalhosEmEventos){
	 
		DADOS_BASICOS_DO_TRABALHO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(trabalho[["DADOS-BASICOS-DO-TRABALHO"]])
		  DADOS_BASICOS_DO_TRABALHO$TITULO_DO_TRABALHO<- ValidateXmlAttr(atributos,"TITULO-DO-TRABALHO")

		  DADOS_BASICOS_DO_TRABALHO$NATUREZA<-ValidateXmlAttr(atributos,"NATUREZA")

		  DADOS_BASICOS_DO_TRABALHO$ANO_DO_TRABALHO<-ValidateXmlAttr(atributos,"ANO-DO-TRABALHO")
		  DADOS_BASICOS_DO_TRABALHO$PAIS_DO_EVENTO<-ValidateXmlAttr(atributos,"PAIS-DO-EVENTO")
		  DADOS_BASICOS_DO_TRABALHO$IDIOMA<- ValidateXmlAttr(atributos,"IDIOMA")

		  DADOS_BASICOS_DO_TRABALHO$MEIO_DE_DIVULGACAO<-ValidateXmlAttr(atributos,"MEIO-DE-DIVULGACAO")

		  DADOS_BASICOS_DO_TRABALHO$FLAG_RELEVANCIA<-ValidateXmlAttr(atributos,"FLAG-RELEVANCIA")

            	  DADOS_BASICOS_DO_TRABALHO$DOI<- ValidateXmlAttr(atributos,"DOI")


		DETALHAMENTO_DO_TRABALHO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(trabalho[["DETALHAMENTO-DO-TRABALHO"]])

		  DETALHAMENTO_DO_TRABALHO$TITULO_DOS_ANAIS_OU_PROCEEDINGS<- ValidateXmlAttr(atributos,"TITULO-DOS-ANAIS-OU-PROCEEDINGS")

		DETALHAMENTO_DO_TRABALHO$CLASSIFICACAO_DO_EVENTO<-ValidateXmlAttr(atributos,"CLASSIFICACAO-DO-EVENTO")

	        DETALHAMENTO_DO_TRABALHO$NOME_DO_EVENTO<-ValidateXmlAttr(atributos,"NOME-DO-EVENTO")

                DETALHAMENTO_DO_TRABALHO$CIDADE_DO_EVENTO<-ValidateXmlAttr(atributos,"CIDADE-DO-EVENTO")

                DETALHAMENTO_DO_TRABALHO$VOLUME<-ValidateXmlAttr(atributos,"VOLUME")

 		DETALHAMENTO_DO_TRABALHO$FASCICULO<-ValidateXmlAttr(atributos,"FASCICULO")
 		DETALHAMENTO_DO_TRABALHO$SERIE<-ValidateXmlAttr(atributos,"SERIE")
		DETALHAMENTO_DO_TRABALHO$PAGINA_INICIAL<-ValidateXmlAttr(atributos,"PAGINA-INICIAL")
                DETALHAMENTO_DO_TRABALHO$PAGINA_FINAL<-ValidateXmlAttr(atributos,"PAGINA-FINAL")

 

		#DETALHAMENTO_DO_TRABALHO$NOME_DO_EVENTO_INGLES<-atributos[["NOME-DO-EVENTO-INGLES"]]

	       ListaDeautores= trabalho[c("AUTORES") ]
		    
	       AUTORES<-list()
	       for(autor in ListaDeautores){
		      autorx<-new.env(parent=emptyenv())
		      autorx$NOME_COMPLETO_DO_AUTOR<-Validate(autor, "NOME-COMPLETO-DO-AUTOR")
		      autorx$ORDEM_DE_AUTORIA<-Validate(autor, "ORDEM-DE-AUTORIA") 
		      autorx$NOME_PARA_CITACAO<-Validate(autor, "NOME-PARA-CITACAO")
		      AUTORES<-c(AUTORES, list(autorx))
	      }



	       TRABALHO_EM_EVENTOS<-new.env(parent=emptyenv())

	       TRABALHO_EM_EVENTOS$AUTORES<-AUTORES
	       TRABALHO_EM_EVENTOS$DETALHAMENTO_DO_TRABALHO<-DETALHAMENTO_DO_TRABALHO
	       TRABALHO_EM_EVENTOS$DADOS_BASICOS_DO_TRABALHO<-DADOS_BASICOS_DO_TRABALHO
	       TRABALHO_EM_EVENTOS$IDLATTES<-"NULL"
	  
	       TRABALHOS_EM_EVENTOS<-c(TRABALHOS_EM_EVENTOS,list(TRABALHO_EM_EVENTOS))
	  
	  }
	  Resultado<-TRABALHOS_EM_EVENTOS
 } else{ Resultado<-NULL}

Resultado

}

