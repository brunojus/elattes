#' @title BUSCAR_ARTIGOS_ACEITOS_PARA_PUBLICACAO
#'
#' 
#'
#' @param doc a list of Lattes CV XML files
#'
#' 
#'
#'
#'
#' @export BUSCAR_ARTIGOS_ACEITOS_PARA_PUBLICACAO

BUSCAR_ARTIGOS_ACEITOS_PARA_PUBLICACAO<-function(doc){

#lista contendo os artigos publicados em periodicos ou revistas
ARTIGOS_PUBLICADOS<-list()

ArtigosPublicados=xmlRoot(doc)[["PRODUCAO-BIBLIOGRAFICA"]][["ARTIGOS-ACEITOS-PARA-PUBLICACAO"]][c("ARTIGO-ACEITO-PARA-PUBLICACAO") ]

if(length(ArtigosPublicados) != 0) { 

	for(artigo in ArtigosPublicados){
	 
		DADOS_BASICOS_DO_ARTIGO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(artigo[["DADOS-BASICOS-DO-ARTIGO"]])
		  DADOS_BASICOS_DO_ARTIGO$TITULO_DO_ARTIGO<-ValidateXmlAttr(atributos,"TITULO-DO-ARTIGO")

		  DADOS_BASICOS_DO_ARTIGO$NATUREZA<-ValidateXmlAttr(atributos,"NATUREZA")

                  DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO<-ValidateXmlAttr(atributos,"ANO-DO-ARTIGO")

	          DADOS_BASICOS_DO_ARTIGO$DOI<-ValidateXmlAttr(atributos,"DOI")


		DETALHAMENTO_DO_ARTIGO<-new.env(parent=emptyenv())
		atributos<-xmlAttrs(artigo[["DETALHAMENTO-DO-ARTIGO"]])
		  DETALHAMENTO_DO_ARTIGO$TITULO_DO_PERIODICO_OU_REVISTA<-ValidateXmlAttr(atributos,"TITULO-DO-PERIODICO-OU-REVISTA")

                  DETALHAMENTO_DO_ARTIGO$ISSN<-ValidateXmlAttr(atributos,"ISSN")

	          DETALHAMENTO_DO_ARTIGO$VOLUME<-ValidateXmlAttr(atributos,"VOLUME")

                  DETALHAMENTO_DO_ARTIGO$PAGINA_INICIAL<-ValidateXmlAttr(atributos,"PAGINA-INICIAL")

                  DETALHAMENTO_DO_ARTIGO$PAGINA_FINAL<-ValidateXmlAttr(atributos,"PAGINA-FINAL")


	       ListaDeautores= artigo[c("AUTORES") ]
		    
	       AUTORES<-list()
	       for(autor in ListaDeautores){
		      autorx<-new.env(parent=emptyenv())
		      autorx$NOME_COMPLETO_DO_AUTOR<-Validate(autor, "NOME-COMPLETO-DO-AUTOR")
                      autorx$NOME_PARA_CITACAO<-Validate(autor, "NOME-PARA-CITACAO")
		      autorx$ORDEM_DE_AUTORIA<-Validate(autor, "ORDEM-DE-AUTORIA") 
		      AUTORES<-c(AUTORES, list(autorx))
	      }

              
	     ARTIGO_PUBLICADO<-new.env(parent=emptyenv())

	       ARTIGO_PUBLICADO$AUTORES<-AUTORES
	       ARTIGO_PUBLICADO$DETALHAMENTO_DO_ARTIGO<-DETALHAMENTO_DO_ARTIGO
	       ARTIGO_PUBLICADO$DADOS_BASICOS_DO_ARTIGO<-DADOS_BASICOS_DO_ARTIGO
	       ARTIGO_PUBLICADO$IDLATTES<-"NULL"

	    ARTIGOS_PUBLICADOS<-c(ARTIGOS_PUBLICADOS,list(ARTIGO_PUBLICADO))
	  
	  }
	  Resultado<-ARTIGOS_PUBLICADOS
 } else{ Resultado<-NULL}

Resultado

}




