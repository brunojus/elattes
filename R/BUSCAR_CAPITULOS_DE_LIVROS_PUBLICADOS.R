

BUSCAR_CAPITULOS_DE_LIVROS_PUBLICADOS<-function(doc){
	

CAPITULOS_DE_LIVROS_PUBLICADOS<-list()


CapitulosDeLivrosPublicados=xmlRoot(doc)[["PRODUCAO-BIBLIOGRAFICA"]][["LIVROS-E-CAPITULOS"]][["CAPITULOS-DE-LIVROS-PUBLICADOS"]][c("CAPITULO-DE-LIVRO-PUBLICADO")]




if(length(CapitulosDeLivrosPublicados) != 0) { 

	for(capitulo in CapitulosDeLivrosPublicados){
	 
		DADOS_BASICOS_DO_CAPITULO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(capitulo[["DADOS-BASICOS-DO-CAPITULO"]])
		  DADOS_BASICOS_DO_CAPITULO$TITULO_DO_CAPITULO_DO_LIVRO<-ValidateXmlAttr(atributos,"TITULO-DO-CAPITULO-DO-LIVRO")

		  DADOS_BASICOS_DO_CAPITULO$TIPO <-ValidateXmlAttr(atributos,"TIPO")

                  DADOS_BASICOS_DO_CAPITULO$ANO <- ValidateXmlAttr(atributos,"ANO") 

	
		  DADOS_BASICOS_DO_CAPITULO$DOI<- ValidateXmlAttr(atributos,"DOI")


		  DADOS_BASICOS_DO_CAPITULO$PAIS_DE_PUBLICACAO<-ValidateXmlAttr(atributos,"PAIS-DE-PUBLICACAO")


		DETALHAMENTO_DO_CAPITULO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(capitulo[["DETALHAMENTO-DO-CAPITULO"]])
		  DETALHAMENTO_DO_CAPITULO$ORGANIZADORES<-ValidateXmlAttr(atributos,"ORGANIZADORES")

 		  DETALHAMENTO_DO_CAPITULO$PAGINA_INICIAL<-ValidateXmlAttr(atributos,"PAGINA-INICIAL")

		  DETALHAMENTO_DO_CAPITULO$PAGINA_FINAL<-ValidateXmlAttr(atributos,"PAGINA-FINAL")

		  DETALHAMENTO_DO_CAPITULO$ISBN<-ValidateXmlAttr(atributos,"ISBN")

                  DETALHAMENTO_DO_CAPITULO$NUMERO_DA_EDICAO_REVISAO<- ValidateXmlAttr(atributos,"NUMERO-DA-EDICAO-REVISAO")

		 DETALHAMENTO_DO_CAPITULO$NOME_DA_EDITORA<- ValidateXmlAttr(atributos,"NOME-DA-EDITORA")

		 DETALHAMENTO_DO_CAPITULO$TITULO_DO_LIVRO<- ValidateXmlAttr(atributos,"TITULO-DO-LIVRO")



	       ListaDeautores= capitulo[c("AUTORES") ]
		    
	       AUTORES<-list()
	       for(autor in ListaDeautores){
		      autorx<-new.env(parent=emptyenv())
		      autorx$NOME_COMPLETO_DO_AUTOR<-Validate(autor, "NOME-COMPLETO-DO-AUTOR")
		      autorx$ORDEM_DE_AUTORIA<-Validate(autor, "ORDEM-DE-AUTORIA") 
		      autorx$NOME_PARA_CITACAO<-Validate(autor, "NOME-PARA-CITACAO")
		      AUTORES<-c(AUTORES, list(autorx))
	      }



	       CAPITULO_DE_LIVRO_PUBLICADO <-new.env(parent=emptyenv())

	      	 CAPITULO_DE_LIVRO_PUBLICADO$AUTORES<-AUTORES
	      	 CAPITULO_DE_LIVRO_PUBLICADO$DETALHAMENTO_DO_CAPITULO<-DETALHAMENTO_DO_CAPITULO
	      	 CAPITULO_DE_LIVRO_PUBLICADO$DADOS_BASICOS_DO_CAPITULO<-DADOS_BASICOS_DO_CAPITULO
	         CAPITULO_DE_LIVRO_PUBLICADO$TIPO<-"CAPITULO_DE_LIVRO_PUBLICADO"
	  
	       CAPITULOS_DE_LIVROS_PUBLICADOS<-c(CAPITULOS_DE_LIVROS_PUBLICADOS,list(CAPITULO_DE_LIVRO_PUBLICADO))
	  
	  }
	  Resultado<-CAPITULOS_DE_LIVROS_PUBLICADOS
 } else{ Resultado<-NULL}

Resultado

}

