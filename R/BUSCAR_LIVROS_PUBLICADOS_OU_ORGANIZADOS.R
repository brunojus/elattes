

BUSCAR_LIVROS_PUBLICADOS_OU_ORGANIZADOS<-function(doc){
	
 #doc = xmlParse("/home/antonio/Fiocruz/files/0311082095842942.xml")

LIVROS_PUBLICADOS_OU_ORGANIZADOS<-list()


LivrosPublicadosOuOrganizados=xmlRoot(doc)[["PRODUCAO-BIBLIOGRAFICA"]][["LIVROS-E-CAPITULOS"]][["LIVROS-PUBLICADOS-OU-ORGANIZADOS"]][c("LIVRO-PUBLICADO-OU-ORGANIZADO")]

if(length(LivrosPublicadosOuOrganizados) != 0) { 
	for(livro in LivrosPublicadosOuOrganizados){
	 
		DADOS_BASICOS_DO_LIVRO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(livro[["DADOS-BASICOS-DO-LIVRO"]])
		  DADOS_BASICOS_DO_LIVRO$TITULO_DO_LIVRO<-ValidateXmlAttr(atributos,"TITULO-DO-LIVRO")

		  DADOS_BASICOS_DO_LIVRO$TIPO <-ValidateXmlAttr(atributos,"TIPO")

		  DADOS_BASICOS_DO_LIVRO$NATUREZA <- ValidateXmlAttr(atributos,"NATUREZA")

                  DADOS_BASICOS_DO_LIVRO$ANO <- ValidateXmlAttr(atributos,"ANO")

                  DADOS_BASICOS_DO_LIVRO$PAIS_DE_PUBLICACAO <- ValidateXmlAttr(atributos,"PAIS-DE-PUBLICACAO")

		  DADOS_BASICOS_DO_LIVRO$IDIOMA <- ValidateXmlAttr(atributos,"IDIOMA")


		  DADOS_BASICOS_DO_LIVRO$DOI<- ValidateXmlAttr(atributos,"DOI")
	


		DETALHAMENTO_DO_LIVRO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(livro[["DETALHAMENTO-DO-LIVRO"]])

		  DETALHAMENTO_DO_LIVRO$NUMERO_DE_VOLUMES<-ValidateXmlAttr(atributos,"NUMERO-DE-VOLUMES")

 		  DETALHAMENTO_DO_LIVRO$NUMERO_DE_PAGINAS<- ValidateXmlAttr(atributos,"NUMERO-DE-PAGINAS")

		  DETALHAMENTO_DO_LIVRO$ISBN<-ValidateXmlAttr(atributos,"ISBN")


                  DETALHAMENTO_DO_LIVRO$NUMERO_DA_EDICAO_REVISAO<- ValidateXmlAttr(atributos,"NUMERO-DA-EDICAO-REVISAO")

		 DETALHAMENTO_DO_LIVRO$NOME_DA_EDITORA<- ValidateXmlAttr(atributos,"NOME-DA-EDITORA")

	       ListaDeautores= livro[c("AUTORES") ]
		    
	       AUTORES<-list()
	       for(autor in ListaDeautores){
		      autorx<-new.env(parent=emptyenv())
		      autorx$NOME_COMPLETO_DO_AUTOR<-Validate(autor, "NOME-COMPLETO-DO-AUTOR")
		      autorx$ORDEM_DE_AUTORIA<-Validate(autor, "ORDEM-DE-AUTORIA") 
                      autorx$NOME_PARA_CITACAO<-Validate(autor, "NOME-PARA-CITACAO")
		      AUTORES<-c(AUTORES, list(autorx))
	      }



	       LIVRO_PUBLICADO_OU_ORGANIZADO<-new.env(parent=emptyenv())

	      	 LIVRO_PUBLICADO_OU_ORGANIZADO$AUTORES<-AUTORES
	      	 LIVRO_PUBLICADO_OU_ORGANIZADO$DETALHAMENTO_DO_LIVRO<-DETALHAMENTO_DO_LIVRO
	      	 LIVRO_PUBLICADO_OU_ORGANIZADO$DADOS_BASICOS_DO_LIVRO<-DADOS_BASICOS_DO_LIVRO
	        
                 LIVRO_PUBLICADO_OU_ORGANIZADO$IDLATTES<-NULL
	  
	       LIVROS_PUBLICADOS_OU_ORGANIZADOS<-c(LIVROS_PUBLICADOS_OU_ORGANIZADOS,list(LIVRO_PUBLICADO_OU_ORGANIZADO))
	  
	  }
	  Resultado<-LIVROS_PUBLICADOS_OU_ORGANIZADOS
 } else{ Resultado<-NULL}

Resultado

}

