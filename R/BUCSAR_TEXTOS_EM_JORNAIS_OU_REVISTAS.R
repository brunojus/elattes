BUCSAR_TEXTOS_EM_JORNAIS_OU_REVISTAS<-function(doc){
	
# doc = xmlParse("/home/antonio/Fiocruz/files/9788199690491510.xml")

TEXTOS_EM_JORNAIS_OU_REVISTAS<-list()


TextosEmJornaisOuRevistas=xmlRoot(doc)[["PRODUCAO-BIBLIOGRAFICA"]][["TEXTOS-EM-JORNAIS-OU-REVISTAS"]][c("TEXTO-EM-JORNAL-OU-REVISTA")]



if(length(TextosEmJornaisOuRevistas) != 0) { 

	for(texto in TextosEmJornaisOuRevistas){
	 
		DADOS_BASICOS_DO_TEXTO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(texto[["DADOS-BASICOS-DO-TEXTO"]])


		  DADOS_BASICOS_DO_TEXTO$NATUREZA<-ValidateXmlAttr(atributos,"NATUREZA")

		  DADOS_BASICOS_DO_TEXTO$TITULO_DO_TEXTO <- ValidateXmlAttr(atributos,"TITULO-DO-TEXTO")

                  DADOS_BASICOS_DO_TEXTO$ANO_DO_TEXTO <-ValidateXmlAttr(atributos,"ANO-DO-TEXTO")

 		  DADOS_BASICOS_DO_TEXTO$DOI<-ValidateXmlAttr(atributos,"DOI")

		  DADOS_BASICOS_DO_TEXTO$PAIS_DE_PUBLICACAO<-ValidateXmlAttr(atributos,"PAIS-DE-PUBLICACAO")


		DETALHAMENTO_DO_TEXTO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(texto[["DETALHAMENTO-DO-TEXTO"]])


		  DETALHAMENTO_DO_TEXTO$TITULO_DO_JORNAL_OU_REVISTA<-ValidateXmlAttr(atributos,"TITULO-DO-JORNAL-OU-REVISTA")

 		  DETALHAMENTO_DO_TEXTO$PAGINA_INICIAL<-ValidateXmlAttr(atributos,"PAGINA-INICIAL")

		  DETALHAMENTO_DO_TEXTO$PAGINA_FINAL<-ValidateXmlAttr(atributos,"PAGINA-FINAL")

		  DETALHAMENTO_DO_TEXTO$ISSN<-ValidateXmlAttr(atributos,"ISSN")

                  DETALHAMENTO_DO_TEXTO$DATA_DE_PUBLICACAO<-ValidateXmlAttr(atributos,"DATA-DE-PUBLICACAO")

		  DETALHAMENTO_DO_TEXTO$LOCAL_DE_PUBLICACAO<-ValidateXmlAttr(atributos,"LOCAL-DE-PUBLICACAO")
		


	       ListaDeautores= texto[c("AUTORES") ]
		    
	       AUTORES<-list()
	       for(autor in ListaDeautores){
		      autorx<-new.env(parent=emptyenv())
		      autorx$NOME_COMPLETO_DO_AUTOR<-Validate(autor, "NOME-COMPLETO-DO-AUTOR")
		      autorx$NOME_PARA_CITACAO<-Validate(autor, "NOME-PARA-CITACAO")
		      autorx$ORDEM_DE_AUTORIA<-Validate(autor, "ORDEM-DE-AUTORIA") 
		      AUTORES<-c(AUTORES, list(autorx))
	      }



	       TEXTO_EM_JORNAL_OU_REVISTA <-new.env(parent=emptyenv())

	      	 TEXTO_EM_JORNAL_OU_REVISTA$AUTORES<-AUTORES
	      	 TEXTO_EM_JORNAL_OU_REVISTA$DETALHAMENTO_DO_TEXTO<-DETALHAMENTO_DO_TEXTO
	      	 TEXTO_EM_JORNAL_OU_REVISTA$DADOS_BASICOS_DO_TEXTO<-DADOS_BASICOS_DO_TEXTO
	   	 TEXTO_EM_JORNAL_OU_REVISTA$IDLATTES<-"NULL"
	  
	       TEXTOS_EM_JORNAIS_OU_REVISTAS<-c(TEXTOS_EM_JORNAIS_OU_REVISTAS,list(TEXTO_EM_JORNAL_OU_REVISTA))
	  
	  }
	  Resultado<-TEXTOS_EM_JORNAIS_OU_REVISTAS
 } else{ Resultado<-NULL}

Resultado

}

