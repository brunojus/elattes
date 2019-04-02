BUSCAR_DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA<-function(doc){
	
 #doc = xmlParse("/home/antonio/Fiocruz/files/9788199690491510.xml")

DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA<-list()


DemaisTiposDeProducaoBibliografica=xmlRoot(doc)[["PRODUCAO-BIBLIOGRAFICA"]][["DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA"]][c("OUTRA-PRODUCAO-BIBLIOGRAFICA")]




if(length(DemaisTiposDeProducaoBibliografica) != 0) { 

	for(outraProducao in DemaisTiposDeProducaoBibliografica){
	 
		DADOS_BASICOS_DE_OUTRA_PRODUCAO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(outraProducao[["DADOS-BASICOS-DE-OUTRA-PRODUCAO"]])

		  DADOS_BASICOS_DE_OUTRA_PRODUCAO$NATUREZA<-ValidateXmlAttr(atributos,"NATUREZA")

		  DADOS_BASICOS_DE_OUTRA_PRODUCAO$TITULO <-ValidateXmlAttr(atributos,"TITULO")

                  DADOS_BASICOS_DE_OUTRA_PRODUCAO$ANO <-ValidateXmlAttr(atributos,"ANO")

 		  DADOS_BASICOS_DE_OUTRA_PRODUCAO$DOI<-ValidateXmlAttr(atributos,"DOI")

		  DADOS_BASICOS_DE_OUTRA_PRODUCAO$PAIS_DE_PUBLICACAO<-ValidateXmlAttr(atributos,"PAIS-DE-PUBLICACAO")


		DETALHAMENTO_DE_OUTRA_PRODUCAO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(outraProducao[["DETALHAMENTO-DE-OUTRA-PRODUCAO"]])

		DETALHAMENTO_DE_OUTRA_PRODUCAO$NUMERO_DE_PAGINAS<-ValidateXmlAttr(atributos,"NUMERO-DE-PAGINAS")
		DETALHAMENTO_DE_OUTRA_PRODUCAO$EDITORA<-ValidateXmlAttr(atributos,"EDITORA")
		DETALHAMENTO_DE_OUTRA_PRODUCAO$CIDADE_DA_EDITORA<-ValidateXmlAttr(atributos,"CIDADE-DA-EDITORA")
		 


	       ListaDeautores= outraProducao[c("AUTORES") ]
		    
	       AUTORES<-list()
	       for(autor in ListaDeautores){
		      autorx<-new.env(parent=emptyenv())
		      autorx$NOME_COMPLETO_DO_AUTOR<-Validate(autor, "NOME-COMPLETO-DO-AUTOR")
		      autorx$NOME_PARA_CITACAO<-Validate(autor, "NOME-PARA-CITACAO") 
		      autorx$ORDEM_DE_AUTORIA<-Validate(autor, "ORDEM-DE-AUTORIA") 
		      AUTORES<-c(AUTORES, list(autorx))
	      }



	       OUTRA_PRODUCAO_BIBLIOGRAFICA <-new.env(parent=emptyenv())

	      	  OUTRA_PRODUCAO_BIBLIOGRAFICA$AUTORES<-AUTORES
	      	  OUTRA_PRODUCAO_BIBLIOGRAFICA$DETALHAMENTO_DE_OUTRA_PRODUCAO<-DETALHAMENTO_DE_OUTRA_PRODUCAO
	      	  OUTRA_PRODUCAO_BIBLIOGRAFICA$DADOS_BASICOS_DE_OUTRA_PRODUCAO<-DADOS_BASICOS_DE_OUTRA_PRODUCAO
	   
	  
	       DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA<-c(DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA,list(OUTRA_PRODUCAO_BIBLIOGRAFICA))
	  
	  }
	  Resultado<-DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA
 } else{ Resultado<-NULL}

Resultado

}

