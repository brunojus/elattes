#' @title BUSCAR_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO
#'
#' 
#'
#' @param doc a list of Lattes CV XML files
#'
#' 
#'
#'
#'
#' @export BUSCAR_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO

BUSCAR_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO<-function(doc){


ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO<-list()


Orientacoes=xmlRoot(doc)[["OUTRA-PRODUCAO"]][["ORIENTACOES-CONCLUIDAS"]][c("ORIENTACOES-CONCLUIDAS-PARA-POS-DOUTORADO") ]

if(length(Orientacoes) != 0) { 
	for(orientacao in Orientacoes){
	 
		DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO<-new.env(parent=emptyenv())
		  atributos<-xmlAttrs(orientacao[["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-POS-DOUTORADO"]])
		DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$NATUREZA<- ValidateXmlAttr(atributos,"NATUREZA")
		DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$TIPO<- ValidateXmlAttr(atributos,"TIPO")


		DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$TITULO_DO_TRABALHO<-ValidateXmlAttr(atributos,"TITULO")
                DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$ANO<-ValidateXmlAttr(atributos,"ANO")
     
		DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$PAIS<- ValidateXmlAttr(atributos,"PAIS")
	

		

		DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO<-new.env(parent=emptyenv())
		atributos<-xmlAttrs(orientacao[["DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-POS-DOUTORADO"]])

		  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$TIPO_DE_ORIENTACAO<- ValidateXmlAttr(atributos,"TIPO-DE-ORIENTACAO")

                  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$NOME_DO_ORIENTADO<- ValidateXmlAttr(atributos,"NOME-DO-ORIENTADO")

	          DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$CODIGO_INSTITUICAO<-ValidateXmlAttr(atributos,"CODIGO-INSTITUICAO")

                  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$NOME_INSTITUICAO<- ValidateXmlAttr(atributos,"NOME-DA-INSTITUICAO")

                  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$CODIGO_CURSO<- ValidateXmlAttr(atributos,"CODIGO-CURSO")
                  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$NOME_CURSO<- ValidateXmlAttr(atributos,"NOME-DO-CURSO")
		  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$FLAG_BOLSA<- ValidateXmlAttr(atributos,"FLAG-BOLSA")
		  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$CODIGO_AGENCIA_FINANCIADORA<- ValidateXmlAttr(atributos,"CODIGO-AGENCIA-FINANCIADORA")
		  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$NOME_DA_AGENCIA<- ValidateXmlAttr(atributos,"NOME-DA-AGENCIA")
		  DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO$NUMERO_ID_ORIENTADO<- ValidateXmlAttr(atributos,"NUMERO-ID-ORIENTADO")


	      		    
	
              
	     ORIENTACAO_CONCLUIDA_PARA_POS_DOUTORADO<-new.env(parent=emptyenv())

	     
	       ORIENTACAO_CONCLUIDA_PARA_POS_DOUTORADO$DETALHAMENTO<-DETALHAMENTO_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO
	       ORIENTACAO_CONCLUIDA_PARA_POS_DOUTORADO$DADOS_BASICOS<-DADOS_BASICOS_DE_ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO
	      
	       ORIENTACAO_CONCLUIDA_PARA_POS_DOUTORADO$IDLATTES<-"NULL"
	       ORIENTACAO_CONCLUIDA_PARA_POS_DOUTORADO$NOME_COMPLETO<-"NULL"

	    ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO<-c(ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO,list(ORIENTACAO_CONCLUIDA_PARA_POS_DOUTORADO))
	  
	  }
	  Resultado<-ORIENTACOES_CONCLUIDAS_PARA_POS_DOUTORADO
 } else{ Resultado<-NULL}

Resultado

}




