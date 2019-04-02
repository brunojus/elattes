GetParameter <-function(path){

	parametrosGerais<-fromJSON(path, simplifyVector = FALSE)
	arquivo_de_entrada<-parametrosGerais$global$arquivo_de_entrada
	parametrosEspecificos<-fromJSON(arquivo_de_entrada, simplifyVector = FALSE)


 	 parameters<-new.env(parent=emptyenv())
	 parameters$gerais<-parametrosGerais
	 parameters$especificos<-parametrosEspecificos
 	 parameters
}
