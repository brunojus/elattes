#' @title GetResearchersByArea
#'
#' 
#'
#' @param ids.researcher a list of Lattes CV XML files
#' @param production.by.researcher
#'
#' @export GetResearchersByArea

GetResearchersByArea <- function(ids.researcher, production.by.researcher){

out.researchers.by.area<-hash()

temp<-hash()

names<-GetAreasName(ids.researcher, production.by.researcher)

for (name in names){

  aux<-list()
  for(id in ids.researcher){

      
     for(area.researcher in production.by.researcher[[id]]$DADOS_GERAIS$AREAS_DE_ATUACAO){
 	 if (name == area.researcher$NOME_DA_AREA_DO_CONHECIMENTO){
		aux<-c(aux,list(id))
	 }
     }
     
   
   }
   .set(out.researchers.by.area, name, aux)
   
}



aux3<-hash()
for(name in names ){
  aux2<-hash()
  for(id in  out.researchers.by.area[[name]]){
     .set(aux2, id, id)
  }

  .set(aux3, name, keys(aux2) )
      
}


return(aux3)
}






