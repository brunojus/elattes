#' @title GetAreasName
#'
#' 
#'
#' @param ids.researcher  a list of Lattes CV XML files
#' @param production.by.researcher
#' 
#'
#'
#'
#' @export GetAreasName

GetAreasName <-function(ids.researcher, production.by.researcher){
names.area<-list()


for(id in ids.researcher){
 
  for(area in production.by.researcher[[id]]$DADOS_GERAIS$AREAS_DE_ATUACAO){
     names.area<-c(names.area, list(area$NOME_DA_AREA_DO_CONHECIMENTO))
  }
}


out.areas<-hash()


for(name in names.area ){
 if(name!=""){
  .set(out.areas, name, name)
 }
      
}

names.area<-keys(out.areas)

return(names.area)
}


