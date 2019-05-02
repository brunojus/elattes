#' @title ConvertDataResearcheresByAreaToJson
#'
#' 
#'
#' @param ids, production.by.researcher a list of Lattes CV XML files
#' @param production.by.researcher
#' 
#'
#'
#'
#' @export ConvertDataResearcheresByAreaToJson

ConvertDataResearcheresByAreaToJson <- function (ids, production.by.researcher){

s<-paste('{\"Areas dos pesquisadores\":{', sep=" ")
names<-GetAreasName(ids, production.by.researcher)

researchers.by.area<-GetResearchersByArea(ids, production.by.researcher)
 

for(name in names){
     s<-paste(s, sep="\"", Escape(name))
     s<-paste(s,sep='\":', build(name, researchers.by.area))
 
}

s<-substr(s, 1, nchar(s)-1)
s<-paste(s,sep=" }", " }")

return(s)   

}


build<-function(area.name, researchers.by.area){

s<-paste("[")
for(id in researchers.by.area[[area.name]]){
   element <- sprintf('"%s",',id)
   s <- paste(s, sep=" ", element)
}

s<-substr(s, 1, nchar(s)-1)
s <- paste(s, sep=" ", "],") 

}
