
BuildFilesJSON <- function(conf.setup) {
  
   

	
 ids.selected<-GetIdsSelected(conf.setup$especificos)

 files.selected<-GetXMLLattesCVFilesSelected(ids.selected, conf.setup)
	
 out.production.by.researcher<-ParserXML(files.selected)
	
 levenshtein<-4 



researchers.by.area<- ConvertDataResearcheresByAreaToJson(ids.selected, out.production.by.researcher)

 # get parameters default
 default<-GetDefaultParameters()

	
 modified.default<-ModifyDefaultParameters(default, conf.setup, "BIBLIOGRAFICA" )
 p<-GetProductionByYearAndType (conf.setup, "BIBLIOGRAFICA", modified.default, ids.selected, out.production.by.researcher, levenshtein)



 modified.default<-ModifyDefaultParameters(default, conf.setup, "ORIENTACAO" )
 o<- GetProductionByYearAndType (conf.setup, "ORIENTACAO", modified.default, ids.selected, out.production.by.researcher , levenshtein)




 profile<-ConvertDataProfileToJson(out.production.by.researcher, p, o)

 publication<-ConvertDataToJson(conf.setup, "BIBLIOGRAFICA", modified.default, p)

 advise<-ConvertDataToJson(conf.setup, "ORIENTACAO", modified.default, o)



#comente estas duas linhas -- isso retira a restrição que mencionei que complica tudo com relação a a tempo de execução

 #modified.default <- ModifyDefaultParameters(GetDefualtParameterGraph(), conf.setup, "GRAFO" )
 #p<-GetProductionByYearAndType (conf.setup, "BIBLIOGRAFICA", modified.default, ids.selected, out.production.by.researcher, levenshtein)


 graph<-ConstruirDadoGrafo(out.production.by.researcher ,p, conf.setup , modified.default )

 
 
     
 Save(advise, conf.setup$gerais$global$diretorio_de_saida, "advise.json" )
 Save(publication, conf.setup$gerais$global$diretorio_de_saida, "publication.json" )
 Save(profile, conf.setup$gerais$global$diretorio_de_saida, "profile.json" )
#comentei o grafo
 Save(graph, conf.setup$gerais$global$diretorio_de_saida, "graph.json" )
 Save(researchers.by.area, conf.setup$gerais$global$diretorio_de_saida, "researchers_by_area.json" )
}







ModifyDefaultParameters<-function(default.parameters, new.choices, type.parameter){

new.defualt<-default.parameters

if (type.parameter == "BIBLIOGRAFICA"){

types<-names(new.choices$gerais$publicacoes)

   for(type in types){
     if( new.choices$gerais$publicacoes[[type]]=='nao'){
         new.defualt[[type]] <- 'nao'
     }
   }

} else if (type.parameter =="ORIENTACAO"){

types<-names(new.choices$gerais$orientacoes)

  for(type in types){
    if( new.choices$gerais$orientacoes[[type]]=='nao'){
        new.defualt[[type]] <- 'nao'
    }
  }
} else if (type.parameter =="GRAFO"){

types<-names(new.choices$gerais$grafo_de_coautoria)

  for(type in types){
    if( new.choices$gerais$grafo_de_coautoria[[type]]=='nao'){
      new.defualt[[type]] <- 'nao'
    }
  }
}

return (new.defualt)


}






GetXMLLattesCVFilesSelected <-function (ids, configuracao){

setwd(configuracao$gerais$global$diretorio_de_armazenamento_de_cvs)
files<-list.files()
files<-GetFiles(ids,files) 
return (files)

}


GetFiles<-function(ids, files){
   
   
   new <- list()

   for (f in files){
	id<-sub('\\.xml$', '', f)
	for(item in ids){
          if(id==item){
            id<-sub('\\.xml$', '.xml', f)
	    new <- c(new, list(id))
          }
        }
     
   }
   
   new 
 }



GetIdsSelected<-function(x){
  tam<-length(x)
  ids<-list()

  for(i in 1:tam){
   for(u in x[[i]]){
      ids <- c(ids, list(u$id))
   }
  
  }

  return(ids)
}


