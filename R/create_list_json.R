#' @title CreateListJson
#'
#' 
#'
#' @param path.file
#' @param name.group
#' @param year.inicial
#' @param save.as.path
#' @param save.as.name.file
#' @export CreateListJson

CreateListJson<-function(path.file, name.group, year.inicial, year.final, save.as.path, save.as.name.file ){

        
   A<- A_load(path.file)
    
   s<- paste ('{"', sep=name.group, '":[')
   for(i in A[,1]){
     e <- sprintf('{ "id":"%s", "periodo":[%s, %s], "nome":"%s"},', i , year.inicial , year.final, "Fulano")
     s<-paste(s,sep="", e)
   }
   
   s<-substr(s, 1, nchar(s)-1)
   s<-paste(s,sep="", '] }')
  
   Save(s, save.as.path, save.as.name.file )


}

A_load <- function (f) {
      A <- as.matrix(read.table(f, colClasses=c("character")))
      return(A)
    }
