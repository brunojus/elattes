#' @title CreateListJson
#'
#' 
#'
#' @param path.file field a list of Lattes CV XML files.
#' @param name.group field a list of Lattes CV XML files.
#' @param year.inicial field a list of Lattes CV XML files.
#' @param year.final field a list of Lattes CV XML files.
#' @param save.as.path field a list of Lattes CV XML files.
#' @param save.as.name.file field a list of Lattes CV XML files.
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


utils::globalVariables("read.table")

A_load <- function (f) {
      A <- as.matrix(read.table(f, colClasses=c("character")))
      return(A)
    }
