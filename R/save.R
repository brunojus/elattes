#' @title Save
#'
#' 
#'
#' @param content  a list of Lattes CV XML files
#' @param path
#' @param filename
#'
#'
#'
#' @export Save

Save<-function(content, path, filename ){
	
	end<-paste(path, sep="", filename)
	end<-paste(end, sep=" ", "Ok->gravado \n")
	cat(end)
	s<-paste(path, sep="/", filename)

	sink(s, append = FALSE)
	cat(content)
	sink()


}
