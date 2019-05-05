#' @title Escape
#'
#' 
#'
#' @param text.var a list of Lattes CV XML files.
#'
#' 
#'
#'
#'
#' @export Escape

Escape <- function(text.var){

 gsub("\\s+", " ", gsub("\\\\r|\\\\n|\\n|\\\\t|\\\"|\\\\'|\\\\", " ", text.var))

  
}



