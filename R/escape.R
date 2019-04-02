Escape <- function(text.var){

 gsub("\\s+", " ", gsub("\\\\r|\\\\n|\\n|\\\\t|\\\"|\\\\'|\\\\", " ", text.var))

  
}



