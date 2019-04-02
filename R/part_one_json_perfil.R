PartOneJsonPerfil<-function(publicacoes){


	s<-""
	tipos<-keys(publicacoes)

	  s <- paste("{", sep = "")

	for(tipo in tipos){
	  if(length(publicacoes[[tipo]])>0){
		  s <- paste(s, sep="\"", tipo)
		  s <- paste(s, sep="\":", PartTwoJsonPerfil(publicacoes, tipo))
	  }	
	}

	if (s!= "{") {s<-substr(s, 1, nchar(s)-1)}

        s <- paste(s ,sep=" ", "}")

}
