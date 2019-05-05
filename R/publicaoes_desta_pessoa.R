#' @title PublicaoesDestaPessoa
#'
#' 
#'
#' @param id  a list of Lattes CV XML files.
#' @param publicacoesPorAno field a list of Lattes CV XML files.
#' 
#'
#'
#'
#' @export PublicaoesDestaPessoa


PublicaoesDestaPessoa<-function(id,publicacoesPorAno){
	periodico<-list() 
	evento<-list()
	livro<-list()
	capitulo<-list()
	aceito<-list()
	texto<-list()
	outro<-list()
 
	producao<-hash()

	tipos<-keys(publicacoesPorAno) 
#p[['incluir_artigo_em_periodico']]

	for (tipo in tipos){
		anos<-keys(publicacoesPorAno[[tipo]]) 
	  	for(ano in anos){
			for(item in publicacoesPorAno[[tipo]][[ano]]){
				for (num in item$authorsEndogenous){
					if(id==num){
					  if(tipo=='incluir_artigo_em_periodico'){
						periodico<-c(periodico, list(item))
					  }else if(tipo=='incluir_trabalho_em_congresso')
						{
						  evento<-c(evento, list(item))
						}else if(tipo=='incluir_livro_publicado'){
							livro<-c(livro, list(item))
						}else if(tipo=='incluir_capitulo_de_livro_publicado'){
							capitulo<-c(capitulo, list(item))
						}else if(tipo=='incluir_artigo_aceito_para_publicacao'){
 							aceito<-c(aceito, list(item))
						}else if(tipo=='incluir_texto_em_jornal_de_noticia'){
							texto<-c(texto, list(item))
						} else if(tipo=='incluir_outro_tipo_de_producao_bibliografica'){
							outro<-c(outro, list(item))
						}
					 
					}
				}
	
			}
		}

	}
	
	.set(producao, 'CAPITULO_DE_LIVRO', capitulo)
	.set(producao, 'LIVRO', livro)
	.set(producao, 'EVENTO', evento)
	.set(producao, 'PERIODICO', periodico)
	.set(producao, 'ARTIGO_ACEITO', aceito)
	.set(producao, 'TEXTO_EM_JORNAIS', texto)
	.set(producao, 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA', outro)

	

	producao

}
