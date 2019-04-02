GetDefualtParameterGraph <- function(){

default <- hash()

	#grafo de coautoria
 .set( default,  "incluir_artigo_em_periodico", "sim" )
 .set( default, "incluir_livro_publicado", "sim" )
 .set( default, "incluir_capitulo_de_livro_publicado", "sim" )
 .set( default, "incluir_texto_em_jornal_de_noticia", "sim" )
 .set( default, "incluir_trabalho_em_congresso", "sim" )	
 .set( default, "incluir_artigo_aceito_para_publicacao", "sim" )
 .set( default, "incluir_outro_tipo_de_producao_bibliografica", "sim" )

return(default)
}
