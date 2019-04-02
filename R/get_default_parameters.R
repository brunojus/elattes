GetDefaultParameters <- function(){
	
default <- hash()
	

	#publicações
 .set( default,  "incluir_artigo_em_periodico", "sim" )
 .set( default, "incluir_livro_publicado", "sim" )
 .set( default, "incluir_capitulo_de_livro_publicado", "sim" )
 .set( default, "incluir_texto_em_jornal_de_noticia", "sim" )
 .set( default, "incluir_trabalho_em_congresso", "sim" )	
 #.set( default, "incluir_resumo_em_congresso", "sim" )
 .set( default, "incluir_artigo_aceito_para_publicacao", "sim" )
 .set( default, "incluir_outro_tipo_de_producao_bibliografica", "sim" )
	


	#orientações
 .set( default, "incluir_orientacao_concluida_pos_doutorado", "sim" )
 .set( default, "incluir_orientacao_concluida_mestrado", "sim" )
 .set( default, "incluir_orientacao_concluida_doutorado", "sim" )
 .set( default, "incluir_outras_orientacoes_concluidas", "sim" )
 .set( default, "incluir_orientacao_em_andamento_mestrado", "sim" )
 .set( default, "incluir_orientacao_em_andamento_doutorado", "sim" )
 .set( default, "incluir_orientacao_em_andamento_pos_doutorado", "sim" )
 .set( default, "incluir_orientacao_em_andamento_tcc", "sim" )
 .set( default, "incluir_orientacao_em_andamento_iniciacao_cientifica", "sim" )


return(default)
} 
