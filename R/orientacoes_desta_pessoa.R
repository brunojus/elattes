#' @title OrientacoesDestaPessoa
#'
#' 
#'
#' @param id a list of Lattes CV XML files
#' @param orientacoesPorAno
#' 
#'
#'
#'
#' @export OrientacoesDestaPessoa


OrientacoesDestaPessoa<-function(id,orientacoesPorAno){
	
	orientacao_concluida_mestrado <-list() 
	orientacao_concluida_doutorado <-list() 
 	outras_orientacoes_concluidas <-list() 
	orientacao_concluida_pos_doutorado <-list() 
	orientacao_em_andamento_mestrado <-list() 
	orientacao_em_andamento_doutorado <- list()
	orientacao_em_andamento_iniciacao_cientifica <- list()
	orientacao_em_andamento_tcc <- list()

	producao<-hash()

	tipos<-keys(orientacoesPorAno) 


	for (tipo in tipos){
		anos<-keys(orientacoesPorAno[[tipo]]) 
	  	for(ano in anos){
			for(item in orientacoesPorAno[[tipo]][[ano]]){
				for (num in item$authorsEndogenous){
					if(id==num){
				if(tipo=='incluir_orientacao_concluida_mestrado'){
		orientacao_concluida_mestrado<-c(orientacao_concluida_mestrado, list(item))
			}else if(tipo=='incluir_orientacao_concluida_doutorado')
						{
						  orientacao_concluida_doutorado<-c(orientacao_concluida_doutorado, list(item))
						} else if(tipo=='incluir_outras_orientacoes_concluidas')
						{
						  outras_orientacoes_concluidas<-c(outras_orientacoes_concluidas, list(item))
						} else if(tipo=='incluir_orientacao_concluida_pos_doutorado')
						{
						  orientacao_concluida_pos_doutorado<-c(orientacao_concluida_pos_doutorado, list(item))
						} else if(tipo=='incluir_orientacao_em_andamento_mestrado')
						{
						  orientacao_em_andamento_mestrado<-c(orientacao_em_andamento_mestrado, list(item))
						} else if(tipo=='incluir_orientacao_em_andamento_doutorado')
						{
						  orientacao_em_andamento_doutorado<-c(orientacao_em_andamento_doutorado, list(item))
						}else if(tipo=='incluir_orientacao_em_andamento_iniciacao_cientifica')
						{
						  orientacao_em_andamento_iniciacao_cientifica<-c(orientacao_em_andamento_iniciacao_cientifica, list(item))
						}else if(tipo=='incluir_orientacao_em_andamento_tcc')
						{
						  orientacao_em_andamento_tcc<-c(orientacao_em_andamento_tcc, list(item))
						}
					}
				}
	
			}
		}

	}
	
 .set(producao, 'ORIENTACAO_CONCLUIDA_MESTRADO', orientacao_concluida_mestrado)
 .set(producao, 'ORIENTACAO_CONCLUIDA_DOUTORADO', orientacao_concluida_doutorado)
 .set(producao, 'OUTRAS_ORIENTACOES_CONCLUIDAS', outras_orientacoes_concluidas)
.set(producao, 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO', orientacao_concluida_pos_doutorado)
.set(producao, 'ORIENTACAO_EM_ANDAMENTO_MESTRADO', orientacao_em_andamento_mestrado)
.set(producao, 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO', orientacao_em_andamento_doutorado)
.set(producao, 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA', orientacao_em_andamento_iniciacao_cientifica)
.set(producao, 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO', orientacao_em_andamento_tcc)

	producao

}
