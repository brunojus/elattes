#' @title CreateGraphC
#'
#' @description This function convert a set of Lattes CV XML files to a hash table indexed by #'id_LATTES each researcher
#'
#' @param json of profiles and publications
#'
#' @return return graphs
#'
#' @examples CreateGraph(profiles=profile,publications=publications)
#'
#' @export CreateGraph



CreateGraph <- function(profiles,publications){
  perfil <- fromJSON(profiles)
  #pegando nome e id do arquivo profile
  id.name <- as.data.frame(cbind(names(perfil),
                                 (unlist(sapply(perfil, function(x) (x$nome))))))
  #organizando os nomes e formato
  colnames(id.name) <- c("idLattes", "name")
  id.name$name <- as.character(id.name$name)
  id.name$idLattes <- as.character(id.name$idLattes)

  #read publication json file
  public <- fromJSON(publications)
  #extract autores-endogenos in data-frame format
  pub.ls <- data.frame()
  for (i in 1:length(public[[1]]))
    pub.ls <- rbind(pub.ls, (public$PERIODICO[[i]]))
  #pegando a ultima coluna do data.frame
  #e retirando os ids duplicados em uma mesma linha
  pub.ls <- sapply(pub.ls[,length(pub.ls)], unique)
  node.id <- unique(unlist(pub.ls))
  #pegando apenas as linhas com dois ou mais IDs
  pub.ls <- pub.ls[sapply(pub.ls, length) > 1]

  #criacao de rede
  #criando o arquivo de linhas
  edge.list <- data.frame()
  for(i in 1:length(pub.ls)){
    edge.list <- rbind(edge.list,
                       (t(as.data.frame(combn(pub.ls[[i]], 2)))))
  }

  #arquivo de linhas com peso
  edge.list.weight <- data.frame()
  #edge.list.weight <- edge.list %>% unite(c(V1, V2), sep = ";") %>%
  edge.list.weight <- edge.list %>% unite("V1_V2",c(V1, V2), sep = ";") %>%
    group_by_at(1) %>% tally() %>% separate(1, into = c("V1", "V2"), sep=";")
  colnames(edge.list.weight) <- c("from", "to", "weight")

  #criando o arquivo de nos
  node.list <- as.data.frame(node.id)
  colnames(node.list) <- "idLattes"
  node.list$idLattes <- as.character(node.list$idLattes)
  node.list <- node.list %>% left_join(id.name, by = "idLattes")

  #criando o arquivo de rede
  g <- graph_from_data_frame(edge.list.weight, directed=FALSE, vertices=node.list)
  V(g)$id <- node.list$idLattes
  V(g)$degree <- degree(g)
  V(g)$betweenness <- betweenness(g, normalized = TRUE)
  V(g)$closeness <- closeness(g, normalized = TRUE)
  V(g)$eigen <- eigen_centrality(g)$vector
  V(g)$cluster <- cluster_leading_eigen(g)$membership


  g
}

