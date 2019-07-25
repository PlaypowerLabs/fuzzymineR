#' @title Visualize Process Model
#'
#' @description  Takes a metrics mined by mine_fuzzy_model function and a number of other configurable parameters, creates a process model for it.
#'
#'
#' @param metrics A list of metrics created by mine_fuzzy_model
#' @param node_sig_threshold Numeric Value between 0 and 1 which specifies threshold level for preserving nodes in the process model. Nodes having significance value less than this will be either clustered or isolated nodes will be removed from the graph
#' @param edge_sig_threshold Numeric Value between 0 and 1 which specifies threshold to filter edges. Edges having local importance less than this threshold will be removed from the process model.
#' @param edge_sig_to_corr_ratio Numeric Value between 0  and 1 which specifies the weights for edge significance metrics and edge correlation metrics. Higher the value, more importance is given to the significance metrics.
#' @param preserve_threshold Numeric Value between 0 and 1 which is used for removing the conflicting edges.
#' @param offset_threshold Numeric Value between 0 and 1 which is used to remove less significant edge in case of conflicting edges or to determine concurrency
#' @param node_sig_weights Numeric Vector of length 2. Each value is between 0 and
#' 1 determining the weights given to frequency metric and routing metric respectively.
#' @param sig_weights Numeric vector of length 2. Each value between 0 and 1 which
#' determinesthe weights of freqency binary metric and distance metric respectively.
#' @param corr_weights Numeric vector of length 5. Each value between 0 and 1. It represents
#' weights of proximity, endpoint, originator, data type and data value correlation respectively.
#'
#'
#'
#' @details \emph{\strong{viz_fuzzy_model}} generates a fuzzy process map based
#' on the configurable parameters that are provided by the user. For creating a
#' process model, the metrics are aggregated into two types of metrics - \strong{aggregated
#' node metrics} and \strong{aggregated edge metrics}. These 2 aggregated metrics contain
#' the weighted sum of the metrics that we provide as input and which were mined by
#' the function \strong{mine_fuzzy_model}. By default, all the metrics are given equal
#' weigth but we can change as per your requirments.
#'
#' For gaining more knowledge about these aggregate metrics and what are
#' their importance please visit \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.81.1207&rep=rep1&type=pdf}{Fuzzy Mining - Adaptive Process Simplification
#' Based on Multi-Perspective Metrics}. All detailed description is provided here.
#' After reading this you will gain insights about how much weights you should
#' give to each of the metrics given your usage and event data structure.
#'
#' One of the most powerful feature of this fuzzy process model is that it is
#' \strong{fully customizable}. We can change the filtering parameters and can get
#' more simplistic graph which contains only the \strong{most significant} information.
#' Configurable filtering parameters are - \strong{node_sig_threshold}, \strong{edge_sig_threshold},
#' \strong{preserve_threshold} and \strong{offset_threshold}.
#'
#'
#' @section Graph Customization:
#' This is the most important feature of fuzzy process model which would not be
#' possible by other traditional mining algorithms. Simplification of graph takes
#' place by these 3 steps which are explained in detail so that the user can get
#' the idea about what filtering parameters they should set so that they get the
#' desired simplified process model.
#'
#' \strong{Step 1 : Removing Conflicting Edges}
#'
#' When there is an edge from A to B and also from B to A then they are said to
#' be in conflict. Now these conflicting edges can be categorised into - \strong{Concurrent edges},
#' \strong{One strong Edge and other weak edge}, \strong{length-2 loop}. For this
#' we have 2 filtering parameters - \strong{preserve threshold} and \strong{offset threshold}.
#'
#' First we calculate \emph{Relative Significance} for each of the conflicting
#' edges. This gives us the local significance of that edge with respect to others.
#' It's mathematical formulation can be given as :
#'
#' \deqn{rel(A,B)=(sig(A,B))/(2*\sum sig(A,X)) + (sig(A,B))/(2*\sum sig(X,B)) }
#' \deqn{rel(B,A)=(sig(B,A))/(2*\sum sig(B,X)) + (sig(B,A))/(2*\sum sig(X,A)) }
#'
#' rel(A,B) gives the local importance of the edge from A to B with respect to
#' all the outgoing edges from A and all the incoming edges to B. If the value of
#' rel(A,B) is higher, than the importance of that edge with respect to A and B is more.
#' Similar reasoning goes with rel(B,A).
#'
#' \emph{Case 1 :}
#' If both rel(A,B) and rel(B,A) is \strong{more} than \strong{preserve threshold} than
#' these edges form a \strong{length-2 loop}.
#'
#' \deqn{rel(A,B) \ge preserve_threshold \& rel(B,A) \ge preserve_threshold => length-2 loop }
#'
#' \emph{Case 2 :}
#' If one of the edges have relative significance \strong{less} than the \strong{preserve threshold}
#' and \strong{difference} between both the relative significance is \strong{less} than the \strong{offset
#' threshold} means that there is concurrency between both the activities. Hence we
#' remove both the edges.
#'
#' \emph{Case 3 :}
#' If one of the edges have relative significance \strong{less} than the \strong{preserve threshold}
#' and \strong{difference} between both the relative significance is \strong{more} than the \strong{offset
#' threshold} than we remove the weaker edge(one which has lower relative significance).
#'
#' \strong{Step 2 : Edge Filtering}
#'
#' In this step we first calculate relative significance for all the edges and
#' remove the edges whose \strong{relative significance} is \strong{lower} than
#' the \strong{edge_sig_threshold} parameter. At last to make sure that the
#' graph remains connected we revive single edge for the nodes whose all the edges
#' were removed due to edge filtering.
#'
#' \strong{Step 3 : Node Filtering and Clustering of nodes}
#'
#' In this step, we mark the nodes whose \strong{aggregate node significance} is
#' \strong{less} than the \strong{node_sig_threshold}. We than cluster the nodes
#' if possible and remove the isolated nodes from the process model. We preserve
#' the ordering relation of the node that is removed by transitivity.
#'
#' @seealso \link[fuzzymineR]{create_eventlog}, \link[fuzzymineR]{mine_fuzzy_model}
#'
#' @examples
#' \dontrun{
#' #' library(fuzzymineR)
#' data("artificial_loan_process")
#' log <- create_eventlog(artificial_loan_process,
#'                        case_id = "case",
#'                        activity_id = "event",
#'                        timestamp = "completeTime")
#' metrics <- mine_fuzzy_model(log)
#' viz_fuzzy_model(metrics = metrics,
#'     node_sig_threshold = 0,
#'     edge_sig_threshold = 0.3,
#'     edge_sig_to_corr_ratio = 0.75)
#' }
#'
#' @import DiagrammeR
#'
#' @export

viz_fuzzy_model <- function(metrics, node_sig_threshold = 0, edge_sig_threshold = 0.4, edge_sig_to_corr_ratio = 0.75, preserve_threshold = 0.4, offset_threshold = 0.1, node_sig_weights = c(freq = 1, routing = 1), sig_weights = c(freq = 1, dist = 1),corr_weights = c(prox = 1, end_point = 1, originator = 1, data_type = 1, data_value = 1)){

  if(length(node_sig_weights) != 2 || !is.numeric(node_sig_weights)){
    stop("Invalid length or class of node_sig_weigths")
  }

  if(length(sig_weights) != 2 || !is.numeric(sig_weights)){
    stop("Invalid length or class of sig_weights")
  }

  if(length(corr_weights) != 5 || !is.numeric(corr_weights)){
    stop("Invalid length or class of corr_weights")
  }

  if(any(!between(node_sig_weights,0,1)) || any(!between(sig_weights,0,1)) || any(!between(corr_weights,0,1))){
    stop("Invalid weight given to some metrics. node_sig_weights, sig_weigths and corr_weights should be between 0 and 1 (Both Inclusive)")
  }

  if(!between(node_sig_threshold,0,1) || !between(edge_sig_threshold,0,1) || !between(edge_sig_to_corr_ratio,0,1) ){
   stop("Invalid threshold. node_sig_threshold, edge_sig_threshold and edge_sig_to_corr_ratio should be between 0 and 1 (Both Inclusive)")
  }

  if(!is.numeric(node_sig_threshold) || !is.numeric(edge_sig_threshold) || !is.numeric(edge_sig_to_corr_ratio)){
    stop("Invalid class of node_sig_threshold or edge_sig_threshold or edge_sig_to_corr_ratio")
  }

  if(all(node_sig_weights == 0)){
    stop("Invalid node significance weights given. All the metrics can not be given zero weight")
  }

  if(all(sig_weights == 0) && all(corr_weights == 0)){
    stop("Invalid edge significance or correlation weights given. All metrics can not be given zero weight")
  }


  #calculating aggregate node significance values
  agg_node_sig = (metrics$freq_sig_unary_log)*(node_sig_weights[1]) + (metrics$routing_sig_unary_der)*(node_sig_weights[2])
  max_agg_node_sig_value = max(agg_node_sig)

  #normalized_values
  agg_node_sig = agg_node_sig /max_agg_node_sig_value

  #calculating aggregate edge significance values
  agg_edge_sig = (metrics$freq_sig_binary_log)*(sig_weights[1]) + (metrics$distance_sig_binary_der)*(sig_weights[2])
  max_agg_edge_sig_value = max(agg_edge_sig)

  #normalized values
  if(max_agg_node_sig_value != 0){
  agg_edge_sig = agg_edge_sig /max_agg_edge_sig_value
  }

  #calculating aggregate edge correlation values
  agg_edge_corr = (metrics$prox_corr_binary_log)*(corr_weights[1]) + (metrics$end_point_corr_binary_log)*(corr_weights[2]) + (metrics$data_type_corr_binary_log)*(corr_weights[4]) + (metrics$data_value_corr_binary_log)*(corr_weights[5]) + (metrics$org_corr_binary_log)*(corr_weights[3])
  max_agg_edge_corr_value = max(agg_edge_corr)

  #normalized values
  if(max_agg_edge_corr_value != 0){
  agg_edge_corr = agg_edge_corr /max_agg_edge_corr_value
  }

  #Taking weighted average of agg_edge_sig and agg_edge_corr
  agg_edge_values = (agg_edge_sig*edge_sig_to_corr_ratio) + agg_edge_corr*(1 - edge_sig_to_corr_ratio)
  max_agg_edge_value = max(agg_edge_values)

  #normalize values
  if(max_agg_edge_value != 0){
  agg_edge_values = agg_edge_values /max_agg_edge_value
  }
  #Simplification step-1 removing all conflicting edges
  agg_edge_values_1 <- agg_edge_values

  for(i in 2:nrow(agg_edge_values)){

    for(j in 1:(i-1)){

      if(agg_edge_values[i,j]>0 && agg_edge_values[j,i]>0){

        rel_i_j = 0.5*((agg_edge_values[i,j]/sum(agg_edge_values[i,])) + (agg_edge_values[i,j]/sum(agg_edge_values[,j])))
        rel_j_i = 0.5*((agg_edge_values[j,i]/sum(agg_edge_values[j,])) + (agg_edge_values[j,i]/sum(agg_edge_values[,i])))

        if((rel_i_j < preserve_threshold && rel_j_i > preserve_threshold) || (rel_i_j > preserve_threshold && rel_j_i < preserve_threshold) || (rel_i_j < preserve_threshold && rel_j_i < preserve_threshold)  ) {

          if(abs(rel_i_j - rel_j_i) > offset_threshold){

            if(rel_i_j > rel_j_i){
              agg_edge_values_1[j,i] = 0
            }

            else{
              agg_edge_values_1[i,j] = 0
            }

          }

          else{
            agg_edge_values_1[i,j]=0
            agg_edge_values_1[j,i]=0
          }


        }

      }

    }
  }

  #Simplification step-2 --> Edge Filtering

  outSig = agg_edge_values_1
  inSig = agg_edge_values_1

  for(i in 1:nrow(agg_edge_values_1)){
    if(max(agg_edge_values_1[i,]) != 0 && all(agg_edge_values_1[i,]>=0)) {
      outSig[i,] = agg_edge_values_1[i,]/sum(agg_edge_values_1[i,])
    }
  }

  for(i in 1:ncol(agg_edge_values_1)){
    if(max(agg_edge_values_1[,i]) != 0 && all(agg_edge_values_1[,i]>=0)) {
      inSig[,i] = agg_edge_values_1[,i]/sum(agg_edge_values_1[,i])
    }
  }

  agg_local_sig <- 0.5*(inSig + outSig)
  agg_edge_values_2 <- ifelse(agg_local_sig<edge_sig_threshold,0,agg_local_sig)


  #To make sure that graph is connected

  for(i in 1:nrow(agg_edge_values_2)) {

    if(all(agg_edge_values_2[i,-i] == 0) && any(agg_local_sig[i,-i] != 0)) {
      agg_edge_values_2[i,names(which.max(agg_local_sig[i,-i]))] = max(agg_local_sig[i,-i])
    }

    if(all(agg_edge_values_2[-i,i] == 0) && any(agg_local_sig[-i,i] != 0)) {
      agg_edge_values_2[names(which.max(agg_local_sig[-i,i])),i] = max(agg_local_sig[-i,i])
    }

  }

  #Simplification step-3 --> Clustering of Nodes!!
  agg_edge_values_3 <- matrix(nrow = nrow(agg_edge_values_2), ncol = ncol(agg_edge_values_2))
  rownames(agg_edge_values_3) <- rownames(agg_edge_values_2)
  colnames(agg_edge_values_3) <- colnames(agg_edge_values_2)
  for(i in 1:nrow(agg_edge_values_2)){
    for(j in 1:ncol(agg_edge_values_2)){
      if(agg_edge_values_2[i,j] == 0){
        agg_edge_values_3[i,j] = agg_edge_values_2[i,j]
      }
      else{
        agg_edge_values_3[i,j] = agg_edge_values[i,j]
      }

    }
  }


  #Task-1 --> Find Victims and convert them to cluster nodes

  nodes_abv_thresh <- agg_node_sig[agg_node_sig>=node_sig_threshold]
  victims <- agg_node_sig[agg_node_sig<node_sig_threshold]
  c_id = 1
  cluster_node <- list()
  clusters <- c()
  for(node in names(victims)) {

    max_pre_ind = which.max(agg_edge_values_3[,node])
    max_suc_ind = which.max(agg_edge_values_3[node,])

    #maximum correlated is a predecessor
    if(agg_edge_values_3[max_pre_ind,node] >= agg_edge_values_3[node,max_suc_ind]) {
      neighbour = rownames(agg_edge_values_3)[max_pre_ind]
      if(neighbour %in% clusters) {
        #creating new values
        new_in_edges = agg_edge_values_3[,node] + agg_edge_values_3[,neighbour]
        new_out_edges = agg_edge_values_3[node,] + agg_edge_values_3[neighbour,]
        new_node = paste0("Cluster_",c_id)
        c_id = c_id+1
        last_value = agg_edge_values_3[node,neighbour]

        #removing old
        curr_nodes = rownames(agg_edge_values_3)
        remove_nodes = c(node,neighbour)
        remove = !(curr_nodes %in% remove_nodes)
        agg_edge_values_3 = agg_edge_values_3[remove,remove]
        new_in_edges = new_in_edges[remove]
        new_out_edges = new_out_edges[remove]
        clusters = clusters[clusters != neighbour]
        curr_nodes = curr_nodes[remove]

        #adding new
        new_out_edges[length(new_out_edges)+1] = last_value
        agg_edge_values_3 = cbind(agg_edge_values_3,new_in_edges)
        agg_edge_values_3 = rbind(agg_edge_values_3,new_out_edges)
        curr_nodes[length(curr_nodes)+1] = new_node
        rownames(agg_edge_values_3) = curr_nodes
        colnames(agg_edge_values_3) = curr_nodes
        clusters[length(clusters)+1] = new_node
        cluster_node[[new_node]] = c(cluster_node[[neighbour]], node)
        cluster_node = cluster_node[clusters]

      }

      else{
        curr_nodes = rownames(agg_edge_values_3)
        new_node = paste0("Cluster_",c_id)
        c_id = c_id+1
        curr_nodes = replace(curr_nodes, curr_nodes == node, new_node)
        rownames(agg_edge_values_3) = curr_nodes
        colnames(agg_edge_values_3) = curr_nodes
        clusters[length(clusters)+1] = new_node
        cluster_node[[new_node]] = node

      }

    }

    else {
      neighbour = colnames(agg_edge_values_3)[max_suc_ind]
      if(neighbour %in% clusters) {
        #creating new values
        new_in_edges = agg_edge_values_3[,node] + agg_edge_values_3[,neighbour]
        new_out_edges = agg_edge_values_3[node,] + agg_edge_values_3[neighbour,]
        new_node = paste0("Cluster_",c_id)
        c_id = c_id+1
        last_value = agg_edge_values_3[neighbour,node]

        #removing old
        curr_nodes = rownames(agg_edge_values_3)
        remove_nodes = c(node,neighbour)
        remove = !(curr_nodes %in% remove_nodes)
        agg_edge_values_3 = agg_edge_values_3[remove,remove]
        new_in_edges = new_in_edges[remove]
        new_out_edges = new_out_edges[remove]
        clusters = clusters[clusters != neighbour]
        curr_nodes = curr_nodes[remove]

        #adding new
        new_out_edges[length(new_out_edges)+1] = last_value
        agg_edge_values_3 = cbind(agg_edge_values_3,new_in_edges)
        agg_edge_values_3 = rbind(agg_edge_values_3,new_out_edges)
        curr_nodes[length(curr_nodes)+1] = new_node
        rownames(agg_edge_values_3) = curr_nodes
        colnames(agg_edge_values_3) = curr_nodes
        clusters[length(clusters)+1] = new_node
        cluster_node[[new_node]] = c(cluster_node[[neighbour]], node)
        cluster_node = cluster_node[clusters]

      }

      else {
        curr_nodes = rownames(agg_edge_values_3)
        new_node = paste0("Cluster_",c_id)
        c_id = c_id+1
        curr_nodes = replace(curr_nodes, curr_nodes == node, new_node)
        rownames(agg_edge_values_3) = curr_nodes
        colnames(agg_edge_values_3) = curr_nodes
        clusters[length(clusters)+1] = new_node
        cluster_node[[new_node]] = node

      }
    }
  }
  #Task-2 --> Merge Clusters
  node_id = 1
  while (node_id <= length(clusters)) {

    node = clusters[node_id]

    pred = names(agg_edge_values_3[agg_edge_values_3[,node]>0, node])
    all_pred_cluster = all(pred %in% clusters)
    all_pred_cluster = ifelse(is.null(pred),FALSE,all_pred_cluster)

    if(all_pred_cluster) {
      max_pre_ind = which.max(agg_edge_values_3[,node])
      neighbour = rownames(agg_edge_values_3)[max_pre_ind]

      #creating new values
      new_in_edges = agg_edge_values_3[,node] + agg_edge_values_3[,neighbour]
      new_out_edges = agg_edge_values_3[node,] + agg_edge_values_3[neighbour,]
      new_node = paste0("Cluster_",c_id)
      c_id = c_id+1
      last_value = agg_edge_values_3[node,neighbour]

      #removing old
      curr_nodes = rownames(agg_edge_values_3)
      remove_nodes = c(node,neighbour)
      remove = !(curr_nodes %in% remove_nodes)
      agg_edge_values_3 = agg_edge_values_3[remove,remove]
      new_in_edges = new_in_edges[remove]
      new_out_edges = new_out_edges[remove]
      clusters = clusters[clusters != neighbour & clusters!= node]
      curr_nodes = curr_nodes[remove]

      #adding new
      new_out_edges[length(new_out_edges)+1] = last_value
      agg_edge_values_3 = cbind(agg_edge_values_3,new_in_edges)
      agg_edge_values_3 = rbind(agg_edge_values_3,new_out_edges)
      curr_nodes[length(curr_nodes)+1] = new_node
      rownames(agg_edge_values_3) = curr_nodes
      colnames(agg_edge_values_3) = curr_nodes
      clusters[length(clusters)+1] = new_node
      cluster_node[[new_node]] = c(cluster_node[[neighbour]], cluster_node[[node]])
      cluster_node = cluster_node[clusters]
      node = new_node
    }

    succ = names(agg_edge_values_3[node, agg_edge_values_3[node,]>0])
    all_succ_cluster = all(succ %in% clusters)
    all_succ_cluster = ifelse(is.null(succ),FALSE,all_succ_cluster)

    if(all_succ_cluster){

      max_suc_ind = which.max(agg_edge_values_3[node,])
      neighbour = colnames(agg_edge_values_3)[max_suc_ind]

      #creating new values
      new_in_edges = agg_edge_values_3[,node] + agg_edge_values_3[,neighbour]
      new_out_edges = agg_edge_values_3[node,] + agg_edge_values_3[neighbour,]
      new_node = paste0("Cluster_",c_id)
      c_id = c_id+1
      last_value = agg_edge_values_3[neighbour,node]

      #removing old
      curr_nodes = rownames(agg_edge_values_3)
      remove_nodes = c(node,neighbour)
      remove = !(curr_nodes %in% remove_nodes)
      agg_edge_values_3 = agg_edge_values_3[remove,remove]
      new_in_edges = new_in_edges[remove]
      new_out_edges = new_out_edges[remove]
      clusters = clusters[clusters != neighbour & clusters!= node]
      curr_nodes = curr_nodes[remove]

      #adding new
      new_out_edges[length(new_out_edges)+1] = last_value
      agg_edge_values_3 = cbind(agg_edge_values_3,new_in_edges)
      agg_edge_values_3 = rbind(agg_edge_values_3,new_out_edges)
      curr_nodes[length(curr_nodes)+1] = new_node
      rownames(agg_edge_values_3) = curr_nodes
      colnames(agg_edge_values_3) = curr_nodes
      clusters[length(clusters)+1] = new_node
      cluster_node[[new_node]] = c(cluster_node[[neighbour]], cluster_node[[node]])
      cluster_node = cluster_node[clusters]

    }

    if(!all_pred_cluster && !all_succ_cluster){

      node_id = node_id+1
    }
  }



  #Task-3 --> Remove Clusters with only one node

  for (node in names(cluster_node)) {

    if(length(cluster_node[[node]]) == 1) {


      all_outs = colnames(agg_edge_values_3)[agg_edge_values_3[node,]>0]
      pred = rownames(agg_edge_values_3)[agg_edge_values_3[,node]>0]

      for(n in all_outs){
        for(p in pred){
          agg_edge_values_3[p,n] = (agg_edge_values_3[p,n] + (agg_edge_values_3[p,node]+agg_edge_values_3[node,n])/2 ) / 2

        }
      }


      agg_edge_values_3 = agg_edge_values_3[rownames(agg_edge_values_3) != node, colnames(agg_edge_values_3) != node]

   }

  }

  #Making compatible for creating graph
  agg_edge_values_3 = agg_edge_values_3/max(agg_edge_values_3)
  outSig = agg_edge_values_3

  for(i in 1:nrow(agg_edge_values_3)){
    if(max(agg_edge_values_3[i,]) != 0 && all(agg_edge_values_3[i,]>=0)) {
      outSig[i,] = agg_edge_values_3[i,]/sum(agg_edge_values_3[i,])
    }
  }

  edge_labels_metrics <- outSig*100

  from_id = c()
  to_id = c()
  edge_labels = c()
  edge_width = c()
  for(i in 1:nrow(agg_edge_values_3)){
    for(j in 1:ncol(agg_edge_values_3)){
      if(agg_edge_values_3[i,j] > 0){
        from_id[length(from_id)+1] <- i
        to_id[length(to_id)+1] <- j
        edge_labels[length(edge_labels)+1] <- edge_labels_metrics[i,j]
        edge_width[length(edge_width)+1] <- edge_labels_metrics[i,j]/100
      }
    }
  }

  edge_width <- ifelse(edge_width<0.25,0.25,edge_width)
  edge_labels <- round(edge_labels, 2)
  edge_labels <- paste0(edge_labels,"%")
  agg_node_sig_2 <- c()

  for(node in rownames(agg_edge_values_3)) {

    if(node %in% names(nodes_abv_thresh)) {
      agg_node_sig_2[length(agg_node_sig_2)+1] = agg_node_sig[node]
    }

    if(node %in% clusters){

      agg_node_sig_2[length(agg_node_sig_2)+1] = mean(agg_node_sig[cluster_node[[node]]])
    }


  }
  names(agg_node_sig_2) = rownames(agg_edge_values_3)
  agg_node_sig_2 <- round(agg_node_sig_2,3)
  node_labels <- paste0(names(agg_node_sig_2),"\\n","(",agg_node_sig_2,")")



  #creating graph nodes and edges
  create_node_df(n = length(node_labels),
                 label = node_labels,
                 color_level = agg_node_sig_2,
                 color = "grey",
                 fontname = "Arial",
                 fontsize = 10,
                 fontcolor = "black",
                 shape = "rectangle",
                 style = "rounded,filled",
                 fixedsize = FALSE,
                 penwidth = 1.5,
                 tooltip = agg_node_sig_2
  ) -> nodes


  create_edge_df(from = from_id,
                 to = to_id,
                 label = edge_labels,
                 penwidth = edge_width,
                 fontname = "Arial",
                 color = "blue"
  ) -> edges

  min_sig_level <- min(nodes$color_level)
  max_sig_level <- max(nodes$color_level[nodes$color_level < Inf])


  create_graph(nodes, edges) %>%
    add_global_graph_attrs(attr = "rankdir", value = "LR",attr_type = "graph") %>%
    colorize_node_attrs(node_attr_from = "color_level",
                        node_attr_to = "fillcolor",
                        default_color = "white",
                        palette = "Blues",
                        cut_points = seq(min_sig_level - .1, max_sig_level + .1, length.out = 9)) %>%
    add_global_graph_attrs(attr = "layout", value =  "dot", attr_type = "graph") -> graph

  #Produces process model
  print(render_graph(graph))

}





