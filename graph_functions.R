#' Internal function: Create Blank Matrix for Graphing
#'
#' Creates blank adjacency matrix for population with edges
#'
#' Creates a blank matrix, with column and row labels taken from metadata \code{node_column}, filled with \code{filler}.
#' Will be either a numeric or character matrix depending on filler. Used for creating graphs (adjacency matrix)
#'
#' @param metadata \code{tibble} The target metadata tibble, does not default in this function but usually defaults to tables_metadata when called by other functions
#'
#' @param node_column \code{integer} Tells the function which column to use for matrix row and column names (eg these will be the graph nodes)
#'
#' @param filler Any \code{string} or \code{integer} that the matrix will be filled with. For an adj matrix this will be \code{integer} 0 (0L), but for further matrices
#' such as those to contain edge attributes this can be some \code{string}, recommended to be \code{string} equivalent of 0 ("0")
#'
#' @return Will return matrix
#'
#' @examples
#'
#' @export
reset_matrix <- function(metadata,node_column,filler){
  numberofnodes <- nrow(unique(metadata[node_column]))
  rowcolnames <- unique(metadata[[node_column]])
  M <- matrix(filler,nrow = numberofnodes,ncol = numberofnodes,dimnames = list(rowcolnames,rowcolnames))
  return(M)
}

#' Internal function: Create Adjacency Matrix
#'
#' Create adjacency matrix for tables_metadata
#'
#' Create adjacency matrix for tables_metadata with table_name as the nodes (column 1) and variable_name as the edges (column 3).
#' Can be modified to explicitly include only certain edges, or exclude certain edges, and also nodes/edges columns can be modified to create
#' graphs showing different relationships. Returns a list of 2 matrices, item 1 being the adjacency matrix and item 2 being edge labels based
#' on edge_column values. Development could take place to create more matrices with further edge attributes, such as colours representing risk
#'
#' @param metadata \code{tibble} defaults to tables_metadata
#' @param edge_column \code{integer} defaults to 3 (check names(tables_metadata) if choosing new values)
#' @param node_column \code{integer} defaults to 1 (check names(tables_metadata) if choosing new values)
#' @param edgeinclude \code{vector} defaults to null. Pass vector of \code{strings} and function will ignore all other value in node_column
#' @param edgeexclude \code{vector} defaults to null. Pass vector of \code{strings} and function will ignore all
#' @param edgecolours \code{list} containing 5 colours as \code{strings}, to represent variable risk
#'
#' @return Returns a list of 3 matrices, item 1 being the adjacency matrix, item 2 being edge labels based on edge_column values and item 3 being the colours
#'
#' @examples
#'
#' @export
adjmatrix_complete <- function(metadata = tables_metadata,edge_column = 3,node_column = 1,edgeinclude=NULL,edgeexclude=NULL,edgecolours = list('black','black','black','red','red')){
  #create empty integer matrix for graph adjacency matrix
  intM <- reset_matrix(metadata,node_column,0L)
  #create empty char matrix for edge labels
  chrM <- reset_matrix(metadata,node_column,'0')
  #create empty colour matrix for edge colours
  colM <- reset_matrix(metadata,node_column,'0')
  if(!(is.null(edgeinclude))){
    edge_list <- edgeinclude
  }else if(!(is.null(edgeexclude))){
    edge_list <- unique(metadata[[edge_column]][!(metadata[[edge_column]] %in% edgeexclude)])
  }else{
    edge_list <- unique(metadata[[edge_column]])
  }
  for(i in edge_list){
    #get list of nodes that use edge i
    node_list <- as.list(metadata[metadata[[edge_column]] == i,node_column])[[1]]
    #create pair combination list from the above, to obtain all pairs excluding duplicates
    #do not run if tables_list is 1 or smaller
    if(length(node_list) > 1){
      node_list_combn <- combn(node_list,2,simplify = F)
      #add 1 into adj matrix for all pairs
      for(pair in node_list_combn){
        #enter 1 where edge exists
        intM[pair[1],pair[2]] <- 1
        intM[pair[2],pair[1]] <- 1
        #record edge label in same position
        chrM[pair[1],pair[2]] <- i
        chrM[pair[2],pair[1]] <- i
        #record edge colour attribute
        if(i %in% dataset_metadata$variable_name){
          edge_colour <- edgecolours[[dataset_metadata[dataset_metadata$variable_name == i,'risk'][[1]]]]
        }else{
          edge_colour <- 'black'
        }
        colM[pair[1],pair[2]] <- edge_colour
        colM[pair[2],pair[1]] <- edge_colour
      }
    }
  }
  return(list(intM,chrM,colM))
}




################################################################################################
#### reporting style functions
################################################################################################
#' Internal function: Create New Adj Matrix showing Single Table Relationships
#'
#' Create adj matrix focussed on the connections to a single table
#'
#' Creates a new adjacency matrix with only the values from \code{table} row/column of full project matrix
#'
#' @param table \code{string} the table name to be targeted
#'
#' @return Returns a matrix
#'
#' @examples
#'
#' @export
adjmatrix_singletable_impact <- function(table){
  #extract single table vector
  M_report <- adjmatrix_complete()
  singleTableConnect <- M_report[[1]][table,]
  #create blank matrix to pass single table connection into
  singleM <- reset_matrix(tables_metadata,1,0L)
  #and pass vector in both dimensions (M is symmetrical across diagonal, therefore same vector in both directions)
  singleM[table,] <- singleTableConnect
  singleM[,table] <- singleTableConnect
  #return singleM along with labels and colours matrices
  return(list(singleM,M_report[[2]],M_report[[3]]))
}


#' Internal function: Draw Labelled Graph
#'
#' Draw Labelled Graph based on adj matrix and label matrix
#'
#' Draws by default the existing graph represented by M.
#'
#' @param M_edges \code{matrix} the adjacency matrix representing nodes and edges.
#' @param M_labels \code{matrix} the matrix representing edge labels.
#' @param M_colours \code{matrix} the matrix representing edge colours.
#'
#' @return Draws Graph
#'
#' @examples
#'
#' @export
labelled_graph <- function(M_edges,M_labels,M_colours){
  if(nrow(M_edges) > 1){
    g <- network::as.network.matrix(M_edges,loops = F,directed = F)
    g <- network::set.edge.value(g,'variable',M_labels)
    g <- network::set.edge.value(g,'colour',M_colours)
    if(network::network.edgecount(g) > 1){
      GGally::ggnet2(g,mode = 'circle',label = T,edge.label = 'variable',edge.color = 'colour',edge.label.color = 'colour')
    }else{
      GGally::ggnet2(g,mode = 'circle',label = T,edge.label = network::get.edge.attribute(g,attrname = 'variable'))
    }
  }else{
    print('Invalid Function for Single Node Graph')
  }
}
