#' Print tables at risk
#'
#' Report tables that use the same variable but different breakdowns
#'
#' Prints all tables per variable that have more than 1 unique breakdown. Does not show tables with only 1 unique breakdown.
#' Draws full graph, could be developed to only draw unique breakdowns > 1 in graph too
#'
#' @param exclude \code{vector} containing \code{strings} that should be excluded from the graph edgelist
#'
#' @param include \code{vector} containing \code{strings} that should be included as the graph edgelist (exclusive)
#'
#' @param colours \code{list} containing 5 colours as \code{strings}, to represent variable risk
#'
#' @return prints subsets of tables_metadata mutated and columns renamed. Does not modify tables_metadata. Also prints graph.
#'
#' @examples
#'
#' @export
report_tablesRisk <- function(exclude=NULL,include=NULL,colours=list('black','black','black','red','red')){
  metadata <- tables_metadata
  metadata_mutated <- metadata %>% dplyr::group_by(variable_name) %>% dplyr::mutate(unique_breakdowns = n_distinct(breakdown)) %>% dplyr::arrange(-(unique_breakdowns)) %>% dplyr::ungroup()
  output <- metadata_mutated[metadata_mutated$unique_breakdowns > 1,][,c("table_name","variable_name","breakdown")]
  colnames(output) <- c("Tables_At_Risk","Risk_Variable","Risk_Breakdown")
  for(i in unique(output$Risk_Variable)){
    print(output[output$Risk_Variable == i,])
  }
  M_report <- adjmatrix_complete(edgeexclude=exclude,edgeinclude=include,edgecolours=colours)
  labelled_graph(M_edges = M_report[[1]],M_labels = M_report[[2]],M_colours = M_report[[3]])
}


#' Report risk profile for a single table
#'
#' Prints full report on all tables, but draws graph centering on only one table
#'
#' Prints full report on all tables, but draws graph centering on only one table
#'
#' @param table \code{string} table name, accepts only a single string
#'
#' @return prints same as report_tablesRisk but draws graph centered on single table
#'
#' @examples
#'
#' @export
report_table <- function(table){
  report_tablesRisk()
  singleM <- adjmatrix_singletable_impact(table)
  labelled_graph(M_edges = singleM[[1]],M_labels = singleM[[2]],M_colours = singleM[[3]])
}

#' Print and locate NA values
#'
#' Reports on any NA values found in the metadata
#'
#' Reports on any NA values found in the metadata
#'
#' @param metadata \code{tibble} defaults to tables_metadata
#'
#' @return prints first column of any rows that contain NAs
#'
#' @examples
#'
#' @export
report_na <- function(metadata=tables_metadata){
  #check for and report on NA values
  if(sum(is.na(metadata)) > 0){
    print("") #newline
    print("Warning: NA values in metadata")
    print("Check the following files:")
    #print which rows need checked. For tables this will print the table name, for
    #datasets this will print the dataset name
    rows_index_with_missing_data <- which(is.na(metadata))%%nrow(metadata)
    missing_data_subset_first_column <- metadata[rows_index_with_missing_data,1]
    first_column_name <- names(metadata)[1]
    print(unique(missing_data_subset_first_column[[first_column_name]]))
  }else{
    print("")
    print("No NA values detected")
  }
}

#' Print breakdown of variables by Risk
#'
#' Print breakdown of variables by Risk
#'
#' Print breakdown of variables by Risk
#'
#' @param metadata \code{tibble} defaults to dataset_metadata
#'
#' @return prints first column of any rows that contain NAs
#'
#' @examples
#'
#' @export
report_variablesrisk <- function(metadata=dataset_metadata){
  for(i in 5:1){
    print(stringr::str_c("Risk ",i))
    print(metadata[metadata$risk == i,1:2])
  }
}

#' Create Full Report HTML file
#'
#' Create Full Report HTML file via Rmarkdown template
#'
#' Using the built in Rmarkdown file \code{FullReport.Rmd}, this function creates an html file
#' using the name provided in the \code{outfile} parameter to the directory specified, default
#' "./MarkdownReports/". Uses global variables \code{tables_metadata} and \code{dataset_metadata}
#'
#' @param outfile Filename used for the output file
#'
#' @param directory Directory where the file should be created. Default "./MarkdownReports/"
#'
#' @return HTML file
#'
#' @examples
#'
#' @export
report_full <- function(outfile,directory="./MarkdownReports/"){
  rmd_file <- system.file("rmd","FullReport.Rmd",package = "differencing.tool")
  rmarkdown::render(rmd_file,output_file=outfile,output_dir="./MarkdownReports/")
}
