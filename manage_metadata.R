#' Initialise Project Variables
#'
#' Creates 2 empty \code{tibbles} to contain project data
#'
#' Create 2 \code{tibbles}: dataset_metadata and tables_metadata, to contain the dataset and tables metadata
#'
#' @param none
#'
#' @return Creates 2 \code{tibbles} required for the project
#'
#' @examples
#'
#' @export
init_project <- function(){
  dataset_metadata <<- dplyr::tibble(dataset_name=character(),variable_name=character(),data_type=character(),data_width=double(),sensitive_var=double(),sensitive_geog=double(),risk=double())
  tables_metadata <<- dplyr::tibble(table_name=character(),original_dataset=character(),variable_name=character(),derived_from=character(),data_type=character(),data_width=double(),breakdown=character(),n=double())
}

#' Read dataset and extract metadata
#'
#' Reads dataset file, extracts metadata and outputs to new csv file in same directory
#'
#' Reads dataset file, extracts metadata and outputs to new csv file in same directory.
#' Automatically detects metadata:
#' \itemize{
#'  \item{dataset_name}
#'  \item{variable_name}
#'  \item{data_type}
#'  \item{data_width}
#' }
#' Further metadata must be edited manually by opening the csv file:
#' \itemize{
#'  \item{sensitive_var: 0 or 1 (0=no,1=yes)}
#'  \item{sensitive_geog: 0 or 1 (0=no,1=yes)}
#'  \item{risk: 1 to 5 (1=low, 5=high)}
#' }
#'
#' @param filename \code{string} with name of file to target
#'
#' @param filepath \code{string} with the folder location of the file. Defaults to directory \code{'./datasets/'}
#'
#' @return Creates a csv in the directory \code{'./datasets/'}
#'
#' @examples
#'
#' @export
create_dataset_meta <- function(filename,filepath = './datasets/'){
  #import dataset
  dataset <- readr::read_csv(paste(c(filepath,filename),collapse = ''))
  #collect metadata
  dataset_name <- stringr::str_split(filename,'\\.')[[1]][1]
  variable_names <- names(dataset)
  data_type <- tibble::enframe(sapply(dataset,class))$value
  data_width <- c()
  for(i in 1:length(variable_names)){
    data_width <- append(data_width,nrow(unique(dataset[,i])))
  }
  metadata <- dplyr::bind_cols(dataset_name=c(rep(dataset_name,length(variable_names))),variable_name=variable_names,data_type=data_type,data_width=data_width,sensitive_var=double(),sensitive_geog=double(),risk=double())
  output_name <- paste(c(filepath,dataset_name,'_meta.csv'),collapse = '')
  readr::write_csv(metadata,output_name)
}


#' Add new table to the project
#'
#' Modifies global variable \code{tables_metadata} with the new table(s) appended to the end
#'
#' Pre-requisite: tables_metadata must exist already, therefore initial config needs to have been run first. See also \code{\link{remove_table}}.
#'
#' @param filenames \code{vector} containing filenames of new table metadata as \code{strings}, for example \code{filenames = c('table1.xlsx','table2.xlsx')}
#'
#' @param filepath The location of the table metadata files as a \code{string}. Default \code{'./tables_metadata/'}
#'
#' @return modifies existing tibble \code{tables_metadata}
#'
#' @examples
#'
#' @export
add_newtable <- function(filenames,filepath = './tables_metadata/'){
  if(!(exists('tables_metadata'))){
    print('tables_metadata does not exist')
  }else{
    #convert filenames to full path to pass to sapply function
    for(i in 1:length(filenames)){
      filenames[i] <- paste(c(filepath,'/',filenames[i]),collapse='')
    }
    new_metadata <- sapply(filenames,readxl::read_xlsx,simplify = FALSE) %>% dplyr::bind_rows()
    tables_metadata <<- dplyr::bind_rows(tables_metadata,new_metadata)
  }
}

#' Remove table from the project
#'
#' Modifies global variable \code{tables_metadata} by removing tables specified
#'
#' Pre-requisite: tables_metadata must exist already, therefore initial config needs to have been run first. See also \code{\link{add_newtable}}.
#'
#' @param tables \code{vector} containing table names to remove, as \code{strings}, for example \code{tables = c('table1','table2')}
#'
#' @return modifies existing tibble \code{tables_metadata}
#'
#' @examples
#'
#' @export
remove_table <- function(tables){
  if(!(exists('tables_metadata'))){
    print('tables_metadata does not exist')
  }else{
    tables_metadata <<- subset(tables_metadata,!(table_name %in% tables))
  }
}


#' Add new dataset metadata to the project
#'
#' Modifies global variable \code{dataset_metadata} with the new dataset meta appended to the end
#'
#' Pre-requisite: dataset_metadata must exist already, therefore initial config needs to have been run first. See also \code{\link{remove_dataset}}.
#'
#' @param filepath The location of the metadata files as a \code{string}. Default \code{'./datasets/'}
#'
#' @param filenames \code{vector} containing filenames of new metadata as \code{strings}, for example \code{filenames = c('datasetname1_meta.csv','datasetname2_meta.csv')}
#'
#' @return modifies existing tibble \code{dataset_metadata}
#'
#' @examples
#'
#' @export
add_newdataset <- function(filenames,filepath = './datasets/'){
  if(!(exists('dataset_metadata'))){
    print('dataset_metadata does not exist yet')
  }else{
    for(i in 1:length(filenames)){
      filenames[i] <- paste(c(filepath,filenames[i]),collapse='')
    }
    new_metadata <- sapply(filenames,readr::read_csv,simplify = FALSE) %>% dplyr::bind_rows()
    dataset_metadata <<- dplyr::bind_rows(dataset_metadata,new_metadata)
  }
}

#' Remove dataset from the project
#'
#' Modifies global variable \code{dataset_metadata} by removing datasets specified
#'
#' Pre-requisite: dataset_metadata must exist already, therefore initial config needs to have been run first. See also \code{\link{add_newdataset}}.
#'
#' @param datasets \code{vector} containing dataset names to remove, as \code{strings}, for example \code{datasets = c('dataset1','dataset2')}
#'
#' @return modifies existing variable \code{datasets_metadata}
#'
#' @examples
#'
#' @export
remove_dataset <- function(datasets){
  if(!(exists('dataset_metadata'))){
    print('dataset_metadata does not exist')
  }else{
    dataset_metadata <<- subset(dataset_metadata,!(dataset_name %in% datasets))
  }
}
