This R package was created to provide a set of tools for those working in Statistical Disclosure Control
The functions will help the user keep track of tables created from a single dataset and create risk profiles for each table
with regards to the disclosure vector of table differencing.
The tool is designed to be used from within an R project, where the data logged is kept across sessions in the .RDATA file.

User Functions

init_project()
    
        Function: Creates the initial tibbles that will contain the metadata.
        Param: No Input
        Output: Two tibbles: tables_metadata and dataset_metadata
    
create_dataset_meta(filename,filepath)
    
        Function: Creates CSV file containing Dataset Metadata
        Param: filename,string - the dataset filename
        Param: filepath,string, - dataset path (default: `./datasets/')
        Output: A single CSV file with the same name as filename but with `meta' appended.
    
add_newdataset(filenames,filepath)
    
        Function: Adds datasets metadata to datasets_metadata tibble
        Param: filenames,vector - a vector containing the metadata filenames as strings
        Param: filepath,string - (default: ./datasets/)
        Output: Modifies datasets_metadata
    
remove_dataset(datasets)
    
        Function: Removes a dataset from datasets_metadata
        Param: datasets,vector - a vector containing dataset names as strings
        Output: Modifies datasets_metadata
    
add_newtable(filenames,filepath)
    
        Function: Adds table metadata to tables_metadata tibble
        Param: filenames,vector - a vector containing the metadata filenames as strings
        Param: filepath,string - (default: ./tables_metadata/)
        Output: Modifies tables_metadata
    
remove_table(tables)
    
        Function: Removes a table from tables_metadata
        Param: tables,vector - a vector containing table names as strings
        Output: Modifies tables_metadata
    
report_tablesRisk()
    
        Function: Produces a full report on tables vs variables and breakdowns risk
        Param: include, vector - modifies network to display only those variables in include
        Param: exclude, vector - modifies network to not display those variables in exclude
        Param: colours, vector - modifies default colour scheme (`black',`black',`black',`red',`red')
        Output: Prints to Console and creates network visualisation
    
report_table(table)
    
        Function: Produces same console report as report_tablesRisk() but has a reduced network based on table
        Param: table, string - table name
        Output: Prints to Console and creates network visualisation
    
report_na(metadata)
    
        Function: Reports if there are any NA values in the metadata
        Param: metadata, tibble - target metadata to report on
        Output: Prints report to console
    
report_variablesrisk()
    
        Function: Produces a report categorising the variables in use by their risk level
        Param: None
        Output: Prints report to console
    
report_full(outfile)
    
        Function: Produces a full study report file based on an Rmarkdown template
        Param: outfile, string - name of the output file
        Output: HTML file
    

Internal Functions

reset_matrix(metadata,node_column,filler
    
        Function: Creates a square matrix for building network object
        Param: metadata,tibble - the target metadata
        Param: node_column,integer - which column in metadata should be used as the row/column labels
        Param: filler,string or integer - value which will fill the matrix.
        Output: Returns matrix object
    
adjmatrix_complete(metadata,edge_column,node_column,include,exclude,colours)
    
        Function: Create Adjacency Matrix
        Param: metadata,tibble - the target metadata
        Param: edge_column,integer - which column in metadata should be used as the network edge information
        Param: node_column,integer - which column in metadata should be used as the row/column labels
        Param: include,vector - list of values to include from the edge_column
        Param: exclude,vector - list of values to exclude from the edge_column
        Param: colours,vector - list of 5 colours as strings representing Risk level of variable
        Output: Returns a list of two matrices, an adjacency matrix for creating a network and an equivalent matrix for labelling the edges.
    
adjmatrix_singletable_impact(M,table)
    
        Function: Create Adjacency Matrix with edges only connecting to table
        Param: table,string - the table to target
        Output: Adjacency Matrix
    
labelled_graph(M_edges,M_labels)
    
        Function: Draws a labelled network
        Param: M_edges,matrix - the adjacency matrix for the network
        Param: M_labels,matrix - the equivalent matrix with edge labels
        Output: Draws Network
    
