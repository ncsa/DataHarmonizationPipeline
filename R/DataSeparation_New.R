#' Function that checks if a provided variable is an integer
#' Helper function for \code{checkinteger.column}
#' 
#' @param n data variable
#' 
#' @return boolean value - true if the variable (\code{n}) is an integer, false if not
#' 
#' @examples 
#' check.integer(1)
#' check.integer(12.34)
check.integer <- function(n) {
  
  return((n %% 1) == 0)
}

#' Function that checks if all the data in a column of a dataset are integers
#' Helper function for \code{getDiscreteAndContinuousIndices}
#' 
#' @param n a column of a dataframe
#' 
#' @return a boolean value - true if the column has only integer values, false if not
#' 
#' @example 
#' lapply(dataset, checkinteger.column)
checkinteger.column <- function(n) {
  
  ## Convert the Data Type to a Double
  new_column = as.double(as.character(n))
  
  ## Drop invalid values from the column.
  new_column = new_column[!is.na(new_column)]
  
  ## Check if all entries are integers and return a boolean value.
  return(all(check.integer(new_column)))
  
}


#' Function that gets the indices of the dataframe columns that have discrete data, and those that have continuous data
#' 
#' @param dataset a dataframe
#' 
#' @return a list of the discrete and continuous indices, of the form 
#' list("discrete" = discrete_indices, "continuous" = continuous_indices)
#' 
#' @example 
#' dataset = loadData(dataset_file_path)
#' getDiscreteAndContinuousIndices(dataset)
getDiscreteAndContinuousIndices <- function(dataset) {
  
  ## Find which columns have integers only i.e. have discrete data
  areColumnsDiscrete = lapply(dataset, checkinteger.column)
  
  ## Based on the above, get the indices of the discrete and continuous data
  discrete_indices = which(areColumnsDiscrete == TRUE)
  continuous_indices = which (areColumnsDiscrete == FALSE)
  
  ## Pack into a list, and return.
  indicesList = list("discrete" = discrete_indices, "continuous" = continuous_indices)
  return(indicesList)
  
}

#' Function that adds identifying info (unique to each row) to the discrete and continuous datasets
#' 
#' @param dataset a dataframe
#' @param indiceslist a list of the discrete and continuous indices, of the form
#' list("discrete" = discrete_indices, "continuous" = continuous_indices) (to be obtained from \code{getDiscreteAndContinuousIndices})
#' 
#' @return a list of the discrete and continuous datasets of the form 
#' list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
#' 
#' @example 
#' data = loadData(dataset_file_path)
#' indices = getDiscreteAndContinuousIndices(dataset)
#' addIdentifyingInfo(data, indices)
addIdentifyingInfo <- function(dataset, indicesList) {
  
  ## Separate the Dataset into Discrete and Continuous Datasets
  
  discrete_dataset = dataset[, indicesList$discrete]
  continuous_dataset = dataset[, indicesList$continuous]
  
  ## Add gender info to the Continuous Dataset to be able to compare
  continuous_dataset$Race = dataset$Race.1
  
  ## Pack into a list, and return.
  datasetsList = list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
  return(datasetsList)
  
}
