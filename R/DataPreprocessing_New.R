#' Function that drops non numerical data (not necessary for analysis) from the datasets
#' 
#' @param datasetsList_old a list of the discrete and continuous datasets of the form 
#' list("discrete" = discrete_dataset, "continuous" = continuous_dataset) (to be obtained from \code{addIdentifyingInfo}
#'  in \code{DataSeparation_New.R})
#'  
#' @return a similar list like the parameter passed in, with non-numerical data removed
#'  
#' @example 
#' data = loadData(dataset_file_path)
#' indices = getDiscreteAndContinuousIndices(dataset)
#' new_data = addIdentifyingInfo(data, indices)
#' dropNonNumericalData(new_data)
dropNonNumericalData <- function(datasetsList_old) {
  
  ## Exclude unecessary (the two specific) columns
  ## @TODO: remove hardcoded (dataset-specfic) values here
  continuous_dataset = datasetsList_old$continuous[-1, c(-1,-2)]
  discrete_dataset = datasetsList_old$discrete[-1, ]
  
  ## Pack into a list, and return
  datasetsList_new = list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
  return(datasetsList_new)
  
}

#' Function that gets the percentage of missing data in the columns of a dataset
#' Helper function for \code{dropColumnsWithMissingData}
#' 
#' @param dataset a dataframe
#' 
#' @return a list of the percentages of missing data in every column
#' 
#' @example 
#' missingPercent(datasetsList$discrete)
missingPercent <- function(dataset) {
  
  return(lapply(dataset, function(n) sum(is.na(n))/(length(n))))
  
}

#' Function that drops columns in the discrete and continuous datasets which have missing data percentage
#' above a provided threshold
#' 
#' @param missingDataThreshold the threshold of missing data in a column for which that data should be removed
#' @param datasetsList_old list of the discrete and continuous datasets
#' 
#' @return a similar list like the parameter passed in, with columns having too much missing data removed
#' 
#' @example 
#' dropColumnsWithMissingData(0.60, datasets)
dropColumnsWithMissingData <- function(missingDataThreshold, datasetsList_old) {
  
  ## Compute the % of Missing Data for each column in the two Datasets
  missing_stats_for_discrete = unlist(missingPercent(datasetsList_old$discrete))
  missing_stats_for_continuous = unlist(missingPercent(datasetsList_old$continuous))
  
  ##Filter out Columns with Missing Data above a Provided Threshold
  discrete_dataset = datasetsList_old$discrete[ , which(missing_stats_for_discrete < missingDataThreshold)]
  continuous_dataset = datasetsList_old$continuous[ , which(missing_stats_for_continuous < missingDataThreshold)]
  
  ## Pack into a list, and return
  datasetsList_new = list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
  return(datasetsList_new)
  
}

#' Function that converts all the data in both datasets to be of uniform data type (double)
#' 
#' @param datasetsList_old a list of the discrete and continuous datasets of the form 
#' list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
#' 
#' @return a similar list like the parameter passed in, with uniform data types
#' 
#' @example
#' makeDataTypesUniform(datasetsList)
makeDataTypesUniform <- function(datasetsList_old) {
  
  ## Convert the data type to double for both discrete and continuous datasets
  discrete_dataset = as.data.frame(lapply(datasetsList_old$discrete, function(x) as.double(as.character(x))))
  continuous_dataset = as.data.frame(lapply(datasetsList_old$continuous, function(x) as.double(as.character(x))))
  
  ## Pack into a list, and return
  datasetsList_new = list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
  return(datasetsList_new)
}

#' Function that drops rows in the discrete and continuous datasets which have missing data percentage
#' above a provided threshold
#' 
#' @param missingDataThreshold the threshold of missing data in a row for which that data should be removed
#' @param datasetsList_old list of the discrete and continuous datasets
#' 
#' @return a similar list like the parameter passed in, with rows having too much missing data removed
#' 
#' @example 
#' dropRowsWithMissingData(0.60, datasets)
dropRowsWithMissingData <- function(missingDataThreshold, datasetsList_old) {
  
  ## Calculate amount of missing data in the rows of discrete dataset, then delete them depending upon
  ## the threshold.
  missing_stats_for_discrete = rowSums(is.na(datasetsList_old$discrete), na.rm = TRUE) / ncol(datasetsList_old$discrete)
  discrete_dataset = datasetsList_old$discrete[which(missing_stats_for_discrete < missingDataThreshold), ]
  
  ## Calculate amount of missing data in the rows of continuous dataset, then delete them depending upon
  ## the threshold.
  missing_stats_for_continuous = rowSums(is.na(datasetsList_old$continuous), na.rm = TRUE) / ncol(datasetsList_old$continuous)
  continuous_dataset = datasetsList_old$continuous[which(missing_stats_for_continuous < missingDataThreshold), ]
  
  ## Pack into a list, and return
  datasetsList_new = list("discrete" = discrete_dataset, "continuous" = continuous_dataset)
  return(datasetsList_new)
}

## Helper Function
## Normalizes a given column's data

#' Function that normalizes data in the column of a dataset
#' Helper function for \code{normalizeContinuousData}
#' 
#' @param n a dataframe column
#' 
#' @return the column, with data normalized
#' 
#' @example 
#' lapply(continuousDataset, normalizeCol)
normalizeCol <- function(n) {
  
  return(n/mean(n, na.rm = TRUE))
  
}

## Normalize the data in the Continuous Dataset

#' Function that normalizes all the data in the continuous dataset
#' 
#' @param continuousDataset the dataset containing continuous data
#' 
#' @return the dataset, normalized
#' 
#' @example 
#' normalizeContinuousData(datasetsList$continuous)
normalizeContinuousData <- function(continuousDataset) {
  
  ## Normalizes each column in the continuous dataset
  continuousDatasetNormalized = as.data.frame(lapply(continuousDataset, normalizeCol))
  
  ## Return the normalized dataset
  return(continuousDatasetNormalized)
  
}