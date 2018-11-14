## Include all the necessary R scripts.


source("../DataSeparation_New.R")
source("../DataPreprocessing_New.R")
source("../DataAnalysis_New.R")

#' Function that loads data to be analyzed from the specified files
#'
#' @param dataset_file_path Relative file path to the file (containing the dataset) to be analyzed
#'
#' @return dataframe containing the dataset in \code{dataset_file_path}
#'
#' @example
#' loadData("./existing_data.csv")
loadData <- function(dataset_file_path) {

  ## Specify the file path here.
  ## @TODO: allow input of file path as a command line argument.
  #dataset_file_path = "./existing_data.csv"

  ## Load the data frame.
  dataset = read.csv(dataset_file_path, header = TRUE)

  ## Return the data frame.
  return(dataset)

}

#' Function that is the main driver for the entire program that executes the entire pipeline
#'
#' @param dataset_file_path Relative file path to the file (containing the dataset) to be analyzed
#' @param missingDataThreshold the threshold of missing data in a colum/row for which that data should be removed
#'
#' @return Plot after PCA Analysis of the Preprocessed, Normalized (Continuous Data) Dataset
#' (obtained from \code{dataset_file_path})
#'
#' @example
#' mainDriver("./existing_data.csv", 0.60)
mainDriver <- function(dataset_file_path, missingDataThreshold) {

  dataset = loadData(dataset_file_path)
  dataset = dataset[1 : 102, ]

  ## @TODO: convert control flow into steps that can be specified to be run
  datasetsList = dropRowsWithMissingData(missingDataThreshold,
                                      makeDataTypesUniform(
                                        dropColumnsWithMissingData(missingDataThreshold,
                                          dropNonNumericalData(
                                            addIdentifyingInfo(dataset,
                                              getDiscreteAndContinuousIndices(dataset))))))

  raceColumn = datasetsList$continuous[ , ncol(datasetsList$continuous)]
  continuous_dataset_to_normalize = datasetsList$continuous[ , -ncol(datasetsList$continuous)]

  normalizedContinuousDataset = normalizeContinuousData(continuous_dataset_to_normalize)
  analysisList = list("race" = raceColumn, "data" = normalizedContinuousDataset)
  plot_ = applyPCA(analysisList)

  return(plot_)
  ## @TODO: save the plot as in image, to prevent weird viewing aspect ratios
}

mainDriver("../existing_data.csv", 0.60)
