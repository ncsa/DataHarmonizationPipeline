#' Function that applies PCA on the preprocessed data and produces plots.
#' 
#' @param analysisList list containing the Race column (from the original dataset in \code{dataset_file_path}) 
#' and the preprocessed, normalized, continuous dataset \code{normalizedContinuousDataset}
#' 
#' @return a plot made with \cdoe{ggplot2} after applying PCA on \code{normalizedContinuousDataset}
#' 
#' @example
#' column_and_data_list = list("race" = raceColumn, "data" = normalizedContinuousDataset)
#' applyPCA(column_and_data_list)
applyPCA <- function(analysisList) {
  
  library(pcaMethods)
  library(ggplot2)
  
  pcBPCA <- pca(analysisList$data, nPcs = 20, method = "bpca")
  dfBPCA <- merge(as.factor(analysisList$race),scores(pcBPCA), by = 0)
  plotBPCA = ggplot(dfBPCA, aes(PC1, PC2, color = dfBPCA$x)) +
             geom_point() +
             xlab(paste("PC1", pcBPCA@R2[1] * 100, "% of variance")) +
             ylab(paste("PC2", pcBPCA@R2[2] * 100, "% of variance"))
  
  return(plotBPCA)
}