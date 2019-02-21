# Further Information: Design, Implementation and Documentation
## Code Design
* As mentioned in the ```README```, the package is the implementation of a proof-of-concept statistical pipeline to analyze public health datasets, developed previously at NCSA.
* The main objective of code design is modularity: we want the user to be able to choose which steps of the pipeline they would like to execute.
* The pipeline involves three main tasks: Data Preprocessing, Data Separation and Analysis. Consequently, we have three R scripts, pertaining to each task.

## Code Implementation
* There are 4 `R` scripts: ```DataPipelineDriver_New.R```, ```DataPreprocessing_New.R```, ```DataSeparation_New.R``` and ```DataAnalysis_New.R```.
* ```DataPipelineDriver_New.R``` - contains a main driver routine that runs all functions sequentially according to the pipeline.
* ```DataPreprocessing_New.R``` - implements the pipeline blocks that preprocess the data.
* ```DataSeparation_New.R``` - implements the pipeline blocks that separate the original dataframe into discrete and continuous data frames.
* ```DataAnalysis_New.R``` - implements the pipeline block that analyzes the data (performs PCA, etc.)
  
## Code Documentation
* Documentation of code in the package has been done using ```roxygen2```. 
* Simply type in ```?function_name``` in the RStudio terminal to bring up a pop-up listing a function description, argument descriptions, usage example and return data type and description.
* ```roxygen2``` is used in our code because of
  1. Standard usage in most CRAN packages
  2. Easy maintainability
  3. Automatic generation of nicely formatted R Markdown documentation files
