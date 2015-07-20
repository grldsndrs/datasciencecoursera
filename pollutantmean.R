pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  initialDirectory <- getwd()
  
  setwd(directory)
  
  vectorForCsvFiles <- list.files(path = ".", all.files = TRUE,full.names = FALSE, 
                                  recursive = TRUE)
  
  vectorForCsvFilesInId <- vectorForCsvFiles[id]
  
  listOfDataFramesForCsvFilesInId <- sapply(vectorForCsvFilesInId, read.csv
                                        , simplify = FALSE)
  
  particulateMatterDataFramesForCsvFilesInId <- sapply(vectorForCsvFilesInId
            ,function(p) { 
              listOfDataFramesForCsvFilesInId[[p]][pollutant]}
            ,simplify = FALSE)
  
  goodParticulateMatterDataFramesForCsvFilesInId <- 
    sapply(particulateMatterDataFramesForCsvFilesInId
           ,function(obs) { !is.na(obs)},simplify = FALSE)
  
  meansParticulateMatterDataFramesForCsvFilesInId <- 
    sapply(particulateMatterDataFramesForCsvFilesInId
           ,colMeans,na.rm=TRUE)
  
  weightsParticulateMatterDataFramesForCsvFilesInId <- 
    sapply(goodParticulateMatterDataFramesForCsvFilesInId
           ,sum)
  
  setwd(initialDirectory)
  
  w <- weightsParticulateMatterDataFramesForCsvFilesInId
  m <- meansParticulateMatterDataFramesForCsvFilesInId
  
  sum(w * m)/sum(w)
  
  }
