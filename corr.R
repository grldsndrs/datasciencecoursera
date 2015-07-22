corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

pollutant<-c("nitrate","sulfate")
id <- 1:332

 source("complete.R")
  numParticulateMatterCompleteCaseDataFramesForCsvFilesInId <-
    complete(directory,id)

  meetThresholdParticulateMatterCompleteCaseDataFramesForCsvFilesInId <-
    numParticulateMatterCompleteCaseDataFramesForCsvFilesInId["nobs"] > threshold

    metThresholdParticulateMatterCompleteCaseDataFramesForCsvFilesInId <-
      numParticulateMatterCompleteCaseDataFramesForCsvFilesInId["id"][
        meetThresholdParticulateMatterCompleteCaseDataFramesForCsvFilesInId]


    metThresholdListOfCsvFilesInId <-
      sapply(metThresholdParticulateMatterCompleteCaseDataFramesForCsvFilesInId
             ,function(fileId) {
                 paste(paste(rep_len("0",3-nchar(fileId)),sep = "",collapse = "")
                       ,as.character(fileId),".csv",sep = "")
             },simplify = FALSE)

    initialDirectory <- getwd()

    setwd(directory)

    metThresholdListOfDataFramesForCsvFilesInId <-
      sapply(metThresholdListOfCsvFilesInId, read.csv
             , simplify = FALSE)


    setwd(initialDirectory)

    correlationMetThresholdListOfDataFramesForCsvFilesInId <-
      sapply(metThresholdListOfDataFramesForCsvFilesInId
           ,function(df) {
             cor(x=df[pollutant[1]],y=df[pollutant[2]]
                 ,use = "complete.obs",method = "pearson")
             },simplify = FALSE)

unlist(correlationMetThresholdListOfDataFramesForCsvFilesInId)

}
