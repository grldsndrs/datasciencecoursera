complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  initialDirectory <- getwd()
  
  setwd(directory)
  
  vectorForCsvFiles <- list.files(path = ".", all.files = TRUE,full.names = FALSE, 
                                  recursive = TRUE)
  
  vectorForCsvFilesInId <- vectorForCsvFiles[id]
  
  listOfDataFramesForCsvFilesInId <- sapply(vectorForCsvFilesInId, read.csv
                                            , simplify = FALSE)
  pollutant<-c("nitrate","sulfate")
  
  particulateMatterCompleteCaseListsForCsvFilesInId <- 
    sapply(vectorForCsvFilesInId
           ,function(file) {
               pollutantsInListOfDataFramesForCsvFilesInId <-
                 sapply(pollutant
                      ,function(p) { 
                        listOfDataFramesForCsvFilesInId[[file]][p]
               },simplify = FALSE)
                      
               completeCases <- 
                 sapply(pollutantsInListOfDataFramesForCsvFilesInId
                      ,function(p) { 
                        complete.cases(p)
               },simplify = FALSE)
               
               sapply(pollutant
                      ,function(p) { 
                        pollutantsInListOfDataFramesForCsvFilesInId[[p]][completeCases[[p]],]
                      },simplify = FALSE)
             },simplify = FALSE)
  
  numGoodParticulateMatterCompleteCaseListsForCsvFilesInId <- 
    sapply(particulateMatterCompleteCaseListsForCsvFilesInId 
           ,function(list) {sum( lengths(list))},simplify = FALSE)
  
  numGoodParticulateMatterCompleteCaseDataFramesForCsvFilesInId <- 
    rev(stack(data.frame(numGoodParticulateMatterCompleteCaseListsForCsvFilesInId)))
  
  colnames(numGoodParticulateMatterCompleteCaseDataFramesForCsvFilesInId) <- 
    c("id", "nobs")
  
  nocsv<-sub(".csv", "", numGoodParticulateMatterCompleteCaseDataFramesForCsvFilesInId["id"][[1]])
  
  noX<-sub("X", "", nocsv)
  
  numGoodParticulateMatterCompleteCaseDataFramesForCsvFilesInId["id"]<-as.numeric(noX)
 
  setwd(initialDirectory)
  
  numGoodParticulateMatterCompleteCaseDataFramesForCsvFilesInId
  
  
}
