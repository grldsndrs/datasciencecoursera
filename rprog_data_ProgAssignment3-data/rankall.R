rankall <- function(outcome, num = "best") {

  extendRankHospital <<- NULL
  source("rankhospital.R")
  outcomes <- c("heart attack","heart failure","pneumonia")
  ha <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  hf <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  p <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  subsetByS <- "State"

  ## Read outcome data
  getData <- function(fileName,...){
    outcomeOfCareMeasures <- read.csv(fileName,...)

    suppressWarnings(outcomeOfCareMeasures[,ha] <-
                       as.numeric(outcomeOfCareMeasures[,ha]))

    suppressWarnings(outcomeOfCareMeasures[,hf] <-
                       as.numeric(outcomeOfCareMeasures[,hf]))

    suppressWarnings(outcomeOfCareMeasures[,p] <-
                       as.numeric(outcomeOfCareMeasures[,p]))
    outcomeOfCareMeasures
  }

  outcomeOfCareMeasures <-
    getData("outcome-of-care-measures.csv", colClasses = "character")

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ## For each state,
  rankByStateOutcomeOfCareMeasuresDf <-
    ##find the hospital of the given rank
    sapply(sort(unique(outcomeOfCareMeasures[[subsetByS]])), function(state) {
     rankhospital(state, outcome, num)
    },simplify = FALSE)

  formattedRanking <- stack(rankByStateOutcomeOfCareMeasuresDf)

  colnames(formattedRanking) <- c("hospital","state")
  row.names(formattedRanking) <- formattedRanking$state
  formattedRanking
}
