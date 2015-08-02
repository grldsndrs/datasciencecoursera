rankhospital <- function(state, outcome, num = "best") {

  rankedX <- NULL
  source("best.R")
  extendBest <<- "defined"
  encapsulatedBest <- best(state, outcome)

  ## Read outcome data
  outcomeOfCareMeasures <-
    encapsulatedBest$getData("outcome-of-care-measures.csv", colClasses = "character")

  #number given by num is larger than the number of hospitals in that state
  set_num <- function() {
    switch(as.character(num),
           "best" = eval({rankedX <<- "Best";TRUE}),
           "worst" = eval({rankedX <<- "Worst";TRUE}),
           eval({
             caller<- "num";
             validateInstance <- encapsulatedBest$validate(caller,num);
             if (validateInstance) {
               rankedX <<- as.numeric(num)
               TRUE
             }
             else{
               FALSE
             }
           })
    )
  }



  # Check that num,  state and outcome are valid
#   if (!encapsulatedBest$set_state()) {
#     stop("invalid state")
#   }
#
#   if (!encapsulatedBest$set_outcome()) {
#     stop("invalid outcome")
#   }

  if (!set_num()) {
    hospitalName <- NA
    return(hospitalName)
  }

  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (rankedX=="Best") {
    hospitalName <- encapsulatedBest$getBest(state = state, outcome = outcome)
  }

  getWorst <- function(state, outcome) {


    allCases <-
      outcomeOfCareMeasures[[encapsulatedBest$outcomeCode[outcome,]]][outcomeOfCareMeasures[encapsulatedBest$subsetByS]==state]

    allHospitals <-
      outcomeOfCareMeasures[[encapsulatedBest$subsetByH.N]][outcomeOfCareMeasures[encapsulatedBest$subsetByS]==state]

    ties <- allCases[] == max(allCases, na.rm = TRUE )

    if (sum(ties,na.rm = TRUE)) {
      #ties <- which(ties)
    }

    hospitalName <<- sort(allHospitals[ties][!is.na(allHospitals[ties])])[1]
  }

  if (rankedX=="Worst") {
    hospitalName <- getWorst(state = state, outcome = outcome)
  }

  getRankedX <- function(state, outcome) {


    allCases <-
      outcomeOfCareMeasures[[encapsulatedBest$outcomeCode[outcome,]]][outcomeOfCareMeasures[encapsulatedBest$subsetByS]==state]

    allHospitals <-
      outcomeOfCareMeasures[[encapsulatedBest$subsetByH.N]][outcomeOfCareMeasures[encapsulatedBest$subsetByS]==state]

    found <- !is.na(allCases)

    hospitalNameIndex <- order(allCases[found],allHospitals[found])[rankedX]
    hospitalName <<- allHospitals[found][hospitalNameIndex]
  }

  if (is.numeric( rankedX)){
    hospitalName <- getRankedX(state = state, outcome = outcome)
  }
  #check to see if best is called to extend it
  if (is.null(extendRankHospital)) {
    hospitalName
  }else{
    extendRankHospital <<- NULL

    list(set_num = set_num,
      getWorst = getWorst,
      getRankedX = getRankedX
      )
  }
}
