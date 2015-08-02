best <- function(state, outcome) {
  State <- NULL
  Outcome <- NULL
  outcomes <- c("heart attack","heart failure","pneumonia")
  subsetByH.N <- "Hospital.Name"
  ha <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  hf <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  p <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  subsetByS <- "State"
  outcomeCode <-
    data.frame(code = c(ha,hf,p),row.names = outcomes,stringsAsFactors = FALSE)

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

  validate <- function(caller, textToValidate) {
    states <- outcomeOfCareMeasures[,"State"]
    validateByCaller <- function(caller,textToValidate) {
      switch(caller,
             "state" = sum(grepl(textToValidate,states)),
             "outcome" = sum(grepl(textToValidate,outcomes)),
             "num"= eval({
               allValByCallerCases <-
                 outcomeOfCareMeasures[[outcomeCode[Outcome,]]][outcomeOfCareMeasures[subsetByS]==State]
               ;allFoundCases <- allValByCallerCases[][!is.na(allValByCallerCases[])]
               ;as.numeric(textToValidate) <= length(allFoundCases)}
               )
             )
    }
    validateByCaller(caller,textToValidate)
  }

  set_state <- function() {
    caller<- "state"
    validateInstance <- validate(caller,state)
    if (validateInstance) {
      State <<- state
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }

  set_outcome <- function() {
    caller<- "outcome"
    validateInstance <- validate(caller,outcome)
    if (validateInstance) {
      Outcome <<- outcome
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }

  ## Check that state and outcome are valid
  if (!set_state()) {
    stop("invalid state")
  }
  if (!set_outcome()) {
    stop("invalid outcome")
  }


  hospitalName <-NULL

  getBest <- function(state, outcome) {


    allCases <-
      outcomeOfCareMeasures[[outcomeCode[outcome,]]][outcomeOfCareMeasures[subsetByS]==state]

    allHospitals <-
      outcomeOfCareMeasures[[subsetByH.N]][outcomeOfCareMeasures[subsetByS]==state]

    ties <- allCases[] == min(allCases, na.rm = TRUE )

    if (sum(ties,na.rm = TRUE)) {
      #ties <- which(ties)
    }

    hospitalName <<- sort(allHospitals[ties][!is.na(allHospitals[ties])])[1]
  }



  ## Return hospital name in that state with lowest 30-day death
  ## rate

  getBest(state = State, outcome = Outcome)

  #check to see if best is called to extend it
  if (is.null(extendBest)) {
    hospitalName
  }else{
    extendBest <<- NULL
    list(getData =getData,
         validate = validate,
         set_state = set_state,
         set_outcome = set_outcome,
         getBest = getBest,
         outcomeCode = outcomeCode ,
         subsetByH.N = subsetByH.N,
         subsetByS = subsetByS)
  }
}
