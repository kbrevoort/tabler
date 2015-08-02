#' Function to generate Latex table of summary statistics
#'
#' @export
summaryTable <- function(inData, funcList = list('mean', 'sd', 'min', 'max'), dropAllNA = FALSE) {

  myTable <- list()
  attr(myTable, "class") <- "sumTableRec"

  myTable$varNames <- names(inData)
  myTable$xlevels <- list()

  for (i in seq_along(inData)) {
    if (is.factor(inData[[1]])) {
      myTable$xlevels[[names(df)[i]]] <- levels(df[[i]])
    }
  }

  # If dropAllNA == TRUE, we drop any observation that has at least one missing value
  keepObs <- rep(TRUE, dim(inData)[1])
  if (dropAllNA) keepObs <- which(apply(inData, 1, function(x) any(is.na(x))))

  # This loops over the variables in inData, calling st_runFun on each one
  byVar <- lapply(inData[keepObs,], st_runFun, funcList)

  for (i in seq_along(byVar)) {
    rownames(byVar[[i]]) <- gsub("inVec", names(byVar[i]), rownames(byVar[[i]]))
  }
  myTable$values <- do.call(rbind, byVar)

  myTable
}

st_runFun <- function(inVec, funcList = NA) {
  a <- lapply(funcList, st_doFun, inVec)
  retVal <- do.call(cbind, a)
  colnames(retVal) <- unlist(funcList)
  retVal
}

st_doFun <- function(f, inVec) {
  myF <- NA
  if (f == "n") {
    myF <- function(x) sum(1 - is.na(x))
  } else {
    myF <- get(f, envir = .GlobalEnv)
    if (!is.function(myF)) stop(paste0('Invalid function supplied to SummaryTable:  ', f))
  }

  apply(model.matrix(~ 0 + inVec), 2, myF)
}

