#' Make a table object from a list of column objects.
#'
#' This function pulls all of the necessary data elements out of the column
#' objects passed as an argument.  It then creates a table object that can
#' be passed to one of the output functions.
makeTable <- function( inCols=list(), title="NA", notes="NA", number=-1 ) {

  # This function will take a series of column objects
  myTable <- list()
  attr(myTable, "class") <- "tableRec"
  myTable$title <- title
  myTable$notes <- notes
  myTable$number <- number

  myTable$depVars <- unlist(lapply(inCols, function(x) x$depVar))
  myTable$varNames <- unique(unlist(lapply(inCols, function(x) x$varNames)))
  myTable$estTypes <- unlist(lapply(inCols, function(x) x$estType))

  # I am going to stack the coefficient matrices
  # estNum will record which estimation (column) the coefficients belong in
  for (j in seq_along(inCols)) inCols[[j]]$coefs$estNum <- j
  myTable$coefs <- do.call("rbind", lapply(inCols, function(x) x$coefs))

  # Combining gof data frames will be more difficult because the number of columns
  # may not be identical
  myTable$gofs <- data.frame(junk=rep(NA,length(inCols)))
  gofNames <- unique(unlist(lapply(inCols, function(x) names(x$gof))))
  for (thisName in gofNames) {
    myTable$gofs[thisName] <- unlist(lapply(inCols, function(x) ifelse(is.element(thisName, names(x$gof)), x$gof[thisName],NA)))
  }
  myTable$gofs["junk"] <- NULL

  # xlevels will follow the same method as gof.  It will be tougher though because
  # I do not know in advance the size of the vector.  This doesn't need to be a
  # data frame.  I'll make it a list.
  temp <- list()
  myXLevels <- unique(unlist(lapply(inCols, function(x) names(x$xlevels))))
  for (myLevel in myXLevels) {
    temp[[myLevel]] <- unique(unlist(lapply(inCols, function(x) ifelse(is.element(myLevel, names(x$xlevels)), x$xlevels[myLevel],NA))))
    if (any(is.na(temp[[myLevel]]))) {  # If there are NA's, get rid of them
      temp[[myLevel]] <- temp[[myLevel]][-which(is.na(temp[[myLevel]]))]
    }
  }
  #print(temp)
  myTable$xlevels <- temp

  return(myTable)
}
