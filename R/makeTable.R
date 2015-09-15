#' Make a tabler object
#'
#' This function produces the underlying tabler object
#' @export
tabler <- function(...) {
  inCols <- list(...)

  # Check to make sure that every element of inCols is a colRec
  for (i in seq_along(inCols)) {
    if (class(inCols[[i]]) != "colRec") {
      inCols[[i]] <- makeColumn(inCols[[i]])
    }
  }

  # Create the tabler object
  myTable <- list()
  attr(myTable, "class") <- "tablerObject"
  myTable$title <- NA
  myTable$notes <- NA
  myTable$number <- NA

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
  myTable$xlevels <- temp

  myTable$theme <- tabler_theme() # Set the theme values as defaults

  # Order the coefficient vector
  myTable$coefs <- variableOrder(myTable$varNames, myTable$xlevels, myTable$coefs)

  return(myTable)
}


`+.tablerObject` <- function(myTable, inObject) {
  if (class(inObject) == "tablerTheme") myTable$theme <- inObject
  else {
    if (class(inObject) != "colRec") inObject <- makeColumn(inObject)

    myTable$depVars <- c(myTable$depVars, inObject$depVar)
    myTable$varNames <- unique(c(myTable$varNames, inObject$varNames))
    myTable$estTypes <- c(myTable$estTypes, inObject$estType)

    # Stack the coefficient data.frames
    inObject$coefs$estNum <- max(myTable$coefs$estNum) + 1
    myTable$coefs <- rbind(myTable$coefs, inObject$coefs)

    # Combine the gof data.frames.  This is complicated because the statistics will differ across
    # the columns.
    newNames <- names(inObject$gof)[names(inObject$gof) %notin% names(myTable$gofs)]
    myTable$gofs[, newNames] <- NA
    emptyNames <- names(myTable$gofs)[names(myTable$gofs) %notin% names(inObject$gof)]
    inObject$gof[, emptyNames] <- NA
    myTable$gofs <- rbind(myTable$gofs, inObject$gof[, names(myTable$gofs)])

    # Consolidate xlevels
    # There must be a more efficient way to do this.
    temp <- list()
    myXLevels <- unique(c(names(myTable$xlevels), names(inObject$xlevels)))
    for (myLevel in myXLevels) {
      if (myLevel %in% names(myTable$xlevels)) {
        if (myLevel %in% names(inObject$xlevels)) {
          temp[[myLevel]] <- unique(c(myTable$xlevels[[myLevel]], inObject$xlevels[[myLevel]]))
        } else {
          temp[[myLevel]] <- myTable$xlevels[[myLevel]]
        }
      } else {
        temp[[myLevel]] <- inObject$xlevels[[myLevel]]
      }
    }
    myTable$xlevels <- temp
  }

  # Order the coefficient vector
  myTable$coefs <- variableOrder(myTable$varNames, myTable$xlevels, myTable$coefs)

  myTable
}

