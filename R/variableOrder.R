variableOrder <- function(varNames, xlevels, coefs) {
  # Get rid of existing order number if it exists.  The new order number will
  # be added in this function
  if ('order' %in% names(coefs)) coefs$order <- NULL

  outVars <- "(Intercept)"

  for (thisVar in varNames) {
    if (grepl(":", thisVar)) {  # Variable is an interaction
      templist <- NULL
      myVars <- strsplit(thisVar, ":")[[1]]
      for (i1 in myVars) {
        if (i1 %in% names(xlevels)) {  # Factor
          if (is.null(templist)) {
            templist <- list(xlevels[[i1]])
          } else {
            templist <- list(templist, xlevels[[i1]])
          }
        } else { # Non-factor
          if (is.null(templist)) {
            templist <- list(i1)
          } else {
            templist <- list(templist, i1)
          }
        }
      }
      alloptions <- expand.grid(templist)
      varList <- apply(alloptions, 1, function(x) paste(x, collapse = ':'))
      outVars <- c(outVars, varList)
    } else if (thisVar %in% names(xlevels)) {  # Variable is a factor
      outVars <- c(outVars, sprintf('%s%s', thisVar, xlevels[[thisVar]]))
    } else {
      outVars <- c(outVars, thisVar)
    }
  }
  nameOrder <- data.frame(outVars, order = seq(from = 1, to = length(outVars), by = 1))

  # I want to make sure that I don't drop any observations, so this section
  # looks for variables that do not appear in nameOrder and adds them to the end
  # of the list if found
  addNames <- coefs$varname[unique(coefs$varName) %notin% nameOrder]
  numNew <- length(addNames)
  if (numNew > 0) rbind(nameOrder, cbind(addNames, max(coefs$estNum) + c(1:numNew)))

  withOrder <- merge(coefs, nameOrder, by.x = 'varName', by.y = 'outVars')
  withOrder[order(withOrder$order, withOrder$estNum), ]
}
