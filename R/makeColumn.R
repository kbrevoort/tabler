# This R script will contain functions that create column objects that record the
# information which will be ouput into the table.  These functions will return
# S3 objects.
#
# Each object will be a list with the following elements
#
# class:  "colRec"
# depVar:  "character"
# varNames:  "character" vector
# estType: "character"
# xlevels: "character"
# coefs:  "matrix"
# gofs: "data.frame"

#' Make Column object from single lm result.
#'
makeColumn.lm <- function(inResult) {

  inSummary <- summary(inResult)

  myCol <- list()
  attr(myCol, "class") <- "colRec"
  myCol$depVar <- as.character(attributes(inResult$terms)$variables[[2]])
  myCol$varNames <- attributes(inResult$terms)$term.labels
  myCol$estType <- class(inResult)[1]
  myCol$xlevels <- inResult$xlevels

  myCol$coefs <- data.frame(inSummary$coefficient)
  names(myCol$coefs) <- c('est','std','tval','pval')
  myCol$coefs$varName <- rownames(myCol$coefs)
  rownames(myCol$coefs) <- NULL

  if ( "(Intercept)" %in% rownames(myCol$coefs) ) myCol$varNames <- c("(Intercept)",myCol$varNames)

  # Add the R-squared to the goodness of fit statistic list
  gof <- data.frame(r2=inSummary$r.squared, nobs=nobs(inResult))
  names(gof) <- c("R-Squared", "Observations")

  # This will add the man of the dependent variable
  if (is.element("model", names(inResult))) gof[["Dep. Var. Mean"]] <- mean(inResult$model[,1])

  myCol$gof <- gof

  return(myCol)
}
