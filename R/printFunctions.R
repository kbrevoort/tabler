#' Print Method for Tabler Object
#'
#' @params myTable Tabler Object
#' @examples
#' print(myTable)
#' @export
print.tablerObject <- function(myTable) {
  if (myTable$theme$style == 'markdown') print_markdown(myTable)
  else if (myTable$theme$style == 'latex') print_latex(myTable)
  else print_html(myTable)
}

#' Print as a LaTeX Table.
#'
#' Prints a tablerObject as a properly formatted latex table.
#'
#' @param myTable tablerObject for printing
#'
#' @return NULL
#' @examples
#' print_latex(myTable)
print_latex <- function(myTable) {

  if (!class(myTable) == "tablerObject") stop("Must supply valid tablerObject to print")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  #if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  #if (!is.na(outfile)) {
  #  sink(outfile, split = TRUE)
  #  on.exit(sink())
  #}

  # Number of columns of data in the table
  numCols <- length(myTable$depVars)

  cat("\\begin{table}[ht]\n")
  if (!is.na(myTable$title)) cat(sprintf("\\caption{%s}\n", myTable$title))
  cat("\\centering\n")
  cat(sprintf("\\begin{tabular}{ll%s}\n", paste0(rep("c", numCols), collapse = "")))

  # Now cycle through the oder in tabler theme order
  for (i in seq(nchar(myTable$theme$order))) {
    thisChar <- substr(myTable$theme$order, i, i)
    if (thisChar == '=') cat("\\hline\\hline\n")
    else if (thisChar == '-') cat("\\hline\n")
    else if (thisChar == 'D') cat(sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n", pca(myTable$depVar)))
    else if (thisChar == 'M') cat(sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" , pca(myTable$estTypes)))
    else if (thisChar == 'N') {
      outVec <- c(1:numCols)
      if (myTable$theme$colNumberStyle == 'parenthetic') outVec <- sprintf('(%i)', outVec)
      else if (myTable$theme$colNumberStyle == 'roman') outVec <- as.roman(outVec)
      else outVec <- as.character(outVec)
    }
    else if (thisChar == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coefMat <- tabulateCoef(myTable$coefs, myTable$theme)
      for (i in 1:dim(coefMat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(coefMat)[i], pca(coefMat[i, ])))
      }
    }
    else if (thisChar == 'G') {
      # The Goodness of fit process will follow the same two step process as the coefficient output:  Get
      # properly formatted output and then output to the screen.
      gofMat <- tabulateGOF(myTable$gofs, myTable$theme)
      for (i in 1:dim(gofMat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(gofMat)[i], pca(gofMat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', thisChar))
  }

  # Close out the LaTeX table
  cat("\\end{tabular} \n")
  if (!is.na(inlabel)) cat(sprintf("\\label{%s} \n", inlabel))
  cat("\\end{table} \n")
}

tabulateCoef <- function(coefs, theme) {

}

tabulateGOF <- function(gofs, theme) {
  outDF <- data.frame(t(gofs))

  # Using the square brackets preserves the output as a data.frame
  outDF[] <- lapply(outDF, function(x) prettyNum(x, digits = theme$digits[1], big.mark = ','))
  outDF
}

#   cat("\\hline\\hline\n")
#
#   if (myOpts$showMethod) cat(sprintf("  \\multicolumn{2}{r}{Method:} & %s \\\\ \n",
#                                      pca(myTable$estTypes)))
#
#   if (myOpts$showDepVar) cat(sprintf("  \\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n",
#                                      pca(myTable$depVars)))
#
#   cat(sprintf(" & & %s \\\\ \n",
#               pca(sprintf("(%i)", 1:numCols))))
#   cat("\\hline \n")
#
#   # This will cycle through the variables
#   for (i in seq_along(myTable$varNames)) {
#     thisVar <- myTable$varNames[i]
#
#     # If this variable is in the suppression list, skip to the next
#     if (thisVar %in% suppress) next
#
#     if (thisVar %in% names(myTable$xlevels)) {  # Factor variables
#       outVar <- thisVar
#       if (thisVar %in% names(rename)) outVar <- rename[[thisVar]]
#       cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
#                   outVar,
#                   pca(rep(' ', numCols))))
#
#       # Print out a line with just the variable name
#       for (j in myTable$xlevels[[thisVar]]) {
#         outLine <- generateOut(thisVar, j, coefMat = myTable$coefs, suppress, myOpts)
#         if (outLine == "") next
#         if (j %in% names(rename)) j <- rename[[j]]
#         cat(sprintf(" & %s & %s", j, outLine))
#       }
#     } else {  # Not a factor variable
#       outLine <- generateOut(thisVar, "", coefMat = myTable$coefs, suppress = suppress, myOpts = myOpts)
#       if (outLine == "") next
#       if (thisVar %in% names(rename)) thisVar <- rename[[thisVar]]
#       cat(sprintf("%s & & %s", thisVar, outLine))
#     }
#   }
#
#   # Now deal with the summarized variables
#   if (!is.na(summarize)) {
#     for (myVar in summarize) {
#       outLine <- rep(" ", numCols)
#       myEstNums <- NULL
#       if (myVar %in% names(myTable$xlevels)) { # myVar is a factor
#         for (myFact in myTable$xlevels[[myVar]]) {
#           thisVar <- paste0(myVar, myFact)
#           myEstNums <- c(myEstNums, myTable$coefs[myTable$coefs[['varName']] == thisVar, 'estNum'])
#         }
#         myEstNums <- unique(myEstNums)
#       } else { # myVar is not a factor
#         myEstNums <- myTable$coefs[myTable$coefs[['varName']] == myVar, 'estNum']
#       }
#       outLine[myEstNums] <- "Y"
#       if (myVar %in% names(rename)) myVar <- rename[[myVar]]
#       cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
#                   myVar,
#                   pca(outLine)))
#     }
#   }
#
#   # Now add goodness of fit statistics
#   cat("\\hline \n")
#
#   for (fitStat in names(myTable$gofs)) {
#     # Calculate the appropriate number of digits
#     numScale <- log10(max(abs(myTable$gofs[[fitStat]])))
#     cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
#                 fitStat,
#                 pca(prettyNum(myTable$gofs[[fitStat]],
#                               big.mark = ",",
#                               digits = myOpts$digits[1],
#                               preserve.width = "none"))))
#   }
#
#   cat("\\hline \n")
#   cat("\\end{tabular} \n")
#   if (!is.na(inlabel)) cat(sprintf("\\label{%s} \n", inlabel))
#   cat("\\end{table} \n")
#
# }
