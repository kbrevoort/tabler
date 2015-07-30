tablerOpts <- function(showDepVar = TRUE,
                    showMethod = TRUE,
                    digits = c(3,2),
                    sigLevels = c('***'=0.01, '**' = 0.05, '*' = 0.1)) {

  optsList <- list()
  attr(optsList, "class") <- "tableROpts"
  optsList$showDepVar <- showDepVar
  optsList$showMethod <- showMethod

  optsList$digits <- digits
  if (length(digits) == 1) optsList$digits <- c(digits, digits)

  optsList$sigLevels <- sort(sigLevels)
  return(optsList)
}

# pca stands for "paste - collapse - ampersand
pca <- function(inVec) paste(inVec, collapse = ' & ')

print.latex <- function(myTable,
                        myOpts = tablerOpts(),
                        outfile = NA,
                        intitle = NA,
                        inlabel = NA,
                        rename = NA,
                        suppress = NA,
                        summarize = NA) {
  # This function will take a table object and print it to the screen in LaTeX format.

  if (!class(myTable) == "tableRec") stop("Must supply valid tableRec object to print.latex")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  if (!is.na(outfile)) {
    sink(outfile, split = TRUE)
    on.exit(sink())
  }

  # Number of columns of data in the table
  numCols <- length(myTable$depVars)

  cat("\\begin{table}[ht]\n")
  if (!is.na(intitle)) cat(sprintf("\\caption{%s}\n", intitle))
  cat("\\centering\n")
  cat(sprintf("\\begin{tabular}{ll%s}\n", paste0(rep("c", numCols), collapse = "")))
  cat("\\hline\\hline\n")

  if (myOpts$showMethod) cat(sprintf("  \\multicolumn{2}{r}{Method:} & %s \\\\ \n",
                                     pca(myTable$estTypes)))

  if (myOpts$showDepVar) cat(sprintf("  \\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n",
                                     pca(myTable$depVars)))

  cat(sprintf(" & & %s \\\\ \n",
              pca(sprintf("(%i)", 1:numCols))))
  cat("\\hline \n")

  # This will cycle through the variables
  for (i in seq_along(myTable$varNames)) {
    thisVar <- myTable$varNames[i]

    # If this variable is in the suppression list, skip to the next
    if (thisVar %in% suppress) next

    if (thisVar %in% names(myTable$xlevels)) {  # Factor variables
      outVar <- thisVar
      if (thisVar %in% names(rename)) outVar <- rename[[thisVar]]
      cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
                  outVar,
                  pca(rep(' ', numCols))))

      # Print out a line with just the variable name
      for (j in myTable$xlevels[[thisVar]]) {
        outLine <- generateOut(thisVar, j, coefMat = myTable$coefs, suppress, myOpts)
        if (outLine == "") next
        if (j %in% names(rename)) j <- rename[[j]]
        cat(sprintf(" & %s & %s", j, outLine))
      }
    } else {  # Not a factor variable
      outLine <- generateOut(thisVar, "", coefMat = myTable$coefs, suppress = suppress, myOpts = myOpts)
      if (outLine == "") next
      if (thisVar %in% names(rename)) thisVar <- rename[[thisVar]]
      cat(sprintf("%s & & %s", thisVar, outLine))
    }
  }

  # Now deal with the summarized variables
  if (!is.na(summarize)) {
    for (myVar in summarize) {
      outLine <- rep(" ", numCols)
      myEstNums <- NULL
      if (myVar %in% names(myTable$xlevels)) { # myVar is a factor
        for (myFact in myTable$xlevels[[myVar]]) {
          thisVar <- paste0(myVar, myFact)
          myEstNums <- c(myEstNums, myTable$coefs[myTable$coefs[['varName']] == thisVar, 'estNum'])
        }
        myEstNums <- unique(myEstNums)
      } else { # myVar is not a factor
        myEstNums <- myTable$coefs[myTable$coefs[['varName']] == myVar, 'estNum']
      }
      outLine[myEstNums] <- "Y"
      if (myVar %in% names(rename)) myVar <- rename[[myVar]]
      cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
                  myVar,
                  pca(outLine)))
    }
  }

  # Now add goodness of fit statistics
  cat("\\hline \n")

  for (fitStat in names(myTable$gofs)) {
    # Calculate the appropriate number of digits
    numScale <- log10(max(abs(myTable$gofs[[fitStat]])))
    cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
                fitStat,
                pca(prettyNum(myTable$gofs[[fitStat]],
                                big.mark = ",",
                                digits = myOpts$digits[1],
                                preserve.width = "none"))))
  }

  cat("\\hline \n")
  cat("\\end{tabular} \n")
  if (!is.na(inlabel)) cat(sprintf("\\label{%s} \n", inlabel))
  cat("\\end{table} \n")
}

generateOut <- function(varName = "", factName = "", coefMat, suppress, myOpts) {
  if (varName == "" & factName == "") stop('No variable names supplied to generateOut')

  thisVarName <- paste0(varName, factName)
  keepInd <- which(coefMat$varName == thisVarName)

  # Check if this specific factor is in the suppression list
  if (thisVarName %in% suppress) return("")

  if (length(keepInd) <= 0) return("")

  # I need three variables
  numCols <- max(coefMat$estNum)
  coefVal <- as.list(rep(" ", numCols))
  coefStd <- as.list(rep(" ", numCols))

  thisCoefs <- coefMat[keepInd,]

  # These are formats that will be used in the sprintf commands for the coefficients
  # and standard errors.  They allow the number of digits to be specified by the user.
  coefFmt <- sprintf("$%%.%if^{%%s}$", myOpts$digits[1])
  stdFmt <- sprintf("$(%%.%if)$", myOpts$digits[2])

  coefVal[thisCoefs[['estNum']]] <- sprintf(coefFmt,
                                            thisCoefs[['est']],
                                            as.character(cut(thisCoefs[['pval']],
                                                             breaks = c(-1, unname(myOpts$sigLevels), 1),
                                                             labels = c(names(myOpts$sigLevels), ""),
                                                             right = TRUE)))
  coefStd[thisCoefs[['estNum']]] <- sprintf(stdFmt, thisCoefs[['std']])

  retVal <- sprintf(" %s \\\\ \n & & %s \\\\ \n",
                    pca(unlist(coefVal)),
                    pca(unlist(coefStd)))
}

