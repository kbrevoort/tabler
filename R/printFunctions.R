#' Print Method for Tabler Object
#'
#' @params myTable Tabler Object
#' @examples
#' print(myTable)
#' @export
print.tablerObject <- function(myTable) {
  if (myTable$theme$style == 'markdown') print_latex(myTable)
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
      cat(sprintf(" & & %s \\\\ \n", pca(outVec)))
    }
    else if (thisChar == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coefList <- tabulateCoef(myTable$coefs, myTable$theme)
      for (i in 1:length(coefList)) {
        #browser()
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n & & %s \\\\ \n",
                    coefList[[i]][1],
                    pca(coefList[[i]][2:(numCols+1)]),
                    pca(coefList[[i]][(numCols + 2):(2*numCols + 1)],
                        pre = "(",
                        post = ")")))
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
  if (!is.na(myTable$latex_label)) cat(sprintf("\\label{%s} \n", myTable$latex_label))
  cat("\\end{table} \n")
}

tabulateCoef <- function(coefs, theme) {
  numCols <- max(coefs$estNum)
  numVars <- max(coefs$order)

  outVec <- vector('list', numVars)  # pre-allocate list

  for (i in 1:numVars) {
    if (i %in% coefs$order) {
      thisRow <- coefs[coefs$order == i, ]
      line1 <- rep("", numCols)
      line2 <- rep("", numCols)

      line1[thisRow$estNum] <- prettyNum(thisRow$est, digits = theme$digits[1])
      line2[thisRow$estNum] <- prettyNum(thisRow$std, digits = theme$digits[2])

      outVec[[i]] <- c(thisRow[1,1], line1, line2)
    }
  }
  outVec
}

tabulateGOF <- function(gofs, theme) {
  outDF <- data.frame(t(gofs))

  # Using the square brackets preserves the output as a data.frame
  outDF[] <- lapply(outDF, function(x) prettyNum(x, digits = theme$digits[1], big.mark = ','))
  outDF
}

