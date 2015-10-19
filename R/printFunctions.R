#' Print Method for Tabler Object
#'
#' @params in_table Tabler Object
#' @examples
#' print(in_table)
#' @export
print.tablerObject <- function(in_tabler) {
  if (in_tabler$theme$style == 'markdown') print_latex(in_tabler)
  else if (in_tabler$theme$style == 'latex') print_latex(in_tabler)
  else print_html(in_tabler)
}

#' Print as a LaTeX Table.
#'
#' Prints a tablerObject as a properly formatted latex table.
#'
#' @param in_tabler tablerObject for printing
#'
#' @return NULL
#' @examples
#' print_latex(in_tabler)
print_latex <- function(in_tabler) {

  if (!class(in_tabler) == "tablerObject") stop("Must supply valid tablerObject to print")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  #if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  #if (!is.na(outfile)) {
  #  sink(outfile, split = TRUE)
  #  on.exit(sink())
  #}

  # Number of columns of data in the table
  numCols <- length(in_tabler$depVars)

  cat("\\begin{table}[ht]\n")
  if (!is.na(in_tabler$title)) cat(sprintf("\\caption{%s}\n", in_tabler$title))
  cat("\\centering\n")
  cat(sprintf("\\begin{tabular}{ll%s}\n", paste0(rep("c", numCols), collapse = "")))

  # Now cycle through the oder in tabler theme order
  for (i in seq(nchar(in_tabler$theme$order))) {
    thisChar <- substr(in_tabler$theme$order, i, i)
    if (thisChar == '=') cat("\\hline\\hline\n")
    else if (thisChar == '-') cat("\\hline\n")
    else if (thisChar == 'D') cat(sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n", pca(in_tabler$depVar)))
    else if (thisChar == 'M') cat(sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" , pca(in_tabler$estTypes)))
    else if (thisChar == 'N') {
      outVec <- c(1:numCols)
      if (in_tabler$theme$colNumberStyle == 'parenthetic') outVec <- sprintf('(%i)', outVec)
      else if (in_tabler$theme$colNumberStyle == 'roman') outVec <- as.roman(outVec)
      else outVec <- as.character(outVec)
      cat(sprintf(" & & %s \\\\ \n", pca(outVec)))
    }
    else if (thisChar == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coefList <- tabulateCoef(in_tabler$coefs, in_tabler$theme)
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
      gofMat <- tabulateGOF(in_tabler$gofs, in_tabler$theme)
      for (i in 1:dim(gofMat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(gofMat)[i], pca(gofMat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', thisChar))
  }

  # Close out the LaTeX table
  cat("\\end{tabular} \n")
  if (!is.na(in_tabler$latex_label)) cat(sprintf("\\label{%s} \n", in_tabler$latex_label))
  cat("\\end{table} \n")
}

#' Print as a Markdown Table.
#'
#' Prints a tablerObject as a properly formatted R Markdown table.
#'
#' @param in_tabler tablerObject for printing
#'
#' @return NULL
#' @examples
#' print_markdown(in_tabler)
print_markdown <- function(in_tabler) {

  if (!class(in_tabler) == "tablerObject") stop("Must supply valid tablerObject to print")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  #if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  #if (!is.na(outfile)) {
  #  sink(outfile, split = TRUE)
  #  on.exit(sink())
  #}

  # Number of columns of data in the table
  numCols <- length(in_tabler$depVars)

  cat("\\begin{table}[ht]\n")
  if (!is.na(in_tabler$title)) cat(sprintf("\\caption{%s}\n", in_tabler$title))
  cat("\\centering\n")
  cat(sprintf("\\begin{tabular}{ll%s}\n", paste0(rep("c", numCols), collapse = "")))

  # Now cycle through the oder in tabler theme order
  for (i in seq(nchar(in_tabler$theme$order))) {
    thisChar <- substr(in_tabler$theme$order, i, i)
    if (thisChar == '=') cat("\\hline\\hline\n")
    else if (thisChar == '-') cat("\\hline\n")
    else if (thisChar == 'D') cat(sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n", pca(in_tabler$depVar)))
    else if (thisChar == 'M') cat(sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" , pca(in_tabler$estTypes)))
    else if (thisChar == 'N') {
      outVec <- c(1:numCols)
      if (in_tabler$theme$colNumberStyle == 'parenthetic') olibutVec <- sprintf('(%i)', outVec)
      else if (in_tabler$theme$colNumberStyle == 'roman') outVec <- as.roman(outVec)
      else outVec <- as.character(outVec)
      cat(sprintf(" & & %s \\\\ \n", pca(outVec)))
    }
    else if (thisChar == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coefList <- tabulateCoef(in_tabler$coefs, in_tabler$theme)
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
      gofMat <- tabulateGOF(in_tabler$gofs, in_tabler$theme)
      for (i in 1:dim(gofMat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(gofMat)[i], pca(gofMat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', thisChar))
  }

  # Close out the LaTeX table
  cat("\\end{tabular} \n")
  if (!is.na(in_tabler$latex_label)) cat(sprintf("\\label{%s} \n", in_tabler$latex_label))
  cat("\\end{table} \n")
}

#' Print as an HTML Table
#'
#' Prints a tablerObject as a properly formatted HTML table.
#'
#' @param in_table tabler_object for printing
#' @return NULL
#' @examples
#' print_html(in_table)
print_html <- function(in_table) {

  if (!class(in_table) == "tablerObject") stop("Must supply valid tablerObject to print")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  #if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  #if (!is.na(outfile)) {
  #  sink(outfile, split = TRUE)
  #  on.exit(sink())
  #}

  # Number of columns of data in the table
  num_cols <- length(in_table$depVars)

  cat("<table style=\"width:100%\">\n")
  if (!is.na(in_table$title)) cat(sprintf("<caption>%s</caption>\n", in_table$title))


  # Now cycle through the oder in tabler theme order
  for (i in seq(nchar(in_tabler$theme$order))) {
    thisChar <- substr(in_tabler$theme$order, i, i)
    if (thisChar == '=') cat("\\hline\\hline\n")
    else if (thisChar == '-') cat("\\hline\n")
    else if (thisChar == 'D') cat(sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n", pca(in_tabler$depVar)))
    else if (thisChar == 'M') cat(sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" , pca(in_tabler$estTypes)))
    else if (thisChar == 'N') {
      outVec <- c(1:numCols)
      if (in_tabler$theme$colNumberStyle == 'parenthetic') olibutVec <- sprintf('(%i)', outVec)
      else if (in_tabler$theme$colNumberStyle == 'roman') outVec <- as.roman(outVec)
      else outVec <- as.character(outVec)
      cat(sprintf(" & & %s \\\\ \n", pca(outVec)))
    }
    else if (thisChar == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coefList <- tabulateCoef(in_tabler$coefs, in_tabler$theme)
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
      gofMat <- tabulateGOF(in_tabler$gofs, in_tabler$theme)
      for (i in 1:dim(gofMat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(gofMat)[i], pca(gofMat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', thisChar))
  }

  # Close out the LaTeX table
  cat("\\end{tabular} \n")
  if (!is.na(in_tabler$latex_label)) cat(sprintf("\\label{%s} \n", in_tabler$latex_label))
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

