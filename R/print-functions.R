#' #' Print Method for Tabler Object
#' #'
#' #' @param in_table Tabler Object
#' #' @examples
#' #' print(in_table)
#' #' @export
#' print.tabler_object <- function(in_tabler) {
#'   if (in_tabler$theme$style == 'markdown') {
#'     print_latex(in_tabler)
#'   } else if (in_tabler$theme$style == 'latex') {
#'     print_latex(in_tabler)
#'   } else print_html(in_tabler)
#' }

#' Print a Summary Table.
#'
#' Prints a sum_tabler object.
#' @export
print.sum_tabler <- function(in_tabler) {
  if (in_tabler$theme$style == 'markdown') print_latex_sum(in_tabler)
  else if (in_tabler$theme$style == 'latex') print_latex_sum(in_tabler)
  else print_latex_sum(in_tabler)
}

#' Prints the Summary Table in Latex
print_latex_sum <- function(in_tabler) {
  if (!class(in_tabler) == "sum_tabler") stop("Must supply valid tablerObject to print")

  num_cols <- dim(in_tabler$values)[2]

  cat("\\begin{table}[ht]\n")
  if (!is.na(in_tabler$title)) cat(sprintf("\\caption{%s}\n", in_tabler$title))
  cat("\\centering\n")
  cat(sprintf("\\begin{tabular}{l%s}\n", paste0(rep("c", num_cols), collapse = "")))

  # Now cycle through the order in tabler theme sum_order
  for (i in seq(nchar(in_tabler$theme$sum_order))) {
    this_char <- substr(in_tabler$theme$sum_order, i, i)
    if (this_char == '=') cat("\\hline\\hline\n")
    else if (this_char == '-') cat("\\hline\n")
    else if (this_char == 'S') cat(sprintf(" & %s \\\\ \n", pca(colnames(in_tabler$values))))
    else if (this_char == 'N') {
      out_vec <- c(1:num_cols)
      if (in_tabler$theme$col_number_style == 'parenthetic') out_vec <- sprintf('(%i)', out_vec)
      else if (in_tabler$theme$col_number_style == 'roman') out_vec <- as.roman(out_vec)
      else out_vec <- as.character(out_vec)
      cat(sprintf(" & %s \\\\ \n", pca(out_vec)))
    }
    else if (this_char == 'V') {
      for (var_name in in_tabler$var_names) {
        if (var_name %in% names(in_tabler$xlevels)) {  # Variable is a factor
          cat(sprintf("%s & %s \\\\ \n",
                      var_name,
                      pca(rep(" ", num_cols))))
          for (fact_name in in_tabler$xlevels[[var_name]]) {
            this_name <- paste0(var_name, fact_name)
            cat(sprintf("\\hline*[3em] %s & %s \\\\ \n",
                        fact_name,
                        pca(prettyNum(in_tabler$values[this_name, ],
                                      digits = 3,
                                      big.mark = ','))))
          }
        } else {  # Variable is not a factor
          if (var_name %in% rownames(in_tabler$values)) {
            cat(sprintf("%s & %s \\\\ \n",
                        var_name,
                        pca(prettyNum(in_tabler$values[var_name, ],
                                      digits = 3,
                                      big.mark = ','))))
          }
        }
      }
    } else warning(sprintf('Invalid element in theme sum_order string:  %s', this_char))
  }

  # Close out the LaTeX table
  cat("\\end{tabular} \n")
  if (!is.na(in_tabler$latex_label)) cat(sprintf("\\label{%s} \n", in_tabler$latex_label))
  cat("\\end{table} \n")
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

  if (!class(in_tabler) == "tabler_object") stop("Must supply valid tablerObject to print")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  #if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  #if (!is.na(outfile)) {
  #  sink(outfile, split = TRUE)
  #  on.exit(sink())
  #}

  # Number of columns of data in the table
  num_cols <- length(in_tabler$dep_vars)

  out_text <- "\\begin{table}[ht]\n"

  # Add title if available
  if (!is.na(in_tabler$title))
    out_text <- sprintf("%s\\caption{%s}\n", out_text, in_tabler$title)

  out_text <- sprintf("%s\\centering\n\\begin{tabular}{ll%s}\n",
                      out_text,
                      paste0(rep("c", num_cols), collapse = ""))

  # Now cycle through the order in tabler theme order
  for (i in seq(nchar(in_tabler$theme$order))) {
    this_char <- substr(in_tabler$theme$order, i, i)
    if (this_char == '=') {
      out_text <- paste0(out_text, "\\hline\\hline\n")
    } else if (this_char == '-') {
      out_text <- paste0(out_text, "\\hline\n")
    } else if (this_char == 'D') {
      out_text <- paste0(out_text,
                         sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n",
                                 pca(in_tabler$dep_vars)))
    } else if (this_char == 'M') {
      out_text <- paste0(out_text,
                         sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" ,
                                 pca(in_tabler$est_types)))
    } else if (this_char == 'N') {
      out_vec <- c(1:num_cols)
      if (in_tabler$theme$col_number_style == 'parenthetic') {
        out_vec <- sprintf('(%i)', out_vec)
      } else if (in_tabler$theme$col_number_style == 'roman') {
        out_vec <- as.roman(out_vec)
      } else out_vec <- as.character(out_vec)
      out_text <- paste0(out_text,
                         sprintf(" & & %s \\\\ \n",
                                 pca(out_vec)))
    } else if (this_char == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coef_list <- tabulate_coef(in_tabler$coefs, in_tabler$theme)
      on_factor <- NA
      for (i in 1:length(coef_list)) {
        this_var <- coef_list[[i]][1]  # Variable being output

        # If the last entry was a factor (given by on_factor), check if this variable is also
        # an element of the same factor, otherwise set on_factor back to NA
        if (!is.na(on_factor)) {
          if (this_var %in% sprintf('%s%s', on_factor, in_tabler$xlevels[[on_factor]])) {
            out_text <- output_coef(gsub(on_factor, '', this_var),
                                    2,
                                    coef_list[[i]],
                                    num_cols) %>%
              paste0(out_text, .)
            next
          } else {
            on_factor <- NA
          }
        }

        if (this_var %in% in_tabler$var_names) {  # Regular variable
          out_text <- output_coef(this_var, 1, coef_list[[i]], num_cols) %>%
            paste0(out_text, .)
        } else {
          # Check if this variable is one of the factors
          for (this_factor in names(in_tabler$xlevels)) {
            if (this_var %in% sprintf('%s%s', this_factor, in_tabler$xlevels[[this_factor]])) {
              out_text <- paste0(out_text,
                                 sprintf('%s %s \\\\ \n',
                                         this_factor,
                                         paste0(rep(' & ', num_cols + 1),
                                                collapse = '')))
              out_text <- paste0(out_text,
                                 output_coef(gsub(this_factor, '', this_var),  # Send only the factor level
                                             2,
                                             coef_list[[i]],
                                             num_cols))
              on_factor <- this_factor
            }
          }

          if (is.na(on_factor)) {  # This means the variable is not a factor or in the var_names list
            out_text <- paste0(out_text,
                               output_coef(this_var, 1, coef_list[[i]], num_cols))
          }
        }
      }
    }
    else if (this_char == 'G') {
      # The Goodness of fit process will follow the same two step process as the coefficient output:  Get
      # properly formatted output and then output to the screen.
      gof_Mat <- tabulate_GOF(in_tabler$gofs, in_tabler$theme)
      for (i in 1:dim(gof_Mat)[1]) {
        out_text <- paste0(out_text,
                           sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n",
                                   rownames(gof_Mat)[i],
                                   pca(gof_Mat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', this_char))
  }

  # Close out the LaTeX table
  out_text <- paste0(out_text, "\\end{tabular} \n")
  if (!is.na(in_tabler$latex_label))
    out_text <- paste0(out_text,
                       sprintf("\\label{%s} \n",
                               in_tabler$latex_label))
  out_text <- paste0(out_text, "\\end{table} \n")

  print(out_text)
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

  if (!class(in_tabler) == "tabler_object") stop("Must supply valid tablerObject to print")

  # Make sure anything in summarize is also in suppress (summarized variables will be handled
  # at the end of the program)
  #if (!is.na(summarize)) suppress <- unique(c(suppress, summarize))

  # Output can be sent to the screen, a file, or both
  #if (!is.na(outfile)) {
  #  sink(outfile, split = TRUE)
  #  on.exit(sink())
  #}

  # Number of columns of data in the table
  num_cols <- length(in_tabler$depVars)

  cat("\\begin{table}[ht]\n")
  if (!is.na(in_tabler$title)) cat(sprintf("\\caption{%s}\n", in_tabler$title))
  cat("\\centering\n")
  cat(sprintf("\\begin{tabular}{ll%s}\n", paste0(rep("c", num_cols), collapse = "")))

  # Now cycle through the oder in tabler theme order
  for (i in seq(nchar(in_tabler$theme$order))) {
    this_char <- substr(in_tabler$theme$order, i, i)
    if (this_char == '=') cat("\\hline\\hline\n")
    else if (this_char == '-') cat("\\hline\n")
    else if (this_char == 'D') cat(sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n", pca(in_tabler$dep_vars)))
    else if (this_char == 'M') cat(sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" , pca(in_tabler$est_types)))
    else if (this_char == 'N') {
      out_vec <- c(1:num_cols)
      if (in_tabler$theme$colNumberStyle == 'parenthetic') out_vec <- sprintf('(%i)', out_vec)
      else if (in_tabler$theme$colNumberStyle == 'roman') out_vec <- as.roman(out_vec)
      else out_vec <- as.character(out_vec)
      cat(sprintf(" & & %s \\\\ \n", pca(out_vec)))
    }
    else if (this_char == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coef_list <- tabulateCoef(in_tabler$coefs, in_tabler$theme)
      for (i in 1:length(coef_list)) {
        #browser()
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n & & %s \\\\ \n",
                    coef_list[[i]][1],
                    pca(coef_list[[i]][2:(num_cols+1)]),
                    pca(coef_list[[i]][(num_cols + 2):(2*num_cols + 1)],
                        pre = "(",
                        post = ")")))
      }
    }
    else if (this_char == 'G') {
      # The Goodness of fit process will follow the same two step process as the coefficient output:  Get
      # properly formatted output and then output to the screen.
      gof_Mat <- tabulate_GOF(in_tabler$gofs, in_tabler$theme)
      for (i in 1:dim(gof_Mat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(gof_Mat)[i], pca(gof_Mat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', this_char))
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
    this_char <- substr(in_tabler$theme$order, i, i)
    if (this_char == '=') cat("\\hline\\hline\n")
    else if (this_char == '-') cat("\\hline\n")
    else if (this_char == 'D') cat(sprintf("\\multicolumn{2}{r}{Dep. Variable:} & %s \\\\ \n", pca(in_tabler$depVar)))
    else if (this_char == 'M') cat(sprintf("\\multicolumn{2}{r}{Method:} & %s \\\\ \n" , pca(in_tabler$estTypes)))
    else if (this_char == 'N') {
      out_vec <- c(1:num_cols)
      if (in_tabler$theme$colNumberStyle == 'parenthetic') out_vec <- sprintf('(%i)', out_vec)
      else if (in_tabler$theme$colNumberStyle == 'roman') out_vec <- as.roman(out_vec)
      else out_vec <- as.character(out_vec)
      cat(sprintf(" & & %s \\\\ \n", pca(out_vec)))
    }
    else if (this_char == 'C') {
      # The coefficient output process will have two steps.  First, create a data frame with the properly
      # fomatted values (as characters).  Second, output these values in the LaTeX format.  The first
      # step will allow the underlying code to be the same for all output formats.
      coef_list <- tabulate_coef(in_tabler$coefs, in_tabler$theme)
      for (i in 1:length(coef_list)) {
        #browser()
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n & & %s \\\\ \n",
                    coef_list[[i]][1],
                    pca(coef_list[[i]][2:(num_cols+1)]),
                    pca(coef_list[[i]][(num_cols + 2):(2*num_cols + 1)],
                        pre = "(",
                        post = ")")))
      }
    }
    else if (this_char == 'G') {
      # The Goodness of fit process will follow the same two step process as the coefficient output:  Get
      # properly formatted output and then output to the screen.
      gof_Mat <- tabulate_GOF(in_tabler$gofs, in_tabler$theme)
      for (i in 1:dim(gof_Mat)[1]) {
        cat(sprintf("\\multicolumn{2}{l}{%s} & %s \\\\ \n", rownames(gof_Mat)[i], pca(gof_Mat[i, ])))
      }
    }
    else warning(sprintf('Invalid element in theme order string:  %s', this_char))
  }

  # Close out the LaTeX table
  cat("\\end{tabular} \n")
  if (!is.na(in_tabler$latex_label)) cat(sprintf("\\label{%s} \n", in_tabler$latex_label))
  cat("\\end{table} \n")
}

tabulate_coef_dt <- function(tblr_obj) {
  num_cols <- length(tblr_obj$dep_vars)
  num_vars <- length(tblr_obj$var_names) # Thiss count includes factors (but not each level)

  # THe number of factor variables (2 factors with 4 levels each would thus yield
  # 8 as num_factors)
  num_factors <- purrr::map_int(tblr_obj$xlevels, length) %>%
    sum()

}


tabulate_coef <- function(coefs, theme) {
  num_cols <- max(coefs$est_num)
  num_vars <- max(coefs$order)

  out_vec <- vector('list', num_vars)  # pre-allocate list

  for (i in 1:num_vars) {
    if (i %in% coefs$order) {
      this_row <- dplyr::filter(coefs, order == i)
      line1 <- rep("", num_cols)
      line2 <- rep("", num_cols)

      line1[this_row$est_num] <- prettyNum(this_row$estimate,
                                           digits = theme$digits[1])
      line2[this_row$est_num] <- prettyNum(this_row$std.error,
                                           digits = theme$digits[2])

      out_vec[[i]] <- c(this_row$term[1], line1, line2)
    }
  }
  out_vec
}

tabulate_GOF <- function(gofs, theme) {
  outDF <- data.frame(t(gofs))

  # Using the square brackets preserves the output as a data.frame
  outDF[] <- lapply(outDF,
                    function(x) prettyNum(x, digits = theme$digits[1], big.mark = ','))
  outDF
}

