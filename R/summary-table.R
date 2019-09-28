#' Function to generate Latex table of summary statistics
#'
#' @param in_data Data.frame to be summarized
#' @param func_list List contaning function names as strings (default is list('mean', 'sd', 'min', 'max'))
#' @param na.rm Boolean indicating whether observations with missing values should be dropped (default = FALSE)
#' @return sum_tabler object
#' @export
sum_tabler <- function(in_data,
                       func_list = list('mean', 'sd', 'min', 'max'),
                       na.rm = FALSE,
                       omit = NULL,
                       alias = NULL) {
  # in_data has to be a data frame
  if (!is.data.frame(in_data)) stop('Must supply a data.frame to sum_tabler')

  sum_table <- list()
  attr(sum_table, "class") <- "sum_tabler"

  sum_table$osa <- create_osa(omit = omit, alias = alias)

  xlevels <- purrr::map(in_data, ~ if(is.factor(.x)) levels(.x) else NULL)
  xlevels <- xlevels[!vapply(xlevels, is.null, TRUE)]
  sum_table$xlevels <- xlevels

  sum_table$var_dt <- purrr::map_dfr(names(in_data),
                                     summarize_variables,
                                     data = in_data,
                                     func_list = func_list,
                                     na.rm = na.rm,
                                     xlevels = xlevels)

  sum_table$title <- ''
  sum_table$notes <- NA
  sum_table$number <- NA
  sum_table$latex_label <- NA

  sum_table$theme <- tabler_theme()

  sum_table
}

is_sumtabler <- function(x) {
  any(class(x) == 'sum_tabler')
}

#' Summarize Variables
#'
#' Takes the name of a variable in the data.frame supplied to sum_tabler
#' and calculates the summary statistics for that variable.
#'
#' This is an internal function.
#' @param nm Character scalar of a variable name from the data.frame
#' @param data Data.frame containing variable `nm`.
#' @param func_list List of functions to use to evaluate variable expressed
#' as a character vector
#' @param na.rm Boolean indicating if NA values should be removed before running
#' functions
#' @param xlevels List with named vectors of factor levels
#' @return A data.frame with the summary table rows for the variable
summarize_variables <- function(nm, data, func_list, na.rm, xlevels) {
  x <- data[[nm]]
  if (is.factor(x)) {
    if (nm %in% names(xlevels)) {
      ret_val <- vector('list', length(xlevels[[nm]]))
      for (i in 1:length(xlevels[[nm]])) {
        my_level <- xlevels[[nm]][i]
        ret_val[[i]] <- summarize_single_var(as.numeric(x == my_level),
                                             func_list = func_list,
                                             na.rm = na.rm) %>%
          mutate(base = nm) %>%
          mutate(suffix = my_level) %>%
          list_first('base', 'suffix')
      }
      return(dplyr::bind_rows(ret_val))
    }
    stop('Factor variable not found among xlevels.')
  } else {
    summarize_single_var(x, func_list = func_list, na.rm = na.rm) %>%
      mutate(base = nm, suffix = '') %>%
      list_first('base', 'suffix')
  }
}

#' Summarize a Single Variable
#'
#' Implements the calculations from summarize_variables.
#'
#' This is an internal function.
#' @param x A numeric vector
#' @param func_list Character vector with names of functions to evaluate
#' @param na.rm Boolean
summarize_single_var <- function(x, func_list, na.rm) {
  if (na.rm == TRUE)
    x <- x[!is.na(x)]

  purrr::map_dfc(func_list, run_single_function, x = x)
}

#' Run Single Function
#'
#' Take a single function name and apply it to x.
run_single_function <- function(f_name, x) {
  f <- get(f_name)
  ret_dt <- data.frame(z = f(x))
  names(ret_dt) <- f_name

  ret_dt
}
