#' Function to generate Latex table of summary statistics
#'
#' @param in_data Data.frame to be summarized
#' @param func_list List contaning function names as strings (default is list('mean', 'sd', 'min', 'max'))
#' @param drop_all_NA Boolean indicating whether observations with missing values should be dropped (default = FALSE)
#' @return sum_tabler object
#' @export
sum_tabler <- function(in_data, func_list = list('mean', 'sd', 'min', 'max'), drop_all_NA = FALSE) {
  # in_data has to be a data frame
  if (!is.data.frame(in_data)) stop('Must supply a data.frame to sum_tabler')

  sum_table <- list()
  attr(sum_table, "class") <- "sum_tabler"

  sum_table$var_names <- names(in_data)
  sum_table$xlevels <- list()

  for (i in seq_along(in_data)) {
    if (is.factor(in_data[[i]])) {
      sum_table$xlevels[[names(in_data)[i]]] <- levels(in_data[[i]])
    }
  }

  # If drop_all_NA == TRUE, we drop any observation that has at least one missing value
  if(drop_all_NA) {
    in_data <- na.omit(in_data)
  }

  # Loop over the variables in in_data, calling tblr_run on each one
  by_var <- lapply(in_data, tblr_fun, func_list)

  # tblr_fun returns all row names with the name "in_vec".  This replaces that
  # with the actual names of each variable.
  for (i in seq_along(by_var)) {
    rownames(by_var[[i]]) <- gsub("in_vec", names(by_var[i]), rownames(by_var[[i]]))
  }
  sum_table$values <- do.call(rbind, by_var)

  sum_table$theme <- tabler_theme()

  sum_table
}

#' This function runs a series of functions, in func_list, on a single variable, in_vec.
tblr_fun <- function(in_vec, func_list = NA) {
  a <- lapply(func_list, tblr_run, in_vec)
  ret_val <- do.call(cbind, a)
  colnames(ret_val) <- unlist(func_list)
  ret_val
}

#' Enhance Summary Table
#'
#' @export
`+.sum_tabler` <- function(sum_tabler, new_object) {
  if (class(new_object) == "tabler_theme") {
    sum_tabler$theme <- new_object
  }

  sum_tabler
}

#' This function runs a single function, f, on a vector of data, in_vec.
#' This is used by tblr_fun.
tblr_run <- function(f, in_vec) {
  my_f <- NA
  if (f == "n") {
    my_f <- function(x) sum(1 - is.na(x))
  } else {
    my_f <- get(f, envir = .GlobalEnv)  # Get the function from the Global Environment
    if (!is.function(my_f)) stop(paste0('Invalid function supplied to sum_tabler:  ', f))
  }

  apply(model.matrix(~ 0 + in_vec), 2, my_f)
}
