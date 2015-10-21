#' Order Variables
#'
#' This function will return an ordering of the variables as they will appear in the table
order_variables <- function(var_names, xlevels, coefs) {
  # Get rid of existing order number if it exists.  The new order number will
  # be added in this function
  if ('order' %in% names(coefs)) coefs$order <- NULL

  var_names <- c(var_names, "(Intercept)")
  temp_names <- unique(coefs$var_names)
  out_vars <- NULL

  for (this_var in temp_names) {
    if (this_var %in% var_names) { # Not a factor or interaction
      out_vars <- c(out_vars, setdiff(this_var, out_vars))
    } else if (this_var %in% names(xlevels)) {  # Variable is a factor -- add all factors
      new_vars <- sprintf('%s%s', this_var, xlevels[[this_var]])
      out_vars <- c(out_vars, setdiff(new_var, out_vars))
    } else {  # Variable is probably an interaction
      out_vars <- c(out_vars, setdiff(this_var, out_vars))
    }
  }

  # Create a data.frame for merging that assigns an order number to each variable
  name_order <- data.frame(out_vars, order = seq(1, length(out_vars)))

  with_order <- merge(coefs, name_order, by.x = 'var_name', by.y = 'out_vars', all.x = TRUE)
  with_order[order(with_order$order, with_order$est_num), ]
}
