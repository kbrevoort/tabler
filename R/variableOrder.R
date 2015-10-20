order_variables <- function(var_names, xlevels, coefs) {
  # Get rid of existing order number if it exists.  The new order number will
  # be added in this function
  if ('order' %in% names(coefs)) coefs$order <- NULL

  out_vars <- "(Intercept)"

  for (this_var in var_names) {
    if (grepl(":", this_var)) {  # Variable is an interaction
      templist <- NULL
      myVars <- strsplit(this_var, ":")[[1]]
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
      out_vars <- c(out_vars, varList)
    } else if (this_var %in% names(xlevels)) {  # Variable is a factor
      out_vars <- c(out_vars, sprintf('%s%s', this_var, xlevels[[this_var]]))
    } else {
      out_vars <- c(out_vars, this_var)
    }
  }
  name_order <- data.frame(out_vars, order = seq(from = 1, to = length(out_vars), by = 1))

  # I want to make sure that I don't drop any observations, so this section
  # looks for variables that do not appear in nameOrder and adds them to the end
  # of the list if found
  add_names <- coefs$var_name[unique(coefs$var_name) %notin% name_order$out_vars]
  num_new <- length(add_names)
  if (num_new > 0) {
    new_order <- cbind(add_names, max(coefs$est_num) + c(1:num_new))
    colnames(new_order) <- c('out_vars', 'order')
    name_order <- rbind(name_order, new_order)
  }

  with_order <- merge(coefs, name_order, by.x = 'var_name', by.y = 'out_vars')
  with_order[order(with_order$order, with_order$est_num), ]
}
