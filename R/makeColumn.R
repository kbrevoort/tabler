# This R script will contain functions that create column objects that record the
# information which will be ouput into the table.  These functions will return
# S3 objects.
#
# Each object will be a list with the following elements
#
# class:  "tabler_column"
# dep_vars:  "character"
# var_names:  "character" vector
# est_types: "character"
# xlevels: "character"
# coefs:  "matrix"
# gofs: "data.frame"
NULL

#' Make Column object from single lm result.
#'
#' @param inResult An object containing the results of a statistical estimation
#' @return Results a colRec object
#' @examples
#' make_column(inResult)
#' @export
make_column <- function(in_result) {
  UseMethod("make_column")
}


make_column.lm <- function(in_result) {

  in_summary <- summary(in_result)

  col_obj <- list()
  attr(col_obj, "class") <- "tabler_column"
  col_obj$dep_var <- as.character(attributes(in_result$terms)$variables[[2]])
  col_obj$var_names <- attributes(in_result$terms)$term.labels
  col_obj$est_type <- class(in_result)[1]
  col_obj$xlevels <- in_result$xlevels

  col_obj$coefs <- data.frame(in_summary$coefficient)
  names(col_obj$coefs) <- c('est','std','tval','pval')
  col_obj$coefs$var_name <- rownames(col_obj$coefs)
  rownames(col_obj$coefs) <- NULL

  if ( "(Intercept)" %in% rownames(col_obj$coefs) ) col_obj$var_names <- c("(Intercept)",col_obj$var_names)

  # Add the R-squared to the goodness of fit statistic list
  gof <- data.frame(r2=in_summary$r.squared, nobs=nobs(in_result))
  names(gof) <- c("R-Squared", "Observations")

  # This will add the man of the dependent variable
  if (is.element("model", names(in_result))) gof[["Dep. Var. Mean"]] <- mean(in_result$model[,1])

  col_obj$gof <- gof

  return(col_obj)
}
