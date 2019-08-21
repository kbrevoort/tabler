#' Make Column object from single lm result.
#'
#' @param in_result An object containing the results of a statistical estimation
#' @return Results a tabler_column object
#' @examples
#' make_column(in_result)
#' @importFrom broom tidy glance
#' @export
make_column <- function(in_result) {
  UseMethod("make_column")
}

#' @export
make_column.lm <- function(in_result) {

  col_obj <- list()
  attr(col_obj, "class") <- "tabler_column"
  col_obj$dep_var <- as.character(attributes(in_result$terms)$variables[[2]])
  col_obj$var_names <- attributes(in_result$terms)$term.labels
  col_obj$est_type <- class(in_result)[1]
  col_obj$xlevels <- in_result$xlevels
  col_obj$absorbed_vars <- NA_character_

  col_obj$coefs <- broom::tidy(in_result)

  if ( "(Intercept)" %in% col_obj$coefs$term)
    col_obj$var_names <- c("(Intercept)", col_obj$var_names)

  # Add the R-squared to the goodness of fit statistic list
  gof <- broom::glance(in_result)
  gof$N <- gof$df.residual + gof$df
  if (is.element("model", names(in_result)))
    gof$dep_var_mean <- mean(in_result$model[,1])
  col_obj$gof <- gof

  return(col_obj)
}

#' @export
make_column.felm <- function(in_result) {

  col_obj <- list()
  attr(col_obj, 'class') <- 'tabler_column'
  col_obj$dep_var <- in_result$lhs
  var_names <- c(attributes(in_result$terms)$term.labels,
                 names(in_result$fe))
  col_obj$var_names <- var_names
  col_obj$est_type <- class(in_result)[1]
  
  # Expanded xlevels creation -- previous method only included absorbed FEs, not
  # factor variables as well.
  fe_xlevels <- purrr::map(names(in_result$fe), ~ levels(in_result$fe[[.x]]))
  names(fe_xlevels) <- names(in_result$fe)
  fct_xlevels <- build_felm_factor_levels(setdiff(names(coef(in_result)), var_names), var_names)
  col_obj$xlevels <- c(fe_xlevels, fct_xlevels)

  col_obj$absorbed_vars <- names(in_result$fe)

  col_obj$coefs <- broom::tidy(in_result)

  gof <- broom::glance(in_result)
  gof$N <- in_result$N
  gof$dep_var_mean <- mean(in_result$response)
  col_obj$gof <- gof

  col_obj
}

build_felm_factor_levels <- function(unaccounted, var_names) {
  ret_val <- list()
  for (nm in var_names) {
    n <- nchar(nm)
    if (any(substr(unaccounted, 1, n) == nm)) {
      ret_val[[nm]] <- unaccounted[substr(unaccounted, 1, n) == nm] %>%
        substring(n + 1)
    }
  }

  if (length(ret_val) == 0) NULL else ret_val
}
