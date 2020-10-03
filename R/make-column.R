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

  col_obj <- check_for_logicals(col_obj)
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
  est_var_names <- names(coef(in_result))
  unaccounted_vars <- setdiff(est_var_names, c(var_names, '(Intercept)'))  # Coefficient names that are not variables
  left_to_match <- setdiff(var_names, est_var_names)
  fct_xlevels <- build_felm_factor_levels(unaccounted_vars, left_to_match)
  col_obj$xlevels <- c(fe_xlevels, fct_xlevels)

  col_obj$absorbed_vars <- names(in_result$fe)

  col_obj$coefs <- broom::tidy(in_result)

  gof <- broom::glance(in_result)
  gof$N <- in_result$N
  gof$dep_var_mean <- mean(in_result$response)
  col_obj$gof <- gof

  #col_obj <- check_for_logicals(col_obj)
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

#' Check for Logicals
#'
#' Explores the tabler_column object information to determine if there are any
#' coefficients that are logical values that have not been appropriately
#' handled. Any that are found are added to the list of factor variables.
#' @param col_obj A tabler_column object
#' @return The same tabler_column with new xlevels added to pick up any logical
#' variables thare found.
check_for_logicals <- function(col_obj) {
  # all possible known variables
  complete_name_list <- order_coefs(col_obj$var_names, xlevels = col_obj$xlevels) %>%
    pull(term)

  # variable names that appears in the coefficient list
  coefs_name <- col_obj$coef$term

  # Currnetly unknown variables
  missing_names <- setdiff(coefs_name, complete_name_list)

  if (length(missing_names) > 0) {
    new_factors <- purrr::map_dfr(missing_names,
                                  identify_logical_variables,
                                  var_names = col_obj$var_names)

    for (nm in unique(new_factors$var_name)) {
      nm_levels <- filter(new_factors, var_name == nm) %>%
        pull(xlevel)

      # Update the xlevels for the col_obj
      if (nm %in% names(col_obj$xlevels)) {
        col_obj$xlevels[[nm]] <- unique(c(col_obj$xlevels[[nm]], nm_levels))
      } else {
        col_obj$xlevels[[nm]] <- nm_levels
      }
    }
  }

  col_obj
}


#' Identify Logical Variables
#'
#' Examine unaccounted for variables from the coefficient matrix and determine
#' which are logical.  This returns 1 element of a data.frame of data that
#' gives the variable name and logical result of those variables. Otherwise,
#' throws a warning if the unaccounted for name is not logical.
#' @param x A character scalar with an unaccounted for variable name
#' @param var_names List of known variables in the estimation
#' @return A tibble with the name of the variable (if it is logical) and its
#' logical value.
#' @importFrom tibble tibble
identify_logical_variables <- function(x, var_names) {
  n <- nchar(x)
  if (substr(x, n - 3L, n) == "TRUE") {
    ret_val <- tibble::tibble(var_name = substr(x, 1L, n - 4L),
                              xlevel = 'TRUE')
  } else if (substr(x, n - 4L, n) == "FALSE") {
    ret_val <- tibble::tibble(var_name = substr(x, 1L, n - 5L),
                              xlevel = 'FALSE')
  } else {
    sprintf('Unaccounted for variable found:  %s', x) %>%
      warning()
    return(NULL)
  }

  if (!pull(ret_val, var_name) %in% var_names)
    sprintf('Found logical variable that does not appear in variable list:  %s',
            pull(ret_val, var_name)) %>%
    warning()

  ret_val
}
