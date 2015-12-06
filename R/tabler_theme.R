#' Set tabler theme elements
#'
#' This function allows the user to specify the details of how tables are displayed.  The
#' tabler theme is designed to operate in the same way as themes in ggplot2.  The theme function
#' can be customized by the user once and then easily applied to all subsequent tables.  This
#' allows a consistent presentation among tables that does not require passing the same options
#' into each tabler function.
#'
#' @description Use this function to modify theme settings
#' @usage tabler_theme(...)
#' @param show_dep_var = TRUE
#' @param show_method = TRUE
#' @param digits = c(3,2)
#' @param sig_levels = c('***' = 0.01, '**' = 0.05, '*' = 0.1)
#' @return tablerTheme object
#' @export
tabler_theme <- function(show_dep_var = TRUE,
                         show_method = TRUE,
                         digits = c(3,2),
                         sig_levels = c('***'=0.01, '**' = 0.05, '*' = 0.1),
                         style = 'markdown',
                         col_number_style = "parenthetic",  # Can also be 'roman'
                         order = "=NMD-C-G=",
                         suppress = NULL) {

  # Validate data
  if (length(digits) != 2) stop('Invalid digits parameter set in tabler_theme')

  elements <- list()
  attr(elements, "class") <- "tabler_theme"
  elements$show_dep_var <- show_dep_var
  elements$show_method <- show_method
  elements$style <- style
  elements$col_number_style <- col_number_style
  elements$suppression_list <- suppress

  elements$order <- toupper(order)

  elements$digits <- digits
  if (length(digits) == 1) elements$digits <- c(digits, digits)

  elements$sig_levels <- sort(sig_levels)
  elements
}
