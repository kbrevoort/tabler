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
#' @param showDepVar = TRUE
#' showMethod = TRUE
#' digits = c(3,2)
#' sigLevels = c('***' = 0.01, '**' = 0.05, '*' = 0.1)
#' @return tablerTheme object
#' @export
tabler_theme <- function(showDepVar = TRUE,
                       showMethod = TRUE,
                       digits = c(3,2),
                       sigLevels = c('***'=0.01, '**' = 0.05, '*' = 0.1),
                       style = 'markdown',
                       order = "=NMD-C-G=") {

  themeElements <- list()
  attr(themeElements, "class") <- "tablerTheme"
  themeElements$showDepVar <- showDepVar
  themeElements$showMethod <- showMethod
  themeElements$style <- style

  themeElements$order <- order

  themeElements$digits <- digits
  if (length(digits) == 1) themeElements$digits <- c(digits, digits)

  themeElements$sigLevels <- sort(sigLevels)
  return(themeElements)
}
