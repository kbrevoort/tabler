#' Convert Coef Tibble to Output
#'
#' Takes a coefficient data.frame from a tabler_object and modifies
#' the format to create a new data.frame that is formatted in the style used to
#' display coefficient results in most economics journals.
#' @param coef_dt A data.frame of coefficient results from a tabler_object
#' @param sig_levels A named character vector that gives the thresholds to use
#' for indicating statistical significance. The names of the vector should provide
#' the characters or symbols to use in denoting that significance level. This
#' can be found in tabler_object$theme$sig_level
#' @importFrom dplyr mutate select arrange
#' @importFrom tidyr gather spread
coef_to_dt <- function(coef_dt, sig_levels) {
  mutate(coef_dt, beta = sprintf('%s%s',
                                 prettyNum(coef_dt$estimate, big.mark = ',', digits = 3L),
                                 cut(coef_dt$p.value,
                                     breaks = c(-1, sig_levels, 1),
                                     labels = c(names(sig_levels), '')))) %>%
    mutate(sd = sprintf('(%s)', prettyNum(coef_dt$std.error,
                                          big.mark = ',',
                                          digits = 2L))) %>%
    select(est_num, term, order, beta, sd) %>%
    gather(key, value, beta, sd) %>%
    arrange(order, est_num, key) %>%
    spread(est_num, value, fill = '') %>%
    arrange(order, key)
}

order_coefs <- function(var_names, xlevels) {
  purrr::map_df(var_names, build_var_names, xlevels = xlevels)
}

build_var_names <- function(var_name, xlevels) {
  if (grepl(':', var_name)) {
    interacted_vars <- str_split(var_name, ':') %>%
      unlist()

    term = interacted_vars %>%
      purrr::map(~ if (.x %in% names(xlevels)) paste0(.x, xlevels[[.x]]) else .x) %>%
      purrr::pmap(paste, sep = ':') %>%
      unlist()

    var_vec <- interacted_vars %>%
      purrr::map(~ if (.x %in% names(xlevels)) xlevels[[.x]] else '') %>%
      expand.grid(stringsAsFactors = FALSE)

    names(var_vec) <- interacted_vars
    var_vec <- var_vec %>%
      purrr::pmap(name_interaction) %>%
      unlist()
  } else if (var_name %in% names(xlevels)) {
    var_vec <- xlevels[[var_name]]
    term <- paste0(var_name, xlevels[[var_name]])
  } else {
    var_vec <- ''
    term <- var_name
  }

  tibble::tibble(base = var_name,
                 term = term,
                 suffix = var_vec)
}

name_interaction <- function(...) {
  var_names <- list(...)

  just_vals <- unname(var_names) %>%
    unlist() %>%
    as.character()

  if (all(just_vals == '')) {
    return('')
  } else if (any(just_vals == '')) {
    just_vals[just_vals == ''] <- names(var_names[just_vals == ''])
  }

  paste(just_vals, collapse = " \u2613 ")
}

output_coef_table <- function(tblr_obj) {
  coefs <- coef_to_dt(tblr_obj$coef, tblr_obj$theme$sig_level)
  var_names <- order_coefs(tblr_obj$var_names, tblr_obj$xlevels)

  right_join(var_names, coefs, by = 'term') %>%
    mutate(term = ifelse(base == term, '', term)) %>%
    select(-order)
}

output_gofs_table <- function(tblr_obj) {
  ret_val <- tblr_obj$gofs %>%
    tibble::rowid_to_column(var = 'column') %>%
    tidyr::gather(key = 'key', value = 'value', -column) %>%
    tidyr::spread(key = 'column', value = 'value') %>%
    rename(term = key) %>%
    mutate(base = '', suffix = '')

  c('base', 'term', 'suffix') %>%
    union(names(ret_val)) %>%
    select(ret_val, .)
}

