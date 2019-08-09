#' Print Method for Tabler Object
#'
#' @param in_table Tabler Object
#' @examples
#' print(in_table)
#' @export
print.tabler_object <- function(in_tabler) {

  my_order <- in_tabler$theme$order %>%
    strsplit('') %>%
    unlist()

  this_format <- getOption('knitr.table.format')
  if (is.null(this_format))
    this_format <- in_tabler$theme$style

  purrr::map_df(my_order, get_tblr_component, in_tabler = in_tabler) %>%
    process_osa(in_tabler$osa, in_tabler$absorbed_vars) %>%
    knitr::kable(caption = in_tabler$title, format = this_format)
}

process_osa <- function(tbl_dt, osa_obj, abs_var) {
  if (!is.na(osa_obj$omit)) {
    tbl_dt <- filter(tbl_dt,
                      tblr_type != 'C' |
                        (base %notin% osa_obj$omit & term %notin% osa_obj$omit))
  }

  absorbed_dt <- build_absorb_dt(abs_var)
  if (!is.null(absorbed_dt)) {
    # Add absorbed data to coefficients
    tbl_dt <- mutate(tbl_dt, row_num = row_number())
    add_place <- tbl_dt %>%
      filter(tblr_type == 'C') %>%
      pull(row_num) %>%
      max()
    tbl_dt <- bind_rows(filter(tbl_dt, row_num <= add_place),
                         absorbed_dt,
                         filter(tbl_dt, row_num > add_place)) %>%
      select(-row_num)

    if (is.na(osa_obj$suppress)) {
      osa_obj$suppress <- absorbed_dt$term
    } else {
      osa_obj$suppress <- union(osa_obj$suppress, absorbed_dt$term)
    }
  }

  if (!is.na(osa_obj$suppress)) {
    # Produce rows that will replace suppressed variables
    replacement_dt <- purrr::map_df(osa_obj$suppress, suppress_compress, dt = tbl_dt)

    # IF there are absorbed variables add them here
    absorbed_dt <- build_absorb_dt(abs_var)
    if (!is.null(absorbed_dt))
      replacement_dt <- dplyr::bind_rows(replacement_dt, absorbed_dt)

    # Remove suppressed rows
    tbl_dt <- filter(tbl_dt, tblr_type != 'C' | base %notin% osa_obj$suppress) %>%
      mutate(row_num = row_number())

    add_place <- tbl_dt %>%
      filter(tblr_type == 'C') %>%
      pull(row_num) %>%
      max()
    tblr_dt <- bind_rows(filter(tbl_dt, row_num <= add_place),
                         replacement_dt,
                         filter(tbl_dt, row_num > add_place)) %>%
      select(-row_num)
  }

  if (!is.na(osa_obj$alias)) {
    for (i in seq_along(osa_obj$alias)) {
      this_name <- names(osa_obj$alias)[i]
      this_alias <- unname(osa_obj$alias)[i]

      if (any(tbl_dt$base == this_name)) {
        tbl_dt$base[tbl_dt$base == this_name] <- this_alias
      }
      if (any(tbl_dt$term == this_name)) {
        tbl_dt$suffix[tbl_dt$term == this_name] <- this_alias
      }
    }
  }

  tbl_dt
}

#' Suppress Compress
#'
#' Takes rows from the coefficient table that are included in the suppression list
#' and generates a single replacement row for each variable that reports whether
#' that factor variable was included in the estimation reported in each column.
#' @param suppress_var Character scalar giving the name of a factor variable to be suppressed
#' @param dt The tibble prepared to be printed
suppress_compress <- function(suppress_var, dt) {
  filter(dt, tblr_type == 'C' & base == suppress_var) %>%
    purrr::map_df(~ if (all(.x == '')) '' else 'Y') %>%
    mutate(base = suppress_var,
           term = suppress_var,
           suffix = '',
           tblr_type = 'C',
           key = 'beta')
}

get_tblr_component <- function(x, in_tabler) {
  if (x == 'C') {
    ret_val <- output_coef_table(in_tabler)
  } else if (x == 'G') {
    ret_val <- output_gofs_table(in_tabler)
  } else if (x == 'N') {
    ret_val <- output_colnum_table(in_tabler)
  } else if (x == 'D') {
    ret_val <- output_depvar_table(in_tabler)
  } else if (x == 'M') {
    ret_val <- output_method_table(in_tabler)
  } else ret_val <- NULL

  ret_val
}

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
  ret_val <- mutate(coef_dt, beta = sprintf('%s%s',
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
    mutate(est_num = sprintf('c_%i', as.integer(est_num))) %>%
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
    mutate(tblr_type = 'C') %>%
    select(-order) %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

output_gofs_table <- function(tblr_obj) {
  ret_val <- tblr_obj$gofs %>%
    tibble::rowid_to_column(var = 'column') %>%
    mutate(column = sprintf('c_%i', column)) %>%
    tidyr::gather(key = 'key', value = 'value', -column) %>%
    mutate(value = as.character(value)) %>%
    tidyr::spread(key = 'column', value = 'value') %>%
    rename(term = key) %>%
    mutate(base = '', suffix = '') %>%
    mutate(tblr_type = 'G')

  list_first(ret_val, 'base', 'term', 'suffix', 'tblr_type')
}

output_depvar_table <- function(tblr_obj) {
  start_df(tblr_obj$dep_vars) %>%
    mutate(base = 'Dep. Variable:',
           term = '',
           suffix = '',
           tblr_type = 'D') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

output_method_table <- function(tblr_obj) {
  start_df(tblr_obj$est_types) %>%
    mutate(base = 'Method:',
           term = '',
           suffix = '',
           tblr_type = 'M') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

output_colnum_table <- function(tblr_obj) {
  num_vec <- c(1:length(tblr_obj$dep_vars))

  if (tblr_obj$theme$col_number_style == 'parenthetic') {
    str_vec <- sprintf('(%s)', num_vec)
  } else if (tblr_obj$theme$col_number_style == 'roman') {
    str_vec <- as.roman(num_vec)
  } else {
    str_vec <- as.character(num_vec)
  }

  ret_val <- purrr::map_dfc(num_vec, ~ str_vec[[.x]])
  names(ret_val) <- sprintf('c_%i', num_vec)

  mutate(ret_val, base = '', term = '', suffix = '', tblr_type = 'N') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

start_df <- function(x) {
  ret_val <- purrr::map_dfc(c(1:length(x)), ~ x[[.x]])
  names(ret_val) <- sprintf('c_%i', c(1:length(x)))
  ret_val
}

#' List First
#'
#' Reorders a data.frame with the specified variables appearing first in order.
#' The order of unspecified varaibles is maintained after all of the specified
#' variables have been listed.
#' @param dt A data.frame
#' @param ... A series of character variables giving the names of the variables
#' to be listed first.
list_first <- function(dt, ...) {
  list(...) %>%
    unlist() %>%
    union(names(dt)) %>%
    select(dt, .)
}

build_absorb_dt <- function(absorb_list) {
  f <- function(i, al) {
    if (is.null(al[[i]])) {
      return(NULL)
    } else {
      tibble::tibble(term = al[[i]], value = sprintf('c_%i', i))
    }
  }

  temp <- purrr::map_df(seq_along(absorb_list), f, al = absorb_list) %>%
    mutate(beta = 'Y')

  right_join(temp,
             expand.grid(term = temp$term,
                         value = sprintf('c_%i', seq_along(absorb_list)),
                         stringsAsFactors = FALSE),
             by = c('term', 'value')) %>%
    mutate(beta = ifelse(is.na(beta), '', beta)) %>%
    spread(value, beta, fill = '') %>%
    mutate(base = '',
           suffix = '',
           tblr_type = 'C') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}
