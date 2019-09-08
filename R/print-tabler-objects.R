#' Print Method for Tabler Object
#'
#' @param in_table Tabler Object
#' @examples
#' print(in_table)
#' @importFrom purrr map_df
#' @importFrom dplyr filter arrange select mutate slice "%>%"
#' @importFrom kableExtra pack_rows add_header_above kable
#' @export
print.tabler_object <- function(in_tabler) {
  tabler2kable(in_tabler)
}

tabler2kable <- function(tblr_obj, format = NULL) {
  # Establish the format
  if (is.null(format)) {
    this_format <- getOption('knitr.table.format')
    if (is.null(this_format))
      this_format <- tblr_obj$theme$style
  } else this_format <- format

  out_dt <- purrr::map_df(strsplit(tblr_obj$theme$order, '')[[1]],
                          get_tblr_component,
                          in_tabler = tblr_obj,
                          this_format) %>%
    process_osa(tblr_obj$osa, tblr_obj$absorbed_vars) %>%
    process_alias(tblr_obj) %>%
    mutate(row_num = row_number())
  my_caption <- if (is.na(tblr_obj$title)) NULL else tblr_obj$title

  if (this_format == 'markdown')
    kableExtra::kable(out_dt,
                 caption = my_caption,
                 format = this_format,
                 escape = FALSE) %>%
    return()

  # The remainder of the function applies to HTML or LaTeX
  header_dt <- filter(out_dt, tblr_type %notin% c('C', 'G'))
  body_dt <- assemble_body_dt(out_dt)

  if (tblr_obj$theme$group_factors) {  # If factors are to be grouped
    for_table_dt <- mutate(body_dt, base = ifelse(tblr_type == 'G', term, base)) %>%
      mutate(term = ifelse(suffix == '', base, suffix)) %>%
      mutate(term = ifelse(tblr_type == 'C' & key == 'sd', '', term)) %>%
      select(-base, -suffix, -tblr_type, -row_num, -key)
    pack_detail <- get_pack_details(body_dt)
  } else {
    for_table_dt <- select(body_dt, -base, -suffix, -tblr_type, -row_num, -key)
    pack_detail <- NULL
  }

  kableExtra::kable(for_table_dt,
               caption = my_caption,
               format = this_format,
               align = c('l', rep('c', dim(for_table_dt)[2] - 1L)),
               booktabs = tblr_obj$theme$booktabs,
               escape = FALSE,
               col.names = NULL) %>%
    kableExtra::row_spec(get_last_coefficient_row(body_dt), hline_after = TRUE) %>%
    do_packing(pack_detail) %>%
    add_header_rows(header_dt) %>%
    clean_errant_codes() %>%
    add_midrule(tblr_obj$theme$booktabs)
}

add_midrule <- function(in_kable, booktabs) {
  #last_header_text <- dplyr::last(attr(in_kable, 'kable_meta')$new_header_row)
  first_var <- paste0('\n', attr(in_kable, 'kable_meta')$rownames[1])

  if (booktabs) {
    paste_string <- '\n\\\\midrule'
  } else paste_string <- '\n\\\\hline
  '
  in_kable[[1]] <- stringr::str_replace(in_kable[[1]],
                                        first_var,
                                        paste0(paste_string, first_var))
  in_kable
}

get_last_coefficient_row <- function(body_dt) {
  filter(body_dt, tblr_type == 'C') %>%
    filter(row_num == max(row_num)) %>%
    pull(row_num)
}

#' Check for Errant Codes
#'
#' Check for errant text codes where multicolumn contains another multicolumn
#' @param in_kable Kable object to process
#' @return Kable object
#' @importFrom stringr str_replace_all
clean_errant_codes <- function(in_kable) {
  in_kable[[1]] <- stringr::str_replace_all(in_kable[[1]],
                                            'multicolumn\\{[0-9]\\}\\{[lrc]\\}\\{(multicolumn\\{[0-9]\\}\\{[lrc]\\}\\{[^\\}]+\\})\\}',
                                            '\\1')
  in_kable
}

add_header_rows <- function(in_kable, data = NULL) {
  if (is.null(data) | is.null(data)) return(in_kable)

  header_dt <- arrange(data, row_num) %>%
    select(-term, -suffix, -tblr_type, -row_num, -key)

  #k <- attr(in_kable, 'kable_meta')$ncol
  k <- dim(header_dt)[2]
  for (i in rev(seq_along(header_dt$base)))
    in_kable <- add_header_above(in_kable,
                                 setNames(rep(1L, times = k),
                                          unname(unlist(slice(header_dt, i)))),
                                 line = FALSE,
                                 bold = FALSE,
                                 escape = FALSE)

  in_kable
}

do_packing <- function(in_kable, data) {
  if (is.null(data)) return(in_kable)

  for (i in seq_along(data$base)) {
    in_kable <- kableExtra::group_rows(in_kable,
                                      group_label = paste0(data$base[[i]], ':'),
                                      start_row = data$start[[i]],
                                      end_row = data$end[[i]],
                                      label_row_css = '',
                                      colnum = 1L,
                                      bold = FALSE)
  }
  in_kable
}

assemble_body_dt <- function(data) {
  filter(data, tblr_type %in% c('C', 'G')) %>%
    mutate(row_num = row_num - min(row_num) + 1) %>% # restart row number at 1
    # Handle logical variables
    mutate(base = ifelse(suffix %in% c("TRUE", "FALSE"),
                         sprintf("%s == %s", base, suffix),
                         base)) %>%
    mutate(suffix = ifelse(suffix %in% c("TRUE", "FALSE"), "", suffix)) %>%
    # Some suppressed variables have a suffix that matches base, which leads
    # to unnecessarily grouping that variable under a factor heading
    mutate(suffix = ifelse(suffix == base, '', suffix))
}

#' @importFrom dplyr filter group_by summarize arrange "%>%"
get_pack_details <- function(in_table) {
  dplyr::filter(in_table, tblr_type == 'C' & suffix != '') %>%
    group_by(base) %>%
    summarize(start = min(row_num),
              end = max(row_num)) %>%
    filter(end > start + 1L) %>% # This will avoid single-variable factors
    arrange(start)
}

#' @importFrom dplyr union filter mutate pull bind_rows select starts_with summarize_all "%>%"
#' @importFrom purrr map_df
process_osa <- function(tbl_dt, osa_obj, abs_var) {
  # Remove rows corresponding to the omit list
  if (any(!is.na(osa_obj$omit))) {
    tbl_dt <- filter(tbl_dt,
                     tblr_type != 'C' |
                       (base %notin% osa_obj$omit & term %notin% osa_obj$omit))
  }

  # If there are any absorbed variables, suppress existing coefficients (if
  # necessary) or add a table
  absorbed_dt <- build_absorb_dt(abs_var)
  if (!is.null(absorbed_dt)) {
    rows_to_add <- filter(tbl_dt,
           base %in% absorbed_dt$term,
           key == 'beta') %>%
      bind_rows(absorbed_dt) %>%
      select(base, term, suffix, tblr_type, starts_with('c_')) %>%
      group_by(base, term, suffix, tblr_type) %>%
      summarize_all(~ if(any(.x != '')) 'Y' else '') %>%
      mutate(key = 'beta')

    # Replace any reference to one of the absorbed variables with 'Y' if that
    # variable was used in that estimation
    tbl_dt <- filter(tbl_dt, !(base %in% absorbed_dt$term)) %>%
      add_suppressed_row(rows_to_add, .)

    # If any of the absorbed variables appear in the suppression list, remove it
    if (any(!is.na(osa_obj$suppress))) {
      osa_obj$suppress <- setdiff(osa_obj$suppress, unique(absorbed_dt$term))
      if (length(osa_obj$suppress) == 0)
        osa_obj$suppress <- NA_character_
    }
  }

  # Process the suppress list (will include absorbed variables)
  if (any(!is.na(osa_obj$suppress))) {
    # Produce rows that will replace suppressed variables
    replacement_dt <- purrr::map_df(osa_obj$suppress, suppress_compress, dt = tbl_dt)

    # Remove suppressed rows
    tbl_dt <- filter(tbl_dt, tblr_type != 'C' | base %notin% osa_obj$suppress) %>%
      mutate(row_num = row_number()) %>%
      add_suppressed_row(row_dt = replacement_dt, .)
  }

  tbl_dt
}

#' Process Alias
#'
#'
process_alias <- function(tbl_dt, tbl_obj) {
  tbl_dt <- mutate(tbl_dt, row_num = row_number())

  if (any(!is.na(tbl_obj$osa$alias))) {
    coef_dt <- filter(tbl_dt, tblr_type == 'C') %>%
      mutate(row_num = row_number())

    alias_dt <- tibble::tibble(var = names(tbl_obj$osa$alias),
                               alias = tbl_obj$osa$alias)

    base_dt <- expand_interaction_to_dt(coef_dt$base) %>%
      left_join(alias_dt, by = 'var') %>%
      mutate(alias = ifelse(is.na(alias), var, alias)) %>%
      select(row_num, alias) %>%
      compress_interaction_dt() %>%
      rename(base_alias = alias)

    term_dt <- expand_interaction_to_dt(coef_dt$term) %>%
      left_join(alias_dt, by = 'var') %>%
      mutate(alias = ifelse(is.na(alias), '', alias)) %>%
      select(row_num, alias) %>%
      compress_interaction_dt() %>%
      rename(term_alias = alias)

    new_dt <- left_join(coef_dt, base_dt, by = 'row_num') %>%
      left_join(term_dt, by = 'row_num') %>%
      mutate(base = base_alias) %>%
      mutate(suffix = suffix_to_alias(suffix, term_alias)) %>%
      select(-row_num, -base_alias, -term_alias) %>%
      list_first('base', 'term', 'suffix', 'tblr_type')

    spot_dt <- filter(tbl_dt, tblr_type == 'C') %>%
      summarize(lowest = min(row_num),
                highest = max(row_num))

    tbl_dt <- bind_rows(filter(tbl_dt, row_num < spot_dt$lowest),
              new_dt,
              filter(tbl_dt, row_num > spot_dt$highest))
  }

  select(tbl_dt, -row_num)
}

#' @importFrom dplyr bind_cols mutate select pull
suffix_to_alias <- function(suf, a) {
  bind_cols(expand_interaction_to_dt(suf, ' \u2613 '),
            expand_interaction_to_dt(a, ' \u2613 ')) %>%
    mutate(alias = ifelse(var1 == '', var, var1)) %>%
    select(row_num, alias) %>%
    compress_interaction_dt() %>%
    pull(alias)
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @importFrom stringr str_split str_replace_all
expand_interaction_to_dt <- function(x, sep = ':') {
  if (sep == ':') {
    x <- stringr::str_replace_all(x,
                                  '([[:alnum:]]):([[:alnum:]])',
                                  '\\1%#%#%#%\\2')
    sep <- '%#%#%#%'
  }

  stringr::str_split(x, sep) %>%
    purrr::map_dfr(~ tibble::tibble(var = .x), .id = 'row_num') %>%
    mutate(row_num = as.integer(row_num))
}

compress_interaction_dt <- function(data) {
  unique_i <- unique(data$row_num)

  compress_dt <- function(i, dt) {
    filter(dt, row_num == i) %>%
      pull(alias) %>%
      paste(collapse = ' \u2613 ') %>%
      tibble::tibble(row_num = i, alias = .)
  }

  purrr::map_dfr(unique_i, compress_dt, dt = data)
}

#' Add Suppressed Row
#'
#' Adds a new row of suppressed data to an existing table data.frame.
#' @param row_dt A tibble containing one row of data to be added
#' @param tbl_dt A tibble containing table information
add_suppressed_row <- function(row_dt, tbl_dt) {
  tbl_dt <- mutate(tbl_dt, row_num = row_number())

  # Calculate hte row where the absorbed variabe is to be added
  add_place <- tbl_dt %>%
    filter(tblr_type == 'C') %>%
    pull(row_num) %>%
    max()

  bind_rows(filter(tbl_dt, row_num <= add_place),
            row_dt,
            filter(tbl_dt, row_num > add_place)) %>%
    select(-row_num)
}

#' Suppress Compress
#'
#' Takes rows from the coefficient table that are included in the suppression list
#' and generates a single replacement row for each variable that reports whether
#' that factor variable was included in the estimation reported in each column.
#' @param suppress_var Character scalar giving the name of a factor variable to be suppressed
#' @param dt The tibble prepared to be printed
#' @importFrom dplyr filter mutate "%>%"
#' @importFrom purrr map_df
suppress_compress <- function(suppress_var, dt) {
  filter(dt, tblr_type == 'C' & base == suppress_var) %>%
    purrr::map_df(~ if (all(.x == '')) '' else 'Y') %>%
    mutate(base = suppress_var,
           term = suppress_var,
           suffix = '',
           tblr_type = 'C',
           key = 'beta')
}

get_tblr_component <- function(x, in_tabler, in_format) {
  if (x == 'C') {
    ret_val <- output_coef_table(in_tabler)
  } else if (x == 'G') {
    ret_val <- output_gofs_table(in_tabler)
  } else if (x == 'N') {
    ret_val <- output_colnum_table(in_tabler)
  } else if (x == 'D') {
    ret_val <- output_depvar_table(in_tabler, in_format)
  } else if (x == 'M') {
    ret_val <- output_method_table(in_tabler, in_format)
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
coef_to_dt <- function(coef_dt, sig_levels, digits) {
  ret_val <- mutate(coef_dt, beta = sprintf('%s%s',
                                 num(coef_dt$estimate, digits = digits[1L]),
                                 cut(coef_dt$p.value,
                                     breaks = c(-1, sig_levels, 1),
                                     labels = c(names(sig_levels), '')))) %>%
    mutate(sd = sprintf('(%s)', prettyNum(coef_dt$std.error,
                                          big.mark = ',',
                                          digits = digits[2]))) %>%
    select(est_num, term, order, beta, sd) %>%
    gather(key, value, beta, sd) %>%
    arrange(order, est_num, key) %>%
    mutate(est_num = sprintf('c_%i', as.integer(est_num))) %>%
    spread(est_num, value, fill = '') %>%
    arrange(order, key)


}

#' @importFrom purrr map_df
order_coefs <- function(var_names, xlevels) {
  purrr::map_df(var_names, build_var_names, xlevels = xlevels)
}

#' @importFrom purrr map pmap
#' @importFrom tibble tibble
#' @importFrom stringr str_split
build_var_names <- function(var_name, xlevels) {
  if (grepl('[[:alnum:]]:[[:alnum:]]', var_name)) {
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

#' @importFrom dplyr right_join mutate select
output_coef_table <- function(tblr_obj) {
  coefs <- coef_to_dt(tblr_obj$coef,
                      tblr_obj$theme$sig_level,
                      tblr_obj$theme$digits)
  var_names <- order_coefs(tblr_obj$var_names, tblr_obj$xlevels)

  right_join(var_names, coefs, by = 'term') %>%
    mutate(term = ifelse(base == term, '', term)) %>%
    mutate(tblr_type = 'C') %>%
    select(-order) %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

#' @importFrom dplyr mutate rename
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr gather spread
output_gofs_table <- function(tblr_obj) {
  order_dt <- tibble::tibble(key = names(tblr_obj$gof_list),
                             long_name = unname(tblr_obj$gof_list)) %>%
    tibble::rowid_to_column(var = 'order')

  ret_val <- tblr_obj$gofs %>%
    tibble::rowid_to_column(var = 'column') %>%
    mutate(column = sprintf('c_%i', column)) %>%
    tidyr::gather(key = 'key', value = 'value', -column) %>%
    right_join(order_dt, by = 'key') %>%
    number2text(digits = tblr_obj$theme$digits) %>%
    tidyr::spread(key = 'column', value = 'value') %>%
    rename(term = long_name) %>%
    mutate(base = term, suffix = '') %>%
    mutate(tblr_type = 'G') %>%
    arrange(order) %>%
    select(-key, -order)

  list_first(ret_val, 'base', 'term', 'suffix', 'tblr_type')
}

number2text <- function(data, digits) {
  number_type <- group_by(data, key) %>%
    summarize(max_num = max(value)) %>%
    mutate(log_num = log10(abs(max_num))) %>%
    select(key, log_num)

  left_join(data, number_type, by = 'key') %>%
    mutate(value = dplyr::case_when(
      log_num < 0 ~ as.character(round(value, digits[1L])),
      log_num >= 3 ~ prettyNum(round(value, 0), big.mark = ',', ),
      TRUE ~ prettyNum(value, digits = digits[1L])
    )) %>%
    select(-log_num)
}

#' Alias Column Names
#'
#' Takes a character vector of column names (such as dependent variables or
#' statistical methods) and determines whether an alias has been supplied.  If
#' so, this function replaces that element
#' @param x Character vector
#' @param alias_list The named list of aliase contained by a tabler object
#' @return A character vector of the same length of x, where elements have been
#' replaced by their alias if an alias exists
alias_column_names <- function(x, alias_list) {
  if (any(!is.na(alias_list))) {
    x <- tibble::tibble(y = x,
                        alias = unname(alias_list[x])) %>%
      mutate(new_y = ifelse(is.na(alias), y, alias)) %>%
      pull(new_y)
  }

  x
}

#' @importFrom kableExtra cell_spec text_spec
output_depvar_table <- function(tblr_obj, in_format) {
  my_dep_vars <- alias_column_names(tblr_obj$dep_vars, tblr_obj$osa$alias)

  start_df(my_dep_vars) %>%
    mutate(base = text_spec('Dep. Variable:', align = 'r', escape = FALSE, format = in_format),
           term = '',
           suffix = '',
           tblr_type = 'D') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

#' @importFrom kableExtra cell_spec
output_method_table <- function(tblr_obj, in_format) {
  my_est_types <- alias_column_names(tblr_obj$est_types, tblr_obj$osa$alias)

  start_df(my_est_types) %>%
    mutate(base = text_spec('Method', align = 'r', escape = FALSE, format = in_format),
           term = '',
           suffix = '',
           tblr_type = 'M') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

#' @importFrom purrr map_dfc
#' @importFrom dplyr mutate
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

  mutate(ret_val, base = ' ', term = ' ', suffix = ' ', tblr_type = 'N') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}

#' @importFrom purrr map_dfc
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
#' @importFrom dplyr union select
list_first <- function(dt, ...) {
  list(...) %>%
    unlist() %>%
    union(names(dt)) %>%
    select(dt, .)
}

#' Build Absorb Data
#'
#' Create a data.frame that contains information on the absorbed values used in
#' each estimation.
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom dplyr mutate right_join "%>%"
#' @importFrom tidyr spread
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
             expand.grid(term = unique(temp$term),
                         value = sprintf('c_%i', seq_along(absorb_list)),
                         stringsAsFactors = FALSE),
             by = c('term', 'value')) %>%
    mutate(beta = ifelse(is.na(beta), '', beta)) %>%
    filter(!is.na(term)) %>%
    spread(value, beta, fill = '') %>%
    mutate(base = term,
           suffix = '',
           tblr_type = 'C',
           key = 'beta') %>%
    list_first('base', 'term', 'suffix', 'tblr_type')
}
