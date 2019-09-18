
#' Print a Summary Table.
#'
#' Prints a sum_tabler object.
#' @export
print.sum_tabler <- function(in_tabler) {
  sumtabler2kable(in_tabler)
}

#' Sum Tabler To Kable
#'
#' Take a `sum_tabler` object and convert it into a kable.
#' @param stbl_obj A sum_tabler object
#' @param format Character vector providing the format of the table to be
#' generated (default = NULL)
#' @return A kable object, modified by kableExtra
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr map_dfc
#' @importFrom kableExtra kable
#' @export
sumtabler2kable <- function(stbl_obj, format = NULL) {
  my_dt <- stbl_obj$var_dt %>%
    mutate(term = ifelse(suffix == '', base, paste0(base, suffix))) %>%
    mutate(tblr_type = 'C') %>%
    process_omit(stbl_obj$osa$omit) %>%
    process_alias(stbl_obj) %>%
    mutate(row_num = row_number()) %>%
    list_first('base', 'suffix', 'term', 'tblr_type', 'row_num')

  names(my_dt) <- purrr::map_chr(names(my_dt),
                                 process_header_alias,
                                 alias_list = stbl_obj$osa$alias)

  for_table_dt <- process_group_variables(my_dt, stbl_obj) %>%
    purrr::map_dfc(~ if (is.numeric(.x)) num(.x, digits = stbl_obj$theme$digits[1L]) else .x)

  pack_detail <- get_pack_details(my_dt, stbl_obj)

  kableExtra::kable(for_table_dt,
                    caption = stbl_obj$title,
                    align = c('l', rep('c', dim(for_table_dt)[2] - 1L)),
                    booktabs = stbl_obj$theme$booktabs,
                    escape = TRUE,
                    format = format) %>%
    do_packing(pack_detail)
}

#' Process Header Alias
#'
#' Process the header of a summary table to see if any of the columns have a
#' specified alias.
#'
#' This is an internal function
#' @param nm Character scalar with name of column to process
#' @param alias_list Named character vector where name is the column header
#' (potentially) and the value is the alias.
#' @return A character scalar
process_header_alias <- function(nm, alias_list) {
  if (nm %in% c('base', 'suffix', 'term', 'tblr_type', 'row_num')) {
    nm
  } else if (nm %in% names(alias_list)) {
    alias_list[[nm]]
  } else nm
}

process_alias <- function(dt, tblr_obj) {
  if (all(is.na(tblr_obj$osa$alias)))
    return(dt)

  coef_dt <- filter(dt, tblr_type == 'C') %>%
    mutate(row_num = row_number())

  alias_dt <- tibble::tibble(var = names(tblr_obj$osa$alias),
                             alias = tblr_obj$osa$alias)

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
}

tabulate_coef_dt <- function(tblr_obj) {
  num_cols <- length(tblr_obj$dep_vars)
  num_vars <- length(tblr_obj$var_names) # Thiss count includes factors (but not each level)

  # THe number of factor variables (2 factors with 4 levels each would thus yield
  # 8 as num_factors)
  num_factors <- purrr::map_int(tblr_obj$xlevels, length) %>%
    sum()

}


tabulate_coef <- function(coefs, theme) {
  num_cols <- max(coefs$est_num)
  num_vars <- max(coefs$order)

  out_vec <- vector('list', num_vars)  # pre-allocate list

  for (i in 1:num_vars) {
    if (i %in% coefs$order) {
      this_row <- dplyr::filter(coefs, order == i)
      line1 <- rep("", num_cols)
      line2 <- rep("", num_cols)

      line1[this_row$est_num] <- prettyNum(this_row$estimate,
                                           digits = theme$digits[1])
      line2[this_row$est_num] <- prettyNum(this_row$std.error,
                                           digits = theme$digits[2])

      out_vec[[i]] <- c(this_row$term[1], line1, line2)
    }
  }
  out_vec
}

tabulate_GOF <- function(gofs, theme) {
  outDF <- data.frame(t(gofs))

  # Using the square brackets preserves the output as a data.frame
  outDF[] <- lapply(outDF,
                    function(x) prettyNum(x, digits = theme$digits[1], big.mark = ','))
  outDF
}

