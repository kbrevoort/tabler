export_kable_html <- function(tblr_obj) {
  out_dt <- reformat_tabler_obj(tblr_obj)
  body_dt <- assemble_body_dt(tblr_obj)
  for_table_dt <- process_group_variables(body_dt, tblr_obj)

  pack_details <- get_pack_details(assemble_body_dt(tblr_obj),
                                   tblr_obj)

  kableExtra::kable(for_table_dt,
                    caption = extract_caption(tblr_obj),
                    format = 'html',
                    align = c('l', rep('c', dim(for_table_dt)[2] - 1L)),
                    booktabs = FALSE,
                    escape = TRUE,
                    col.names = names(for_table_dt)) %>%
    add_lines_to_html_table(tblr_obj) %>%
    do_packing(pack_details)

}

export_kable_latex <- function(tblr_obj) {

  out_dt <- reformat_tabler_obj(tblr_obj)

  # The remainder of the function applies to HTML or LaTeX
  header_dt <- filter(out_dt, tblr_type %notin% c('C', 'G'))
  body_dt <- assemble_body_dt(tblr_obj)

  for_table_dt <- process_group_variables(body_dt, tblr_obj)

  pack_details <- get_pack_details(assemble_body_dt(tblr_obj),
                                   tblr_obj)

  kableExtra::kable(for_table_dt,
                    caption = extract_caption(tblr_obj),
                    format = 'latex',
                    align = c('l', rep('c', dim(for_table_dt)[2] - 1L)),
                    booktabs = tblr_obj$theme$booktabs,
                    linesep = if (tblr_obj$theme$booktabs) '' else '\\hline',
                    escape = TRUE,
                    col.names = NULL) %>%
    kableExtra::row_spec(get_last_coefficient_row(body_dt),
                         hline_after = TRUE,
                         extra_latex_after = '\\addlinespace[0.5em]') %>%
    add_midrule(tblr_obj$theme$booktabs) %>%
    do_packing(pack_details) %>%
    add_header_rows(header_dt) %>%
    clean_errant_codes()

}

reformat_tabler_obj <- function(tblr_obj) {
  purrr::map_df(strsplit(tblr_obj$theme$order, '')[[1]],
                get_tblr_component,
                in_tabler = tblr_obj,
                tblr_obj$theme$format) %>%
    process_osa(tblr_obj$osa, tblr_obj$absorbed_vars) %>%
    process_tabler_alias(tblr_obj) %>%
    mutate(row_num = dplyr::row_number())
}

add_lines_to_html_table <- function(kable_obj, tblr_obj) {
  order <- tblr_obj$theme$order
  body_dt <- assemble_body_dt(tblr_obj)

  for (i in 1:nchar(order)) {
    my_char <- substr(order, i, i)
    if (my_char %in% c('-', '=')) {
      # border-bottom: 1px solid
      my_css <- sprintf('1px %s',
                        if (my_char == '-') 'solid' else 'double')

      # Find the sections that precede the line
      sub_line <- substr(order, 1, i)
      last_section <- return_last_section(sub_line)

      if (is.na(last_section)) {
        kable_obj <- kableExtra::row_spec(kable_obj,
                                          1L,
                                          extra_css = sprintf('border-top: %s', my_css))
      } else {
        row_num <- filter(body_dt, tblr_type == last_section) %>%
          summarize(rownum = max(row_num)) %>%
          pull(rownum)
        if (row_num <= 0)
          next()
        kable_obj <- kableExtra::row_spec(kable_obj,
                                          row_num,
                                          extra_css = sprintf('border-bottom: %s', my_css))
      }
    }
  }

  kable_obj
}

#' Return Last Section
#'
#' Takes a subset of the order specification, strips out all of the
#' line elements, and returns the section that preceded the last
#' position.  This will be used to identify the row number that
#' the line will be added below.
#' @param x Character scalar providing the order of the tabler table.
#' Normally stored as tblr_obj$theme$order.
#' @return A single character giving the group that precdes where the
#' line will be added. If the line is to be added at the very top of
#' the table, will return NA.
return_last_section <- function(x) {
  if (!is.character(x) | length(x) != 1L)
    stop('Invalid input supplied to return_last_section.')

  only_letters <- gsub('[-=]', '', x) # removes - and = from order
  n <- nchar(only_letters)

  if(n > 0) {
    ret_val <- substr(only_letters, n, n)
  } else {
    ret_val <- NA_character_
  }

  ret_val
}
