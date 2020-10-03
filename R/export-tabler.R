export_kable_html <- function(tblr_obj) {
  out_dt <- reformat_tabler_obj(tblr_obj)
  body_dt <- assemble_body_dt(tblr_obj)
  for_table_dt <- process_group_variables(body_dt, tblr_obj)

  kableExtra::kable(for_table_dt,
                    caption = extract_caption(tblr_obj),
                    format = 'html',
                    align = c('l', rep('c', dim(for_table_dt)[2] - 1L)),
                    booktabs = FALSE,
                    escape = TRUE,
                    col.names = names(for_table_dt))
}

export_kable_latex <- function(tblr_obj) {

  out_dt <- reformat_tabler_obj(tblr_obj)

  # The remainder of the function applies to HTML or LaTeX
  header_dt <- filter(out_dt, tblr_type %notin% c('C', 'G'))
  body_dt <- assemble_body_dt(tblr_obj)

  for_table_dt <- process_group_variables(body_dt, tblr_obj)

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
    do_packing(tblr_obj) %>%
    add_header_rows(header_dt) %>%
    clean_errant_codes()

}

reformat_tabler_obj <- function(tblr_obj) {
  purrr::map_df(strsplit(tblr_obj$theme$order, '')[[1]],
                get_tblr_component,
                in_tabler = tblr_obj,
                this_format) %>%
    process_osa(tblr_obj$osa, tblr_obj$absorbed_vars) %>%
    process_tabler_alias(tblr_obj) %>%
    mutate(row_num = dplyr::row_number())
}
