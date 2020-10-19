`%notin%` <- function(ina, inb) {
  !(ina %in% inb)
}

# pca stands for "paste - collapse - ampersand
pca <- function(in_vec, pre = "", post = "") {
  # Ensure that if the value of in_vec is blank, pre and post characters are not added.
  in_vec <- ifelse((in_vec == "") | (is.na(in_vec)),
                   "",
                   paste0(pre, in_vec, post))
  paste(in_vec, collapse = ' & ')
}

output_coef <- function(var_name, var_pos, coef_list, num_cols) {
  prefix <- "\\multicolumn{2}{l}{%s} & "
  if (var_pos == 2) prefix <- " & %s & "
  out_line <- paste0(prefix, "%s \\\\ \n & & %s \\\\ \n")

  sprintf(out_line,
          var_name,
          pca(coef_list[2:(num_cols + 1)]),
          pca(coef_list[(num_cols + 2):(2 * num_cols + 1)],
              pre = "(",
              post = ")"))
}

num <- function(x, digits = 3L) {
  char_if_negative <- ifelse(x < 0, 1L, as.integer(0))
  log_val <- log10(abs(x))
  # Convert to character, where all elements with decimals == digits
  temp <- sprintf('%%#.0%df', digits) %>%
    sprintf(x)
  # Calculate the position in the character where the decimal point is
  dec_pos <- stringr::str_locate(temp, '\\.') %>%
    as.data.frame() %>%
    pull(start)

  # Determine what the length the character should be
  my_digits <- dplyr::case_when(
    (dec_pos - 1L) > digits ~ dec_pos - 1L,  # Take everything up to the decimal
    log_val >= 1 ~ as.integer(digits) + 2L,
    TRUE ~ as.integer(digits) + 2L + char_if_negative)  # compensate for leading zero and decimal point

  # Add a comma for large values, set widths individually
  prettyNum(substr(temp, 1L, my_digits),
            big.mark = ',',
            preserve.width = 'individual')
}


