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

  cat(sprintf(out_line,
              var_name,
              pca(coef_list[2:(num_cols + 1)]),
              pca(coef_list[(num_cols + 2):(2 * num_cols + 1)],
                  pre = "(",
                  post = ")")))
}


