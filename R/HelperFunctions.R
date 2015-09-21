`%notin%` <- function(ina, inb) {
  !(ina %in% inb)
}

# pca stands for "paste - collapse - ampersand
pca <- function(inVec, pre = "", post = "") {
  inVec <- paste0(pre, inVec, post)
  paste(inVec, collapse = ' & ')
}


