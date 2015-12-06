#' Make a tabler object
#'
#' This function produces the underlying tabler object.
#' @export
tabler <- function(...) {
  in_cols <- list(...)

  # Check to make sure that every element of in_cols is a colRec.
  # If it's not, then convert it.
  for (i in seq_along(in_cols)) {
    if (class(in_cols[[i]]) != "tabler_column") {
      in_cols[[i]] <- make_column(in_cols[[i]])
    }
  }

  # Create the tabler object
  tblr_obj <- list()
  attr(tblr_obj, "class") <- "tabler_object"
  tblr_obj$title <- NA
  tblr_obj$notes <- NA
  tblr_obj$number <- NA
  tblr_obj$latex_label <- NA

  tblr_obj$dep_vars <- unlist(lapply(in_cols, function(x) x$dep_var))
  tblr_obj$var_names <- unique(unlist(lapply(in_cols, function(x) x$var_names)))
  tblr_obj$est_types <- unlist(lapply(in_cols, function(x) x$est_type))

  # I am going to stack the coefficient matrices
  # estNum will record which estimation (column) the coefficients belong in
  for (j in seq_along(in_cols)) in_cols[[j]]$coefs$est_num <- j
  tblr_obj$coefs <- do.call("rbind", lapply(in_cols, function(x) x$coefs))

  # Combining gof data frames will be more difficult because the number of columns (stats)
  # may not be identical
  tblr_obj$gofs <- data.frame(junk = rep(NA, length(in_cols)))
  gof_names <- unique(unlist(lapply(in_cols, function(x) names(x$gof))))
  for (this_name in gof_names) {
    tblr_obj$gofs[this_name] <- unlist(lapply(in_cols, function(x) ifelse(is.element(this_name, names(x$gof)), x$gof[this_name],NA)))
  }
  tblr_obj$gofs["junk"] <- NULL

  # xlevels will follow the same method as gof.  It will be tougher though because
  # I do not know in advance the size of the vector.  This doesn't need to be a
  # data frame.  I'll make it a list.
  temp <- list()
  xlevels <- unique(unlist(lapply(in_cols, function(x) names(x$xlevels))))
  for (this_level in xlevels) {
    temp[[this_level]] <- unique(unlist(lapply(in_cols, function(x) ifelse(is.element(this_level, names(x$xlevels)), x$xlevels[this_level],NA))))
    if (any(is.na(temp[[this_level]]))) {  # If there are NA's, get rid of them
      temp[[this_level]] <- temp[[this_level]][-which(is.na(temp[[this_level]]))]
    }
  }
  tblr_obj$xlevels <- temp

  tblr_obj$theme <- tabler_theme() # Set the theme values as defaults

  # Order the coefficient vector
  tblr_obj$coefs <- order_variables(tblr_obj$var_names, tblr_obj$xlevels, tblr_obj$coefs)

  return(tblr_obj)
}


`+.tabler_object` <- function(tblr_obj, new_object) {
  if (class(new_object) == "tabler_theme") tblr_obj$theme <- new_object
  else {
    if (class(new_object) != "tabler_col") new_object <- makeColumn(new_object)

    tblr_obj$dep_vars <- c(tblr_obj$dep_vars, new_object$dep_var)
    tblr_obj$var_names <- unique(c(tblr_obj$var_names, new_object$var_names))
    tblr_obj$estTypes <- c(tblr_obj$est_types, new_object$est_type)

    # Stack the coefficient data.frames
    new_object$coefs$est_num <- max(tblr_obj$coefs$est_num) + 1
    tblr_obj$coefs <- rbind(tblr_obj$coefs, new_object$coefs)

    # Combine the gof data.frames.  This is complicated because the statistics will differ across
    # the columns.
    new_names <- names(new_object$gof)[names(new_object$gof) %notin% names(tblr_obj$gofs)]
    tblr_obj$gofs[, new_names] <- NA
    empty_names <- names(tblr_obj$gofs)[names(tblr_obj$gofs) %notin% names(new_object$gof)]
    new_object$gof[, empty_names] <- NA
    tblr_obj$gofs <- rbind(tblr_obj$gofs, new_object$gof[, names(tblr_obj$gofs)])

    # Consolidate xlevels
    # There must be a more efficient way to do this.
    temp <- list()
    xlevels <- unique(c(names(tblr_obj$xlevels), names(new_object$xlevels)))
    for (this_level in xlevels) {
      if (this_level %in% names(tblr_obj$xlevels)) {
        if (this_level %in% names(new_object$xlevels)) {
          temp[[this_level]] <- unique(c(tblr_obj$xlevels[[this_level]], new_object$xlevels[[this_level]]))
        } else {
          temp[[this_level]] <- tblr_obj$xlevels[[this_level]]
        }
      } else {
        temp[[this_level]] <- new_object$xlevels[[this_level]]
      }
    }
    tblr_obj$xlevels <- temp
  }

  # Order the coefficient vector
  tblr_obj$coefs <- order_variables(tblr_obj$var_names, tblr_obj$xlevels, tblr_obj$coefs)

  tblr_obj
}

