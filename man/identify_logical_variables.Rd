% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-column.R
\name{identify_logical_variables}
\alias{identify_logical_variables}
\title{Identify Logical Variables}
\usage{
identify_logical_variables(x, var_names)
}
\arguments{
\item{x}{A character scalar with an unaccounted for variable name}

\item{var_names}{List of known variables in the estimation}
}
\value{
A tibble with the name of the variable (if it is logical) and its
logical value.
}
\description{
Examine unaccounted for variables from the coefficient matrix and determine
which are logical.  This returns 1 element of a data.frame of data that
gives the variable name and logical result of those variables. Otherwise,
throws a warning if the unaccounted for name is not logical.
}
