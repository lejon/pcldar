% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcldar.R
\name{get_held_out_log_likelihood}
\alias{get_held_out_log_likelihood}
\title{get_held_out_log_likelihood}
\usage{
get_held_out_log_likelihood(lda)
}
\arguments{
\item{lda}{the trained lda model}
}
\description{
Extract the heldo log likelihood for each iteration. The sampler must have
been run with a test set
}
