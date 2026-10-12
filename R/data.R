#' 4-highway Example
#'
#' The state highway example with 5 destinations used in the paper.
#'
#' @format ## `highway4`
#' This is a list containing all elements needed to run the examples presented in the paper.
#' \describe{
#'   \item{A}{Configuration matrix}
#'   \item{FMB}{Full Markov basis for A, found by 4ti2}
#'   \item{LB}{1-Markov basis, found by partitioning A (first 4 columns are in A1)}
#'   \item{x}{An element of the fibre}
#'   \item{y}{An observation of the road system}
#'   \item{lambda}{Estimated origin/destination rates under Poisson assumption}
#'   \item{A2IdxLB}{The index of columns for A2 when A is partitioned to form LB}
#'   \item{A1IdxFMB}{The index of columns for A1 when A is partitioned to form FMB}
#' }
"highway4"

#' 7-highway Example
#'
#' The state highway example with 7 destinations used in the supplementary material.
#'
#' @format ## `highway7`
#' This is a list containing all elements needed to run the examples presented in the paper.
#' \describe{
#'   \item{A}{Configuration matrix}
#'   \item{FMB}{Full Markov basis for A, found by 4ti2}
#'   \item{LB}{1-Markov basis, found by partitioning A (first 4 columns are in A1)}
#'   \item{y}{An observation of the road system}
#'   \item{lambda}{Estimated origin/destination rates under Poisson assumption}
#'   \item{A2IdxLB}{The index of columns for A2 when A is partitioned to form LB}
#'   \item{A1IdxFMB}{The index of columns for A1 when A is partitioned to form FMB}
#' }
"highway7"

#' 3x4x6 Contingency table Example
#'
#' The 3-way contingency table example used in the supplementary material.
#'
#' @format ## `contingencyTable3x4x6`
#' This is a list containing all elements needed to run the examples presented in the paper.
#' \describe{
#'   \item{A}{Configuration matrix}
#'   \item{FMB}{Full Markov basis for A, found by 4ti2}
#'   \item{B}{1-Markov basis, found using Yoshida and Barnhill's result (The collection of basic moves)}
#'   \item{y}{An observation of the road system}
#'   \item{lambda}{Estimated origin/destination rates under Poisson assumption}
#'   \item{A2IdxHR}{The index of columns for A2 when A is partitioned for the hyperrectangle sampler}
#' }
"contingencyTable3x4x6"

#' 3x4x12 Contingency table Example
#'
#' The 3-way contingency table example used in the paper.
#'
#' @format ## `contingencyTable3x4x12`
#' This is a list containing all elements needed to run the examples presented in the paper.
#' \describe{
#'   \item{A}{Configuration matrix}
#'   \item{B}{1-Markov basis, found using Yoshida and Barnhill's result (The collection of basic moves)}
#'   \item{x}{An element of the fibre}
#'   \item{y}{An observation of the road system}
#'   \item{lambda}{Estimated origin/destination rates under Poisson assumption}
#'   \item{A2IdxHR}{The index of columns for A2 when A is partitioned for the hyperrectangle sampler}
#' }
"contingencyTable3x4x12"

#' 3x4x20 Contingency table Example
#'
#' The 3-way contingency table example used in the supplementary material.
#'
#' @format ## `contingencyTable3x4x20`
#' This is a list containing all elements needed to run the examples presented in the paper.
#' \describe{
#'   \item{A}{Configuration matrix}
#'   \item{B}{1-Markov basis, found using Yoshida and Barnhill's result (The collection of basic moves)}
#'   \item{x}{An element of the fibre}
#'   \item{y}{An observation of the road system}
#'   \item{A2IdxHR}{The index of columns for A2 when A is partitioned for the hyperrectangle sampler}
#' }
"contingencyTable3x4x20"
