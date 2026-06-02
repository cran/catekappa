#' catekappa: Design and Analysis of Categorical Agreement Tests Based on Kappa Statistics
#'
#' @description
#' CATEKAPPA (Categorical Agreement Test Evaluation) provides a Shiny interactive
#' application and supporting functions for the design and analysis of
#' categorical agreement tests.
#'
#' @details
#' This package wraps the core functionality of the \code{irr} and
#' \code{kappaSize} packages:
#' \itemize{
#'   \item \strong{Design stage}: Use \code{\link{calc_sample_size_kappa}} to
#'         calculate sample size, supporting 2–5 categories and 2+ raters.
#'   \item \strong{Analysis stage}: Use \code{\link{analyze_kappa}} to compute
#'         Cohen's, Fleiss', and Light's Kappa statistics.
#'   \item \strong{Interactive app}: Use \code{\link{run_cate_app}} to launch
#'         the Shiny interface.
#' }
#'
#' @author
#'   Gai Zheng \email{z2118778229@163.com}
#'   Xincheng Li \email{lxc409014@qq.com}
#'   Yingjie Jiangwang \email{2312055564@qq.com}
#'   Panwei Zhao \email{1581729526@qq.com}
#'
#' @seealso
#' \code{\link{run_cate_app}}, \code{\link{calc_sample_size_kappa}},
#' \code{\link{analyze_kappa}}
#'
#' @keywords package
"_PACKAGE"
