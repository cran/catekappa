#' Calculate Sample Size for Kappa Statistic
#'
#' Calculate required sample size for consistency tests.
#' Directly wraps kappaSize::PowerBinary / Power3Cats / Power4Cats / Power5Cats.
#'
#' @param kappa0 Null hypothesis value of kappa (H0).
#' @param kappa1 Alternative hypothesis value of kappa (H1).
#' @param props Expected proportions of categories. Must sum to 1.
#' @param alpha Significance level. Default 0.05.
#' @param power Desired power. Default 0.8.
#' @param raters Number of raters (>=2). Default 2.
#' @return A list with sample size n and parameters.
#' @export
#' @examples
#' calc_sample_size_kappa(kappa0 = 0.4, kappa1 = 0.6, props = c(0.5, 0.5))
#' calc_sample_size_kappa(kappa0 = 0.4, kappa1 = 0.6,
#'                        props = c(0.6, 0.3, 0.1), raters = 3)
calc_sample_size_kappa <- function(kappa0 = 0.4, kappa1 = 0.6,
                                   props = c(0.5, 0.5),
                                   alpha = 0.05, power = 0.8,
                                   raters = 2) {

  if (kappa0 < 0 || kappa0 > 1 || kappa1 < 0 || kappa1 > 1) {
    stop("Kappa values must be between 0 and 1")
  }

  if (abs(sum(props) - 1) > 1e-6) {
    props <- props / sum(props)
    warning("Category proportions were normalized to sum to 1")
  }

  if (raters < 2) {
    stop("Number of raters must be at least 2")
  }

  k <- length(props)

  if (k == 2) {
    res <- kappaSize::PowerBinary(
      kappa0 = kappa0, kappa1 = kappa1,
      props = props[1],
      alpha = alpha, power = power
    )
    n <- as.numeric(res)
    func_name <- "PowerBinary"
  } else if (k == 3) {
    res <- kappaSize::Power3Cats(
      kappa0 = kappa0, kappa1 = kappa1,
      props = props,
      raters = raters,
      alpha = alpha, power = power
    )
    n <- res$N
    func_name <- "Power3Cats"
  } else if (k == 4) {
    res <- kappaSize::Power4Cats(
      kappa0 = kappa0, kappa1 = kappa1,
      props = props,
      raters = raters,
      alpha = alpha, power = power
    )
    n <- res$N
    func_name <- "Power4Cats"
  } else if (k == 5) {
    res <- kappaSize::Power5Cats(
      kappa0 = kappa0, kappa1 = kappa1,
      props = props,
      raters = raters,
      alpha = alpha, power = power
    )
    n <- res$N
    func_name <- "Power5Cats"
  } else {
    stop("kappaSize only supports 2 to 5 categories. Your props has ", k, " categories.")
  }

  structure(list(
    n = as.numeric(n),
    kappa0 = kappa0,
    kappa1 = kappa1,
    props = props,
    alpha = alpha,
    power = power,
    raters = raters,
    categories = k,
    kappaSize_function = func_name
  ), class = "cate_design")
}

#' Print Method for cate_design Objects
#'
#' @param x An object of class cate_design.
#' @param ... Additional arguments.
#' @export
print.cate_design <- function(x, ...) {
  cat("\n=== Sample Size Calculation for Kappa Statistic ===\n\n")
  cat("Required Sample Size:", ceiling(x$n), "\n")
  cat("kappaSize Function:", x$kappaSize_function, "\n\n")
  cat("Parameters:\n")
  cat("  Null Kappa (H0):      ", x$kappa0, "\n")
  cat("  Alternative Kappa (H1):", x$kappa1, "\n")
  cat("  Categories:           ", x$categories, "\n")
  cat("  Category Proportions: ", paste(round(x$props, 3), collapse = ", "), "\n")
  cat("  Significance Level:   ", x$alpha, "\n")
  cat("  Power:                ", x$power, "\n")
  cat("  Number of Raters:     ", x$raters, "\n")
  cat("\n")
  invisible(x)
}

#' Fixed N Analysis for Kappa Statistic
#'
#' Given a fixed sample size, estimate the lower confidence bound.
#' Wraps kappaSize::FixedNBinary / FixedN3Cats / FixedN4Cats / FixedN5Cats.
#'
#' @param n Sample size.
#' @param kappa0 Anticipated value of kappa.
#' @param props Category proportions.
#' @param alpha Significance level.
#' @param raters Number of raters.
#' @return List with kappaSize raw result and parameters.
#' @export
kappa_fixed_n <- function(n, kappa0 = 0.4,
                          props = c(0.5, 0.5),
                          alpha = 0.05, raters = 2) {

  if (abs(sum(props) - 1) > 1e-6) {
    props <- props / sum(props)
  }

  k <- length(props)

  if (k == 2) {
    res <- kappaSize::FixedNBinary(
      kappa0 = kappa0,
      props = props[1],
      n = n,
      alpha = alpha
    )
    func_name <- "FixedNBinary"
  } else if (k == 3) {
    res <- kappaSize::FixedN3Cats(
      kappa0 = kappa0,
      props = props,
      n = n,
      raters = raters,
      alpha = alpha
    )
    func_name <- "FixedN3Cats"
  } else if (k == 4) {
    res <- kappaSize::FixedN4Cats(
      kappa0 = kappa0,
      props = props,
      n = n,
      raters = raters,
      alpha = alpha
    )
    func_name <- "FixedN4Cats"
  } else if (k == 5) {
    res <- kappaSize::FixedN5Cats(
      kappa0 = kappa0,
      props = props,
      n = n,
      raters = raters,
      alpha = alpha
    )
    func_name <- "FixedN5Cats"
  } else {
    stop("kappaSize only supports 2 to 5 categories. Your props has ", k, " categories.")
  }

  structure(list(
    n = n,
    kappa0 = kappa0,
    props = props,
    alpha = alpha,
    raters = raters,
    categories = k,
    kappaSize_result = res,
    kappaSize_function = func_name
  ), class = "cate_fixed_n")
}

#' Print Method for cate_fixed_n Objects
#'
#' @param x An object of class cate_fixed_n.
#' @param ... Additional arguments.
#' @export
print.cate_fixed_n <- function(x, ...) {
  cat("\n=== Fixed N Analysis for Kappa Statistic ===\n\n")
  cat("Sample Size:", x$n, "\n")
  cat("kappaSize Function:", x$kappaSize_function, "\n\n")
  cat("Parameters:\n")
  cat("  Kappa0:               ", x$kappa0, "\n")
  cat("  Categories:           ", x$categories, "\n")
  cat("  Category Proportions: ", paste(round(x$props, 3), collapse = ", "), "\n")
  cat("  Significance Level:   ", x$alpha, "\n")
  cat("  Number of Raters:     ", x$raters, "\n\n")
  cat("kappaSize Raw Output:\n")
  print(x$kappaSize_result)
  invisible(x)
}
