#' Analyze Agreement Using Kappa Statistics
#'
#' Analyze consistency between raters using Cohen's, Fleiss', or Light's Kappa.
#' Wraps functions from the irr package.
#'
#' @param data A data frame or matrix with subjects as rows and raters as columns.
#' @param type Type of kappa: "cohen" (2 raters), "fleiss" (3+ raters), or "light" (3+ raters, pairwise).
#' @param detail Logical. If TRUE, returns detailed output including individual scores.
#' @return A list with kappa results, interpretation, and data summary.
#' @export
#' @examples
#' data <- data.frame(
#'   Rater1 = c("Yes", "No", "Yes", "Yes", "No"),
#'   Rater2 = c("Yes", "No", "Yes", "No", "No")
#' )
#' analyze_kappa(data, type = "cohen")
analyze_kappa <- function(data, type = "cohen", detail = FALSE) {

  check_kappa_data(data, type)
  data <- as.data.frame(data)

  if (any(sapply(data, is.character))) {
    data <- as.data.frame(lapply(data, as.factor))
  }

  result <- switch(type,
                   "cohen" = {
                     irr::kappa2(data, sort.levels = TRUE)
                   },
                   "fleiss" = {
                     irr::kappam.fleiss(data)
                   },
                   "light" = {
                     irr::kappam.light(data)
                   },
                   stop("Unknown kappa type. Choose 'cohen', 'fleiss', or 'light'.")
  )

  kappa_value <- result$value
  interpretation <- interpret_kappa(kappa_value)

  n_subjects <- nrow(data)
  n_raters <- ncol(data)

  all_values <- unlist(data)
  category_table <- table(all_values)

  output <- list(
    statistic = result,
    kappa = kappa_value,
    interpretation = interpretation,
    type = type,
    n_subjects = n_subjects,
    n_raters = n_raters,
    categories = names(category_table),
    category_counts = as.vector(category_table),
    data = if(detail) data else NULL
  )

  class(output) <- c("cate_analysis", "list")
  return(output)
}

#' Print Method for cate_analysis Objects
#'
#' @param x An object of class cate_analysis.
#' @param ... Additional arguments.
#' @export
print.cate_analysis <- function(x, ...) {
  cat("\n=== Kappa Agreement Analysis ===\n\n")
  cat("Type:", switch(x$type,
                      "cohen" = "Cohen's Kappa",
                      "fleiss" = "Fleiss' Kappa",
                      "light" = "Light's Kappa"), "\n")
  cat("Subjects:", x$n_subjects, "\n")
  cat("Raters:", x$n_raters, "\n\n")

  cat("Kappa Statistic:", round(x$kappa, 4), "\n")
  cat("Agreement Level:", x$interpretation$level, "\n")
  cat("Description:", x$interpretation$description, "\n\n")

  cat("Categories:", paste(x$categories, collapse = ", "), "\n")
  cat("Category Counts:", paste(x$category_counts, collapse = ", "), "\n")
  cat("\n")

  cat("Detailed Statistic:\n")
  print(x$statistic)
  invisible(x)
}

#' Summary Method for cate_analysis Objects
#'
#' @param object An object of class cate_analysis.
#' @param ... Additional arguments.
#' @export
summary.cate_analysis <- function(object, ...) {
  cat("\nSummary of Kappa Analysis\n")
  cat("-------------------------\n")
  cat("Kappa:", round(object$kappa, 4), "\n")
  cat("Level:", object$interpretation$level, "\n")
  cat("Description:", object$interpretation$description, "\n")
  cat("\nSubject-Rater Summary:\n")
  cat("  Subjects:", object$n_subjects, "\n")
  cat("  Raters:", object$n_raters, "\n")
  cat("  Categories:", length(object$categories), "\n")
}
