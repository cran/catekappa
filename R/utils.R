#' Interpret Kappa Value
#'
#' Interpret the strength of agreement based on Landis and Koch criteria.
#'
#' @param kappa Numeric value of Kappa statistic.
#' @return A named list with level, description, and color code.
#' @export
#' @examples
#' interpret_kappa(0.3)
#' interpret_kappa(0.75)
interpret_kappa <- function(kappa) {
  if (!is.numeric(kappa) || length(kappa) != 1) {
    stop("kappa must be a single numeric value")
  }

  if (kappa < 0) {
    return(list(
      level = "Poor",
      description = "Less than chance agreement",
      color = "#d9534f"
    ))
  } else if (kappa < 0.20) {
    return(list(
      level = "Slight",
      description = "Slight agreement",
      color = "#f0ad4e"
    ))
  } else if (kappa < 0.40) {
    return(list(
      level = "Fair",
      description = "Fair agreement",
      color = "#f0ad4e"
    ))
  } else if (kappa < 0.60) {
    return(list(
      level = "Moderate",
      description = "Moderate agreement",
      color = "#5bc0de"
    ))
  } else if (kappa < 0.80) {
    return(list(
      level = "Substantial",
      description = "Substantial agreement",
      color = "#5cb85c"
    ))
  } else {
    return(list(
      level = "Almost perfect",
      description = "Almost perfect agreement",
      color = "#5cb85c"
    ))
  }
}

#' Check Kappa Data Validity
#'
#' Internal function to validate input data for kappa analysis.
#'
#' @param data A data frame or matrix.
#' @param type Type of kappa analysis.
#' @return Logical indicating validity.
#' @keywords internal
check_kappa_data <- function(data, type = "cohen") {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame")
  }

  data <- as.data.frame(data)

  if (type == "cohen" && ncol(data) != 2) {
    stop("Cohen's kappa requires exactly 2 raters (2 columns)")
  }

  if (ncol(data) < 2) {
    stop("At least 2 raters (columns) are required")
  }

  if (any(is.na(data))) {
    warning("Data contains missing values. Analysis may remove incomplete cases.")
  }

  return(TRUE)
}
