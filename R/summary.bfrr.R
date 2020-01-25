#' Summarise bfrr object
#'
#' @param object a bfrr list
#' @param ... Other arguments
#'
#' @return string
#' @export
#'
summary.bfrr <- function(object, ...) {
  precision <- log10(object$interval$SD[2] - object$interval$SD[1]) %>% abs() %>% round()
  explanation <- sprintf(
    paste0("The likelihood of your data under the theoretical distribution %s is %.2f. The likelihood of your data under the null distribution %s is %.2f. The Bayes Factor is %.1f; this test finds %s with a criterion of %s. The region of theoretical SDs that give the same conclusion is RR = [%.", precision, "f, %.", precision, "f]."),
    object$H1_model,
    round(object$theory, 2),
    object$H0_model,
    round(object$null, 2),
    round(object$BF, 1),
    dplyr::case_when(
      object$conclusion == "H0" ~ "evidence for H0",
      object$conclusion == "H1" ~ "evidence for H1",
      TRUE ~ "no evidence for H0 or H1"
    ),
    dplyr::case_when(
      object$conclusion == "H0" ~ paste0("1/", object$criterion),
      object$conclusion == "H1" ~ as.character(object$criterion),
      TRUE ~ paste0(object$criterion, " (1/", object$criterion, ")")
    ),
    object$RR[1], object$RR[2]
  )

  cat(explanation, "\n")
}

