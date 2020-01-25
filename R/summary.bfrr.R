#' Summarise bfrr object
#'
#' @param x a bfrr list
#' @param ... Other arguments
#'
#' @return string
#' @export
#'
summary.bfrr <- function(x, ...) {
  precision <- log10(x$interval$SD[2] - x$interval$SD[1]) %>% abs() %>% round()
  explanation <- sprintf(
    paste0("The likelihood of your data under the theoretical distribution %s is %.2f. The likelihood of your data under the null distribution %s is %.2f. The Bayes Factor is %.1f; this test finds %s with a criterion of %s. The region of theoretical SDs that give the same conclusion is RR = [%.", precision, "f, %.", precision, "f]."),
    x$H1_model,
    round(x$theory, 2),
    x$H0_model,
    round(x$null, 2),
    round(x$BF, 1),
    dplyr::case_when(
      x$conclusion == "H0" ~ "evidence for H0",
      x$conclusion == "H1" ~ "evidence for H1",
      TRUE ~ "no evidence for H0 or H1"
    ),
    dplyr::case_when(
      x$conclusion == "H0" ~ paste0("1/", x$criterion),
      x$conclusion == "H1" ~ as.character(x$criterion),
      TRUE ~ paste0(x$criterion, " (1/", x$criterion, ")")
    ),
    x$RR[1], x$RR[2]
  )

  cat(explanation, "\n")
}

