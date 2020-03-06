#' Summarise bfrr object
#'
#' @param object a bfrr list
#' @param ... Other arguments
#'
#' @return string
#' @export
#'
summary.bfrr <- function(object, ...) {
  precision <- object$rr_data[1,2] - object$rr_data[1, 1]
  half <- ifelse(identical(object$params$tail, 1), "H", "")

  RR <- sapply(names(object$RR), function(n) {
    rng <- object$RR[[n]]
    if (unique(rng) %>% length() > 1) {
      dplyr::case_when(
        n == "mean" ~ paste0(half, "N([", toString(rng), "], ", object$params$sd, ")"),
        n == "sd" ~ paste0(half, "N(", object$params$mean, ", [", toString(rng), "])"),
        n == "lower" ~ paste0("U([", toString(rng), "], ", object$params$upper, ")"),
        n == "upper" ~ paste0("U(", object$params$lower, ", [", toString(rng), "])")
      )
    } else {
      "no"
    }

  })

  RR <- RR[RR != "no"] %>% paste(collapse = "; ")

  explanation <- sprintf(
    "The likelihood of your data under the theoretical distribution %s is %.2f. The likelihood of your data under the null distribution %s is %.2f. The Bayes Factor is %.2f; this test finds %s with a criterion of %s. The region of theoretical model parameters that give the same conclusion is `%s`.",
    object$H1_model,
    round(object$theory, 2),
    object$H0_model,
    round(object$null, 2),
    round(object$BF, 2),
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
    RR
  )

  cat(explanation, "\n")
}




