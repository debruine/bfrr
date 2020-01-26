#' Show bfrr plot
#'
#' @param x a bfrr list
#' @param ... Other arguments
#'
#' @return plot
#' @export
#'
plot.bfrr <- function(x, ...) {
  ## recover model params ----
  m <- regexec("(.)\\((.*), (.*)\\)", x$H1_model) %>%
        regmatches(x$H1_model, .)
  model <- m[[1]][2]
  param1 <- as.numeric(m[[1]][3])
  param2 <- as.numeric(m[[1]][4])

  interval <- x$rr_data[1, 2] - x$rr_data[1, 1]

  x$rr_data$support <- factor(x$rr_data$support,
                               levels = c("H1", "no", "H0"),
                               labels = c("Evidence for H1",
                                          "No evidence",
                                          "Evidence for H0"))

  if (all(c("M", "SD") %in% names(x$rr_data))) {
    if (length(unique(x$rr_data$M)) > 1) {
      ## N with varying M and SD ----
      ggplot2::ggplot(x$rr_data, ggplot2::aes(SD, M, fill = support)) +
        ggplot2::geom_tile() +
        ggplot2::geom_point(ggplot2::aes(x = param2, y = param1), size = 2, color = "black", show.legend = FALSE) +
        ggplot2::scale_fill_manual(
          name = "",
          values = c("seagreen", "goldenrod", "indianred"),
          drop=FALSE
        ) +
        ggplot2::labs(x = "SD of H1", y = "Mean of H1")

    } else {
      ## N with only varying SD ----
      ggplot2::ggplot(x$rr_data, ggplot2::aes(SD, BF)) +
        ggplot2::geom_rect(ggplot2::aes(xmin = SD-interval/2, xmax = SD+interval/2, fill = support), ymin = -Inf, ymax = Inf, alpha = 0.25) +
        ggplot2::geom_hline(yintercept = x$criterion, color = "grey50") +
        ggplot2::geom_hline(yintercept = 1/x$criterion, color = "grey50") +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_point(ggplot2::aes(x = param2, y = x$BF), color = "dodgerblue", size = 2) +
        ggplot2::scale_fill_manual(
          name = "",
          values = c("seagreen", "goldenrod", "indianred"),
          drop=FALSE
        ) +
        ggplot2::labs(x = "SD of H1", y = "Bayes Factor") +
        ggplot2::theme_bw() +
        ggplot2::scale_y_continuous(trans='log10')
    }
  } else if (all(c("lower", "upper") %in% names(x$rr_data))) {
    ## U ---
    ggplot2::ggplot(x$rr_data, ggplot2::aes(lower, upper, fill = support)) +
      ggplot2::geom_tile() +
      ggplot2::geom_point(ggplot2::aes(x = param1, y = param2), size = 2, color = "black", show.legend = FALSE) +
      ggplot2::scale_fill_manual(
        name = "",
        values = c("seagreen", "goldenrod", "indianred"),
        drop=FALSE
      ) +
      ggplot2::labs(x = "Lower Bound of H1", y = "Upper Bound of H1")
  }
}
