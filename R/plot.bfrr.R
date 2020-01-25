#' Show bfrr plot
#'
#' @param x a bfrr list
#' @param ... Other arguments
#'
#' @return plot
#' @export
#'
plot.bfrr <- function(x, ...) {
  m <- regexec("\\(0, (.*)\\)", x$H1_model) %>%
        regmatches(x$H1_model, .)
  x$theory_sd <- as.numeric(m[[1]][2])

  interval <- x$interval$SD[2] - x$interval$SD[1]
  x$interval$support <- factor(x$interval$support,
                               levels = c("H1", "no", "H0"),
                               labels = c("Evidence for H1",
                                          "No evidence",
                                          "Evidence for H0"))

  ggplot2::ggplot(x$interval, ggplot2::aes(SD, BF)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = SD-interval/2, xmax = SD+interval/2, fill = support), ymin = -Inf, ymax = Inf, alpha = 0.25) +
    ggplot2::geom_hline(yintercept = x$criterion, color = "grey50") +
    ggplot2::geom_hline(yintercept = 1/x$criterion, color = "grey50") +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = x$theory_sd, y = x$BF), color = "dodgerblue", size = 2) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("seagreen", "goldenrod", "indianred"),
      drop=FALSE
    ) +
    ggplot2::labs(x = "SD of H1", y = "Bayes Factor") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(trans='log10')
}
