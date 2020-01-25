#' Show bfrr plot
#'
#' @param x a bfrr list
#' @param ... Other arguments
#'
#' @return plot
#' @export
#'
plot.bfrr <- function(obj, ...) {
  m <- regexec("\\(0, (.*)\\)", obj$H1_model) %>%
        regmatches(obj$H1_model, .)
  obj$theory_sd <- as.numeric(m[[1]][2])

  interval <- obj$interval$SD[2] - obj$interval$SD[1]

  ggplot2::ggplot(obj$interval, ggplot2::aes(SD, BF)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = SD-interval/2, xmax = SD+interval/2, fill = support), ymin = -Inf, ymax = Inf, alpha = 0.5) +
    ggplot2::geom_hline(yintercept = obj$criterion, color = "grey50") +
    ggplot2::geom_hline(yintercept = 1/obj$criterion, color = "grey50") +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = obj$theory_sd, y = obj$BF), color = "red", size = 2) +
    ggplot2::scale_fill_manual(
      values = c("seagreen", "goldenrod", "indianred"),
      drop=FALSE
    ) +
    ggplot2::labs(x = "SD of H1", y = "Bayes Factor") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(trans='log10')
}
