#' Bayes Factor with Robustness Region (normal)
#'
#' @param sample_mean the observed sample mean
#' @param sample_se the observed sample standard error
#' @param sample_df the observed sample degrees of freedom
#' @param model the model under which to calculate likelihood (H0, normal or uniform
#' @param ... model parameters (mean, sd and tail for normal, lower and upper for uniform)
#' @param criterion the cutoff Bayes Factor for concluding evidence in favour of H0 or H1
#' @param rr_interval the interval within which to test for robustness (default 0 to twice the theory_sd)
#' @param precision the number of decimal places to calculate the robustness region to (default 2)
#'
#' @return list
#' @export
#'
#' @examples
#'
#' rr <- bfrr(0.5, 1/sqrt(30), 29, 0, .7, 1, 5, c(0,3))
#' summary(rr)
#' plot(rr)
bfrr <- function(sample_mean = 0,
                 sample_se = 0.1,
                 sample_df = 99,
                 model = "normal",
                 ...,
                 criterion = 3,
                 rr_interval = NA,
                 precision = 2) {

  params <- list(...)
  model <- toupper(substr(model, 1, 1))

  x <- list(
    null = likelihood(sample_mean, sample_se, sample_df),
    theory = likelihood(sample_mean, sample_se, sample_df, model, ...)
  )
  x$BF <- x$theory/x$null

  # check BF against criterion
  if (x$BF >= criterion) {
    conclusion <- "H1"
  } else if (x$BF <= 1/criterion) {
    conclusion <- "H0"
  } else {
    conclusion <- "no"
  }

  # calculate robustness region

  if (model == "N") {
    theory_mean <- default(params$mean, 0)
    theory_sd <- default(params$sd, 1)
    tail <- default(params$tail, 2)
    if (all(is.na(rr_interval))) rr_interval <- c(0, 2*theory_sd)

    rr_sd <- seq(rr_interval[1], rr_interval[2], 10^-precision)
    rr_sd <- rr_sd[rr_sd > 0]
    rr_BF <- purrr::map_dbl(rr_sd, function(rr_theory_sd) {
      likelihood(sample_mean, sample_se, sample_df, model, mean = theory_mean,
                 sd = rr_theory_sd, tail = tail) / x$null
    })

    H1 <- sprintf("%sN(%g, %g)",
                  ifelse(identical(tail,1),"H", ""),
                  theory_mean, theory_sd)

  } else if (model == "U") {
    lower <- default(params$lower, 0)
    upper <- default(params$upper, 1)
    if (all(is.na(rr_interval))) rr_interval <- c(lower, upper)

    rr_sd <- c()
    rr_BF <- c()

    H1 <- sprintf("U(%g, %g)", lower, upper)
  }

  dat <- data.frame(
    "SD" = rr_sd,
    "BF" = rr_BF
  )
  dat$support <- dplyr::case_when(
    dat$BF >= criterion ~ "H1",
    dat$BF <= 1/criterion ~ "H0",
    TRUE ~ "no"
  )

  rr <- range(dplyr::filter(dat, support == conclusion)$SD)

  #H0 <- sprintf("N(0, %g)", sample_se)
  H0 <- sprintf("T(%g)", sample_df)

  ret <- list("theory" = x$theory,
              "null" = x$null,
              "BF" = x$BF,
              "RR" = rr,
              "conclusion" = conclusion,
              "sample_mean" = sample_mean,
              "sample_se" = sample_se,
              "sample_df" = sample_df,
              "criterion" = criterion,
              "H0_model" = H0,
              "H1_model" = H1,
              "interval" = dat)

  class(ret) <- c("bfrr", "list")

  invisible(ret)
}
