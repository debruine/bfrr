#' Calculate likelihood of data under model
#'
#' @param sample_mean the observed sample mean
#' @param sample_se the observed sample standard error
#' @param sample_df the observed sample degrees of freedom
#' @param model the model under which to calculate likelihood (H0, normal or uniform
#' @param ... model parameters (mean, sd and tail for normal, lower and upper for uniform)
#' @param steps the number of steps to integrate over
#'
#' @return double
#' @export
#'
#' @examples
#'
#' lNull <- likelihood(0.5, 1/sqrt(30), 29, "H0")
#' lTheory <- likelihood(0.5, 1/sqrt(30), 29, "normal", mean = 0, sd = 1, tail = 1)
#' BayesFactor <- lNull/lTheory
#'
likelihood <- function(sample_mean = 0,
                       sample_se = 0.1,
                       sample_df = 99,
                       model = c("H0", "normal", "uniform"),
                       ...,
                       steps = 2000) {
  model <- trimws(tolower(model[[1]]))
  params <- list(...)
  likelihood <- NA

  if (model == "h0") { # H0 ----
    likelihood <- stats::dt(sample_mean/sample_se, df = sample_df)
  } else if (model %in% c("normal", "norm", "n")) { # Normal ----
    # get parameters or defaults
    theory_mean <-default(params$mean, 0)
    theory_sd <- default(params$sd, 1)
    tail <- default(params$tail, 2)

    # integrate over +/- 5 SD generally captures all the area
    incr <- theory_sd * 10 / steps
    lower <- theory_mean - 5 * theory_sd
    upper <- theory_mean + 5 * theory_sd
    theta_list <- seq(lower, upper-incr, incr)
    if (identical(tail, 1)) {
      # if 1-tailed, skip all theta <= 0 and double the increment multiplier
      theta_list <- theta_list[theta_list>0]
      incr <- 2*incr
    }

    dist_theta <- stats::dnorm(theta_list, theory_mean, theory_sd)
    heights <- stats::dt((sample_mean-theta_list)/sample_se, df=sample_df)

    likelihood <- sum(dist_theta * heights)*incr
  } else if (model %in% c("uniform", "unif", "u")) { # Uniform ----
    # get parameters or defaults
    lower <- default(params$lower, 0)
    upper <- default(params$upper, 1)
    if (lower > upper) { # swap if in the wrong order
      tmp <- lower
      lower <- upper
      upper <- tmp
    }

    range <- upper - lower
    incr <- range / steps
    #theta_list <- seq(lower, upper-incr, incr) # Lisa's way
    #dist_theta <- stats::dunif(theta_list, lower, upper) # inefficient but conceptually consistent
    theta_list <- seq(lower+incr, upper+incr, incr) # Zoltan's way
    dist_theta <- 1 / range
    heights <- stats::dt((sample_mean-theta_list)/sample_se, df=sample_df)

    likelihood <- sum(dist_theta * heights)*incr
  }

  return(likelihood)
}
