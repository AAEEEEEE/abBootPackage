#' Bootstrap Basic CI for A/B conversion rates
#'
#' This function computes the bootstrap *basic* confidence interval
#' for the difference in conversion rates between A and B.
#'
#' @param nA Sample size of group A.
#' @param yA Successes in group A.
#' @param nB Sample size of group B.
#' @param yB Successes in group B.
#' @param B Number of bootstrap replicates.
#' @param conf Confidence level (default 0.95).
#'
#' @return A list with:
#' \itemize{
#'   \item pA conversion rate in A
#'   \item pB conversion rate in B
#'   \item diff difference pB - pA
#'   \item ci basic bootstrap confidence interval
#' }
#'
#' @examples
#' boot_ab_basic(1000, 70, 1000, 95, B = 500)
#' @export
#'
boot_ab_basic <- function(nA, yA, nB, yB, B = 2000, conf = 0.95) {

  # basic error checks
  if (nA <= 0 || nB <= 0) stop("nA and nB must be > 0")
  if (yA < 0 || yA > nA || yB < 0 || yB > nB) stop("Invalid success counts")

  # observed stats
  pA_hat <- yA / nA
  pB_hat <- yB / nB
  diff_hat <- pB_hat - pA_hat

  # build 0/1 data
  dataA <- c(rep(1, yA), rep(0, nA - yA))
  dataB <- c(rep(1, yB), rep(0, nB - yB))

  # bootstrap sampling
  boot_diff <- numeric(B)
  for (b in seq_len(B)) {
    sampA <- sample(dataA, nA, replace = TRUE)
    sampB <- sample(dataB, nB, replace = TRUE)
    boot_diff[b] <- mean(sampA) - mean(sampB)
  }

  # basic CI
  alpha <- 1 - conf
  q <- stats::quantile(boot_diff, c(1 - alpha/2, alpha/2), names = FALSE)

  ci <- 2*diff_hat - q   # basic CI formula

  return(list(
    pA = pA_hat,
    pB = pB_hat,
    diff = diff_hat,
    ci = ci,
    method = "bootstrap_basic"
  ))
}
