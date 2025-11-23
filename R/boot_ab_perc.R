#' Bootstrap percentile CI for A/B conversion rates
#'
#' This function performs a bootstrap percentile confidence interval
#' for the difference in conversion rates between group A and group B.
#'
#' @param nA Integer. Sample size of group A.
#' @param yA Integer. Number of successes in group A.
#' @param nB Integer. Sample size of group B.
#' @param yB Integer. Number of successes in group B.
#' @param B Integer. Number of bootstrap replications.
#' @param conf Confidence level (default 0.95)
#'
#' @return A list containing:
#' \itemize{
#'   \item pA Estimated conversion rate in A
#'   \item pB Estimated conversion rate in B
#'   \item diff Difference (pB - pA)
#'   \item ci Percentile bootstrap CI
#' }
#'
#' @examples
#' boot_ab_perc(1000, 70, 1000, 95, B = 500)
#' @export
#'
boot_ab_perc <- function(nA, yA, nB, yB, B = 2000, conf = 0.95) {

  if (nA <= 0 || nB <= 0) stop("nA and nB must be > 0")
  if (yA < 0 || yA > nA || yB < 0 || yB > nB) stop("Invalid success counts")

  # conversion rates
  pA_hat <- yA / nA
  pB_hat <- yB / nB
  diff_hat <- pB_hat - pA_hat

  # turn into 0/1 vector
  dataA <- c(rep(1, yA), rep(0, nA - yA))
  dataB <- c(rep(1, yB), rep(0, nB - yB))

  # bootstrap loop
  boot_diff <- numeric(B)
  for (b in 1:B) {
    sampA <- sample(dataA, nA, replace = TRUE)
    sampB <- sample(dataB, nB, replace = TRUE)
    boot_diff[b] <- mean(sampA) - mean(sampB)
  }

  # percentile CI
  alpha <- 1 - conf
  ci <- stats::quantile(boot_diff, c(alpha/2, 1 - alpha/2), names = FALSE)

  return(list(
    pA = pA_hat,
    pB = pB_hat,
    diff = diff_hat,
    ci = ci,
    method = "bootstrap_percentile"
  ))
}
