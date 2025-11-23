#' Bootstrap-t CI for A/B conversion rates
#'
#' This function computes the *bootstrap-t* confidence interval
#' for the difference in conversion rates between A and B.
#'
#' @param nA Sample size of group A.
#' @param yA Successes in group A.
#' @param nB Sample size of group B.
#' @param yB Successes in group B.
#' @param B Number of bootstrap replicates.
#' @param conf Confidence level (default 0.95).
#'
#' @return A list containing:
#' \itemize{
#'   \item pA conversion rate for group A
#'   \item pB conversion rate for group B
#'   \item diff difference pB - pA
#'   \item ci bootstrap-t confidence interval
#' }
#'
#' @examples
#' boot_ab_t(1000, 70, 1000, 95, B = 500)
#' @export
#'
boot_ab_t <- function(nA, yA, nB, yB, B = 2000, conf = 0.95) {

  # observed stats
  pA_hat <- yA / nA
  pB_hat <- yB / nB
  diff_hat <- pB_hat - pA_hat

  # shortcut SE formula for proportions
  se_hat <- sqrt(pA_hat*(1-pA_hat)/nA + pB_hat*(1-pB_hat)/nB)

  # build raw 0/1 data
  dataA <- c(rep(1, yA), rep(0, nA - yA))
  dataB <- c(rep(1, yB), rep(0, nB - yB))

  # store bootstrap t-values
  t_star <- numeric(B)

  for (b in seq_len(B)) {
    sampA <- sample(dataA, nA, replace = TRUE)
    sampB <- sample(dataB, nB, replace = TRUE)

    diff_star <- mean(sampA) - mean(sampB)

    # compute SE for the bootstrap sample
    se_star <- sqrt(mean(sampA)*(1-mean(sampA))/nA +
                      mean(sampB)*(1-mean(sampB))/nB)

    t_star[b] <- (diff_star - diff_hat) / se_star
  }

  alpha <- 1 - conf
  q <- stats::quantile(t_star, c(1 - alpha/2, alpha/2))

  # bootstrap-t CI formula
  ci <- diff_hat - q * se_hat

  return(list(
    pA = pA_hat,
    pB = pB_hat,
    diff = diff_hat,
    ci = ci,
    method = "bootstrap_t"
  ))
}
