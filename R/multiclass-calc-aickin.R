#' Calculate Aickin's Alpha.
#'
#' @param ... `r rox("dots")`
#'
#' @source
#' 1. Aickin, M. (1990) Maximum Likelihood Estimation of Agreement in the Constant Predictive Probability Model, and Its Relation to Cohen's Kappa. Biometrics 46, 293-302.
#'
#' @author
#' Shamelessly stolen and only slightly adapted from Joseph, L. and Bélisle, P. <https://www.medicine.mcgill.ca/epidemiology/Joseph/PBelisle/Aickin-Alpha-Agreement-R.html>
#'
#' @export
#'
calc_aickin <- function(...) UseMethod("calc_aickin")



#' @describeIn calc_aickin
#'
#' @param tbl `r rox("tbl")`
#' @param d The agreement matrix for tbl. Must have the same dimensions as tbl, with 1 to indicate agreeing scores, and 0 disagreeing scores. By default the diagonal is considered agreeing.
#' @param epsilon Convergence criterion. The algorithm stops when two consecutive \eqn{\alpha} estimates differ by less than epsilon. Default is 1e-7.
#' @param ci.type Either FALSE if no confidence intervals are desired or 'aickin' if the default CI's by Aickin should be calculated. If FALSE overwrites ci.level. Default is to calculate the CI.
#' @param ci.level `r rox("ci.level")`
#' @param maxiter Integer or double. Maximum number of iterations to try until convergence. Default is 1000.
#'
#' @export
#'
calc_aickin.table <- function(tbl,
                              d = diag(1, nrow=nrow(tbl), ncol=ncol(tbl)),
                              epsilon = 1e-7,
                              ci.type = "aickin",
                              ci.level = 0.95,
                              maxiter = 1000,
                              ...)
{

  if (any(dim(tbl) != dim(d))) stop("tbl and d must be of equal dimensions.")
  if (diff(dim(tbl)) != 0) stop("tbl and d must be square matrices.")

  m <- nrow(tbl)          # Number of classes
  tbl <- tbl + 1 / (m^2)  # Continuity correction
  #J <- rep(1, m) needed for original method with %*%
  n <- sum(tbl)
  otp <- sum(tbl * d)       # OTP
  p0 <- otp / n             # OTP %
  rows.tot <- rowSums(tbl) # as.vector(tbl %*% matrix(J, ncol = 1))
  cols.tot <- colSums(tbl) # as.vector(matrix(J, nrow=1) %*% tbl)
  pr <- rows.tot / n
  pc <- cols.tot / n
  s <- sum(matrix(pc, nrow = m, ncol = m, byrow = T) * pr * d)
  alpha <- (p0 - s) / (1 - s)
  continue <- T
  niter <- 0

  while (continue) {

    previous.alpha <- alpha
    pr.denominator <- n * (
      1 - alpha + alpha * as.vector(
        d %*% matrix(pc, ncol=1)
        ) / s
      )
    pr <- rows.tot/pr.denominator
    pr[1] <- 1- sum(pr[-1])
    pc.denominator <- n * (
      1 - alpha + alpha * as.vector(
        matrix(pr, nrow=1) %*% d
        ) / s
      )
    pc <- cols.tot/pc.denominator
    pc[1] <- 1- sum(pc[-1])
    s <- sum(matrix(pc, nrow=m, ncol=m, byrow=T) * pr * d)
    alpha <- (p0 - s) / (1 - s)
    niter <- niter + 1

    if (niter < maxiter) {
      continue <- abs(alpha - previous.alpha) > epsilon
    } else {
      continue <- FALSE
    }
  }

  if (ci.type == "aickin") {

    prdiff <- pr[-1] - pr[1] # m-1 x 1
    pcdiff <- pc[-1] - pc[1] # m-1 x 1

    d2L.da2 <- - n * (1-s)/(1-alpha)/((1-alpha)*s+alpha)
    R <- alpha/s/((1-alpha)*s+alpha)
    U <- 1/alpha - 1
    d2L.dadpri <- - otp * pcdiff * ((n/otp)^2) # m-1 x 1
    d2L.dadpcj <- - otp * prdiff * ((n/otp)^2) # m-1 x 1

    d2L.dpridprj <- -sum(tbl[1,]) / (pr[1]^2) +
      otp * (2 * s * U + 1) * R * R *
      matrix(pcdiff, ncol = 1) %*%
      matrix(pcdiff, nrow = 1) # m-1 x m-1
    d2L.dpri2 <- -rows.tot[-1] / (pr[-1]^2) -
      rows.tot[1] / (pr[1]^2) +
      otp * (2 * s * U + 1) * R * R * (pcdiff^2)
    diag(d2L.dpridprj) <- d2L.dpri2

    d2L.dpridpcj <- -otp * R +
      otp * (2 * s * U + 1) * R * R *
      matrix(pcdiff, ncol = 1) %*%
      matrix(prdiff, nrow = 1) # m-1 x m-1
    d2L.dpridpci <- -2 * otp * R +
      otp * (2 * s * U + 1) * R * R * prdiff * pcdiff
    diag(d2L.dpridpcj) <- d2L.dpridpci

    d2L.dpcidpcj <- -sum(tbl[,1]) / (pc[1]^2) +
      otp * (2 * s * U + 1) * R * R *
      matrix(prdiff, ncol = 1) %*%
      matrix(prdiff, nrow = 1) # m-1 x m-1
    d2L.dpci2 <- -cols.tot[-1] / (pc[-1]^2) -
      cols.tot[1] / (pc[1]^2) +
      otp * (2 * s * U + 1) * R * R * (prdiff^2)
    diag(d2L.dpcidpcj) <- d2L.dpci2

    matrix.second.derivatives.top <- matrix(
      c(d2L.da2, d2L.dadpri, d2L.dadpcj), nrow = 1
    )
    matrix.second.derivatives.middle <- cbind(
      matrix(d2L.dadpri, ncol = 1), d2L.dpridprj, d2L.dpridpcj
    )
    matrix.second.derivatives.bottom <- cbind(
      matrix(d2L.dadpcj, ncol = 1), t(d2L.dpridpcj), d2L.dpcidpcj
    )
    matrix.second.derivatives <- rbind(
      matrix.second.derivatives.top,
      matrix.second.derivatives.middle,
      matrix.second.derivatives.bottom
    )

    parms.cov.matrix <- solve(-matrix.second.derivatives)
    alpha.sd <- sqrt(parms.cov.matrix[1])
    z <- qnorm((1 + ci.level)/2)
    ll <- alpha - z * alpha.sd
    ul <- alpha + z * alpha.sd

  } else {
    ll <- NA_real_
    ul <- NA_real_
  }

  res <- c(alpha, ll, ul)
  names(res) <- c("alpha", "ll", "ul")
  return(res)

}



#' @describeIn calc_aickin
#'
#' @param data `r rox("data")`
#' @param prediction `r rox("prediction")`
#' @param reference `r rox("reference")`
#' @param d The agreement matrix for tbl. Must have the same dimensions as tbl, with 1 to indicate agreeing scores, and 0 disagreeing scores. By default the diagonal is considered agreeing.
#' @param epsilon Convergence criterion. The algorithm stops when two consecutive \eqn{\alpha} estimates differ by less than epsilon. Default is 1e-7.
#' @param ci.type Either FALSE if no confidence intervals are desired or 'aickin' if the default CI's by Aickin should be calculated. If FALSE overwrites ci.level. Default is to calculate the CI.
#' @param ci.level `r rox("ci.level")`
#' @param maxiter Integer or double. Maximum number of iterations to try until convergence. Default is 1000.
#'
#' @export
#'
calc_aickin.data.frame <- function(
    data,
    prediction, reference,
    d = diag(1, nrow=nrow(tbl), ncol=ncol(tbl)),
    epsilon = 1e-7,
    ci.type = "aickin",
    ci.level = 0.95,
    maxiter = 1000,
    ...
) {

  data <- data[, c(prediction, reference)]
  tbl <- table(data)

  calc_aickin.table(tbl,
                    d, epsilon,
                    ci.type, ci.level,
                    maxiter)

}

