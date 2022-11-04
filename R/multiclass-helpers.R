#' Aggregates data for multiclass confusion matrix for one target
#'
#' @param data Data frame of prediction and reference
#' @param target The target class
#'
#' @noRd
#' @keywords Internal
aggregate_multiclass_cm <- function(data, target) {

  res <- data

  res[,1] <- data[,1] == target
  res[,2] <- data[,2] == target

  res

}

#' Calculate the combination of n and r.
#'
#' @param n n
#' @param r r
#'
#' @noRd
#' @keywords Internal
#'
ncr <- function(n, r) {

  if (r > n) return(0)
  r = min(r, n - r)
  floor(prod(seq(n, n - r, -1)) / prod(seq(1, r + 1)))

}



#' Calculate Reference and Response likelihood.
#'
#' @param item number of positives in actual (pos) or predict vector (ppos) per class
#' @param n number of observations per class
#'
#' @noRd
#' @keywords Internal
#'
calc_entropy <- function(item, n) {

  lik <- item / n
  lik[lik != 0] <- lik[lik != 0] * log(lik[lik != 0], 2)

  -sum(lik)

}



#' Calculate Variance for symmetric Goodman Kruskal Lambda.
#'
#'
#' @param tbl input table
#' @param n n
#' @param max.csum max.csum
#' @param max.rsum max.rsum
#' @param csum csum
#' @param rsum rsum
#' @param nr nr
#' @param nc nc
#' @param cmax cmax
#' @param rmax rmax
#'
#'
#' @noRd
#' @keywords Internal
#'
lambda.var.symmetric <- function(tbl, n,
                                 max.csum, max.rsum,
                                 csum, rsum,
                                 nr, nc,
                                 cmax, rmax) {

  L.col <- matrix(NA, ncol(tbl))
  L.row <- matrix(NA, nrow(tbl))

  l <- which.max(csum)
  k <- which.max(rsum)
  li <- apply(tbl, 1, which.max)
  ki <- apply(tbl, 2, which.max)
  w <- 2 * n - max.csum - max.rsum
  v <- 2 * n - sum(rmax, cmax)
  xx <- sum(rmax[li == l], cmax[ki == k], rmax[k],
            cmax[l])
  y <- 8 * n - w - v - 2 * xx
  t <- rep(NA, length(li))

  for (i in 1:length(li)) {
    t[i] <- (ki[li[i]] == i & li[ki[li[i]]] == li[i])
  }

  sigma2 <- 1/w^4 * (w * v * y - 2 * w^2 * (n - sum(rmax[t])) -
                       2 * v^2 * (n - tbl[k, l]))
  sigma2
}



#' Calculate Variance for asymmetric Goodman Kruskal Lambda (column).
#'
#'
#' @param tbl input table
#' @param n n
#' @param max.csum max.csum
#' @param max.rsum max.rsum
#' @param csum csum
#' @param rsum rsum
#' @param nr nr
#' @param nc nc
#' @param cmax cmax
#' @param rmax rmax
#'
#'
#' @noRd
#' @keywords Internal
#'
lambda.var.column <- function(tbl, n,
                              max.csum, max.rsum,
                              csum, rsum,
                              nr, nc,
                              cmax, rmax) {

  L.col <- matrix(NA, ncol(tbl))
  L.row <- matrix(NA, nrow(tbl))

  L.col.max <- min(which(csum == max.csum))
  for (i in 1:nr) {

    l <- length(
      which(tbl[i, intersect(
        which(tbl[i, ] == max.csum),
        which(tbl[i, ] == max.rsum))] == n))

    if (l > 0) {
      L.col[i] <- min(
        which(tbl[i,
                  intersect(which(tbl[i, ] == max.csum),
                            which(tbl[i, ] == max.rsum))] == n)
      )
    } else if (tbl[i, L.col.max] == max.csum) {
      L.col[i] <- L.col.max
    } else {
      L.col[i] <- min(which(tbl[i, ] == rmax[i]))
    }
  }

  (n - sum(rmax)) *
    (sum(rmax) + max.csum - 2 * (sum(rmax[which(L.col == L.col.max)])))/
    (n - max.csum)^3

}



#' Calculate Variance for asymmetric Goodman Kruskal Lambda (row).
#'
#'
#' @param tbl input table
#' @param n n
#' @param max.csum max.csum
#' @param max.rsum max.rsum
#' @param csum csum
#' @param rsum rsum
#' @param nr nr
#' @param nc nc
#' @param cmax cmax
#' @param rmax rmax
#'
#'
#' @noRd
#' @keywords Internal
#'
lambda.var.row <- function(tbl, n,
                           max.csum, max.rsum,
                           csum, rsum,
                           nr, nc,
                           cmax, rmax) {

  L.col <- matrix(NA, ncol(tbl))
  L.row <- matrix(NA, nrow(tbl))
  L.row.max <- min(which(rsum == max.rsum))

  for (i in 1:nc) {

    l <- length(which(tbl[intersect(which(tbl[, i] == max.rsum),
                                    which(tbl[, i] == max.csum)), i] == n))

    if (l > 0) {
      L.row[i] <- min(
        which(tbl[i, intersect(which(tbl[i, ] == max.csum),
                               which(tbl[i, ] == max.rsum))] == n)
      )
    } else if (tbl[L.row.max, i] == max.rsum) {
      L.row[i] <- L.row.max
    } else {
      L.row[i] <- min(which(tbl[, i] == cmax[i]))
    }
  }

  (n - sum(cmax)) *
    (sum(cmax) + max.rsum - 2 * (sum(cmax[which(L.row == L.row.max)]))) /
    (n - max.rsum)^3

}



#' Lochi.
#'
#' @param chival chival
#' @param df df
#' @param conf conf
#'
#' @importFrom stats pchisq
#'
#' @noRd
#' @keywords Internal
#'
lochi <- function(chival, df, conf) {
  if (chival == 0) return(NA)
  ulim <- 1 - (1 - conf)/2
  lc <- c(0.001, chival/2, chival)
  while (pchisq(chival, df, lc[1]) < ulim) {
    if (pchisq(chival, df) < ulim)
      return(c(0, pchisq(chival, df)))
    lc <- c(lc[1]/4, lc[1], lc[3])
  }
  diff <- 1
  while (diff > 1e-05) {
    if (pchisq(chival, df, lc[2]) < ulim)
      lc <- c(lc[1], (lc[1] + lc[2])/2, lc[2])
    else lc <- c(lc[2], (lc[2] + lc[3])/2, lc[3])
    diff <- abs(pchisq(chival, df, lc[2]) - ulim)
    ucdf <- pchisq(chival, df, lc[2])
  }
  c(lc[2], ucdf)
}



#' Hichi.
#'
#' @param chival chival
#' @param df df
#' @param conf conf
#'
#' @importFrom stats pchisq
#'
#' @noRd
#' @keywords Internal
#'
hichi <- function(chival, df, conf) {
  if (chival == 0)
    return(NA)
  uc <- c(chival, 2 * chival, 3 * chival)
  llim <- (1 - conf)/2
  while (pchisq(chival, df, uc[1]) < llim) {
    uc <- c(uc[1]/4, uc[1], uc[3])
  }
  while (pchisq(chival, df, uc[3]) > llim) {
    uc <- c(uc[1], uc[3], uc[3] + chival)
  }
  diff <- 1
  while (diff > 1e-05) {
    if (pchisq(chival, df, uc[2]) < llim)
      uc <- c(uc[1], (uc[1] + uc[2])/2, uc[2])
    else uc <- c(uc[2], (uc[2] + uc[3])/2, uc[3])
    diff <- abs(pchisq(chival, df, uc[2]) - llim)
    lcdf <- pchisq(chival, df, uc[2])
  }
  c(uc[2], lcdf)
}



#' Calculate TPR, TNR, PPV, FNR, FPR, or F1 micro.
#'
#' @param item1 item1 in micro averaging
#' @param item2 item2 in micro averaging
#'
#' @noRd
#' @keywords Internal
#'
calc_micro <- function(item1, item2) {

  sum(item1) / (sum(item1) + sum(item2))

}



#' Calculate PPV_Macro and TPR_Macro.
#'
#' @param item True positive rate (TPR) or Positive predictive value (PPV)
#'
#' @noRd
#' @keywords Internal
#'
calc_macro <- function(item) {

  sum(item) / length(item)

}



#' Calculate Percent chance agreement for Gwets AC1.
#'
#' @param pos vector of actual positives per class
#' @param ppos vector of positives in predict vector per class
#' @param n Total number of observations
#'
#' @noRd
#' @keywords Internal
#'
calc_pc_ac1 <- function(pos, ppos, n) {

  pi <- ((pos + ppos) / (2 * n))
  res <- sum(pi * (1 - pi))

  res / (length(pos) - 1)

}



#' Calculate Percent chance agreement for Bennett-et-al.s-S-score.
#'
#' @param tbl input table
#'
#' @noRd
#' @keywords Internal
#'
calc_pc_s <- function(tbl) {

  1 / (ncol(tbl))

}
