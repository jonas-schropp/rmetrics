#' Calculate Fowlkes–Mallows index
#'
#' The Fowlkes–Mallows index is the geometric mean of precision (PPV) and recall (TPR). It is generally used to compare the results of two clustering algorithms. It ranges from 0 to 1, with 1 indicating perfect classification.
#'
#'
#' @param tp Number of true positives in the contingency table.
#' @param fp Number of false positives in the contingency table.
#' @param fn Number of false negatives in the contingency table.
#'
#' @references
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983). "A Method for Comparing Two Hierarchical Clusterings". Journal of the American Statistical Association. 78 (383): 553. doi:10.2307/2288117
#'
#' @export
#'
calc_fmi <- function(otp, ofp, ofn) {

  ppv <- calc_precision(otp, ofp, F, 0)[1]
  tpr <- calc_tpr(otp, ofn, F, 0)[1]

  sqrt(ppv * tpr)

}



#' Calculate Fowlkes–Mallows index
#'
#' The Fowlkes–Mallows index is the geometric mean of precision (PPV) and recall (TPR). It is generally used to compare the results of two clustering algorithms. It ranges from 0 to 1, with 1 indicating perfect classification.
#'
#'
#' @param A1_clusters a numeric vector of cluster grouping (numeric) of items, with a name attribute of item name for each element from group A1. These are often obtained by using some k cut on a dendrogram.
#' @param A2_clusters a numeric vector of cluster grouping (numeric) of items, with a name attribute of item name for each element from group A2. These are often obtained by using some k cut on a dendrogram.
#' @param assume_sorted_vectors assume_sorted_vectors
#' @param warn warn
#'
#' @references
#' Fowlkes, E. B.; Mallows, C. L. (1 September 1983). "A Method for Comparing Two Hierarchical Clusterings". Journal of the American Statistical Association. 78 (383): 553. doi:10.2307/2288117
#' dendextend implementation for multiclass - compare!
#'
#' @export
#'
FM_index_R <- function (tbl) {

  n <- sum(tbl)
  Tk <- sum(tbl^2) - n
  ppos <- apply(tbl, 1, sum)
  pos <- apply(tbl, 2, sum)

  Pk <- sum(ppos^2) - n
  Qk <- sum(pos^2) - n
  FM <- Tk / sqrt(Pk * Qk)

  return(FM)
}
