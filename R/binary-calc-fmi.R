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
calc_fmi <- function(tp, fp, fn) {

  ppv <- calc_precision(tp, fp, F, 0)[1]
  tpr <- calc_tpr(tp, fn, F, 0)[1]

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
FM_index_R <- function (A1_clusters, A2_clusters, assume_sorted_vectors = FALSE,
          warn = dendextend_options("warn"), ...) {
  if (!assume_sorted_vectors) {
    sorted_As <- sort_2_clusters_vectors(A1_clusters, A2_clusters,
                                         assume_sorted_vectors = assume_sorted_vectors, warn = warn)
    A1_clusters <- sorted_As[[1]]
    A2_clusters <- sorted_As[[2]]
  }
  if (any(is.na(A1_clusters)) | any(is.na(A2_clusters))) {
    if (warn)
      warning("The clusterings have some NA's in them - returned NA.")
    FM_index <- NA
    attr(FM_index, "E_FM") <- NA
    attr(FM_index, "V_FM") <- NA
    return(FM_index)
  }
  M <- table(A1_clusters, A2_clusters)
  n <- length(A1_clusters)
  Tk <- sum(M^2) - n
  m_i. <- apply(M, 1, sum)
  m_.j <- apply(M, 2, sum)
  m_.. <- n
  if (sum(M) != n)
    stop("Why does M matrix doesn't sum up to n ??")
  Pk <- sum(m_i.^2) - n
  Qk <- sum(m_.j^2) - n
  FM <- Tk/sqrt(Pk * Qk)
  E_FM <- sqrt(Pk * Qk)/(n * (n - 1))
  Pk2 <- sum(m_i. * (m_i. - 1) * (m_i. - 2))
  Qk2 <- sum(m_.j * (m_.j - 1) * (m_.j - 2))
  V_FM <- 2/(n * (n - 1)) +
    4 * Pk2 * Qk2 /
    ((n * (n - 1) * (n - 2)) * Pk * Qk) +
    (Pk - 2 - 4 * Pk2/Pk) *
    (Qk - 2 - 4 * Qk2/Qk) /
    ((n * (n - 1) * (n - 2) * (n - 3))) -
    Pk * Qk/(n^2 * (n - 1)^2)
  # Includes the attributes E_FM and V_FM for the relevant expectancy and
  # variance under the null hypothesis of no-relation.
  FM_index <- FM
  attr(FM_index, "E_FM") <- E_FM
  attr(FM_index, "V_FM") <- V_FM
  return(FM_index)
}
