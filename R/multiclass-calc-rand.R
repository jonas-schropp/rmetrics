
#' Calculate Rand index (rand).
#'
#' @param prediction prediction
#' @param reference reference
#'
#' @export
#'
calc_rand <- function(prediction, reference) {

  x <- vapply(prediction, FUN = function(x) x != prediction, integer(length(prediction)))
  y <- vapply(reference, FUN = function(x) x != reference, integer(length(reference)))

  sg <- sum(abs(x - y)) / 2
  bc <- choose(dim(x)[1], 2)

  1 - sg / bc
}
