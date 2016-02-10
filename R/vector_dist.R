#' Pitagorean distance
#'
#' Pitagorean distance
#'
#' @param x first point
#' @param y second point
#' @export

vector.dist <- function(x,y) {
    sqrt(sum((x-y)^2))
}
