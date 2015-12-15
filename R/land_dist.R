#' Distance between landmarks
#'
#' Calculates distance between landmarks in two shapes.
#'
#' @param x shape 1
#' @param y shape 2
#' @export

land.dist <- function(x,y) {
    sqrt(apply((x-y)^2, 1, sum))
    }
