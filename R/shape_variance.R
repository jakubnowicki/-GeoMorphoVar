#' Shape variance
#'
#' Calculates shape variance
#'
#' @param landmarks set of landmark shapes
#' @import geomorph
#' @import morphoutils
#' @export

shape.variance <- function(landmarks) {
    mean.shape <- geomorph::mshape(landmarks)
    n.specimens <- dim(landmarks)[3]
    sum.dist <- NULL
    for (i in 1:n.specimens) {
        sum.dist[i] <- (morphoutils::fpdist(x = mean.shape,y = landmarks[,,i]))^2
    }
    variance <- sum(sum.dist)/(n.specimens-1)
    return(variance)
}
