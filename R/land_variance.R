#' Individual variance of landmarks
#'
#' Calculates individual variance of landmarks
#'
#' @param landmarks landmarks array
#' @export

landmarks.variance <- function(landmarks) {
    mean.shape <- geomorph::mshape(landmarks)
    n.spec <- dim(landmarks)[3]
    n.landmarks <- dim(landmarks)[1]
    dist.matrix <- matrix(0,ncol = n.landmarks,nrow = n.spec)
    for (i in 1:n.spec) {
        dist.matrix[i,] <- land.dist(mean.shape,landmarks[,,i])
    }
    variance <- apply(dist.matrix,2,var)
    return(variance)
}
