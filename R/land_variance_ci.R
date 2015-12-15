#' Confidence intervals of individual landmarks
#'
#' Calculates confidence intervals of individual landmarks
#'
#' @param landmarks landmark array
#' @param iter number of iterations
#' @param interval confience interval
#' @export

landmarks.variance.ci <- function(landmarks,iter = 1000,interval = 0.95) {
    var.matrix <- NULL
    n.spec <- dim(landmarks)[3]
    n.land <- dim(landmarks)[1]
    var.matrix <- matrix(NA,ncol = iter,nrow = n.land)
    for (i in 1:iter) {
        smpl <- sample(1:n.spec,size = n.spec,replace = TRUE)
        tmp.l <- landmarks[,,smpl]
        var.matrix[,i] <- landmarks.variance(tmp.l)
    }
    q <- c((1-interval)/2,1-(1-interval)/2)
    ci <- apply(var.matrix,1,quantile,probs = q)
    return(t(ci))
}
