#' Shape variance confidence intervals
#'
#' Calculates shape variance confidence intervals
#'
#' @param landmarks landmark array
#' @param iter number of iterations
#' @param interval confience interval
#' @export

shape.variance.ci <- function(landmarks,iter = 1000,interval = 0.95) {
    var.vector <- NULL
    n.spec <- dim(landmarks)[3]
    for (i in 1:iter) {
        smpl <- sample(1:n.spec,size = n.spec,replace = TRUE)
        tmp.l <- landmarks[,,smpl]
        var.vector[i] <- shape.variance(tmp.l)
    }
    q <- c((1-interval)/2,1-(1-interval)/2)
    ci <- quantile(var.vector,probs = q)
    return(ci)
}
