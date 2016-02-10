#' ANOVA test for all landmarks
#'
#' Calculates anova test for all landmarks
#'
#' @param group.1 first group
#' @param group.2 second group
#' @param iteracje iterations
#' @param quant alpha
#' @export

full.anova.test <- function(group.1,group.2,iteracje=1000,quant = 0.95) {
    n.landmarks <- dim(group.1)[1]
    output <- rep(FALSE,times = n.landmarks)
    for (i in 1:n.landmarks) {
        tmp.gr.1 <- group.1[i,,]
        tmp.gr.2 <- group.2[i,,]
        tmp.anova <- landmark.anova.test(tmp.gr.1,tmp.gr.2,iteracje = iteracje,quant = quant)
        if (tmp.anova$F.value > tmp.anova$f.quantiles) {
            output[i] <- TRUE
        } else {
            output[i] <- FALSE
        }
    }
    return(output)
}
