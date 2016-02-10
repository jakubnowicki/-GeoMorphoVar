#' Landmark ANOVA test
#'
#' Calculates distribution for single landmark ANOVA
#'
#' @param group.1 first group
#' @param group.2 second group
#' @param iteracje iterations
#' @param quant alpha
#' @export

landmark.anova.test <- function(group.1,group.2,iteracje=1000,quant = 0.95) {
    F.value <- landmark.anova(group.1, group.2)
    f.distribution <- NULL
    group.1 <- t(group.1)
    group.2 <- t(group.2)
    data <- rbind(group.1,group.2)
    n.1 <- dim(group.1)[1]
    n.2 <- dim(group.2)[1]
    n <- n.1 + n.2
    for (i in 1:iteracje) {
        tmp.gr.1 <- t(data[sample(1:n,size = n.1,replace = T),])
        tmp.gr.2 <- t(data[sample(1:n,size = n.2,replace = T),])
        tmp.f <- landmark.anova(tmp.gr.1,tmp.gr.2)
        f.distribution <- c(f.distribution,tmp.f)
    }
    qnt <- quantile(f.distribution, probs = quant)
    return(list(F.value = F.value, f.distribution = f.distribution, f.quantiles = qnt))
}
