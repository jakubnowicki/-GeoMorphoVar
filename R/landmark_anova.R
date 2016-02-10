#' Landmark ANOVA
#'
#' Calculates ANOVA for single landmark
#'
#' @param group.1 first group
#' @param group.2 second group
#' @export

landmark.anova <- function(group.1,group.2) {
    group.1 <- t(group.1)
    group.2 <- t(group.2)
    n.1 <- dim(group.1)[1]
    n.2 <- dim(group.2)[1]
    n <- n.1 + n.2
    data <- rbind(group.1,group.2)
    m.landmark.all <- apply(data,2,mean)
    m.landmark.1 <- apply(group.1,2,mean)
    m.landmark.2 <- apply(group.2,2,mean)
    SS.m <- ((vector.dist(m.landmark.1,m.landmark.all))^2)*n.1 + ((vector.dist(m.landmark.2,m.landmark.all))^2)*n.2
    SS.c <- 0
    for (i in 1:n) {
        tmp <- (vector.dist(data[i,],m.landmark.all))^2
        SS.c <- sum(SS.c,tmp)
    }
    SS.w <- SS.c - SS.m
    df.m <- 1
    df.w <- n - 2
    df.c <- n - 1
    MS.m <- SS.m/df.m
    MS.w <- SS.w/df.w
    F.value <- MS.m/MS.w
    return(F.value)
}
