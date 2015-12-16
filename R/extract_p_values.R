#' Extract p-values
#'
#' Extract p-values from MANOVA list
#'
#' @param manova.lists MANOVA list
#' @export

extract.p.values <- function(manova.lists) {
    n.land <- length(manova.lists)
    p.vector <- NULL
    for (i in 1:n.land) {
        tmp <- summary(manova.lists[[i]])
        p <- tmp$stats[1,6]
        p.vector[i] <- p
    }
    return(p.vector)
}
