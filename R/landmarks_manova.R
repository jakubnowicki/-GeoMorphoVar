#' MANOVA for single landmarks
#'
#' Calculate manova for single landmarks.
#'
#' @param data data
#' @param takson taxon factor
#' @param centroid.size centroid size
#' @export

landmarks.manova <- function(data,takson=NULL,centroid.size=NULL) {
    n.land <- dim(data)[1]
    output <- list()
    if (is.null(takson) & !is.null(centroid.size)) {
        for (i in 1: n.land) {
            tmp <- t(data[i,,])
            output[[i]] <- manova(tmp ~ centroid.size)
        }
    } else {
        if (!is.null(takson) & is.null(centroid.size)) {
            for (i in 1: n.land) {
                tmp <- t(data[i,,])
                output[[i]] <- manova(tmp ~ takson)
            }
        } else {
            for (i in 1: n.land) {
                tmp <- t(data[i,,])
                output[[i]] <- manova(tmp ~ takson + centroid.size)
            }
        }
    }

    return(output)
}
