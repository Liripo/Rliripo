#' @title show your colors by ggplot2
#'
#' @param colors colors
#' @param ncol ncol
#' @importFrom ggplot2 ggplot geom_tile aes aes_
#' @importFrom ggplot2 geom_text scale_fill_manual theme_void
#' @export
show_color <- function(colors,ncol = NULL){
  n <- length(colors)
  ncol <- ncol %||% ceiling(sqrt(length(colors)))
  nrow <- ceiling(n/ncol)
  tbl <- data.frame(cols = rep(1:ncol,times = nrow),
    rows = rep(nrow:1,each = ncol),
    colors = c(colors,rep(NA,ncol*nrow-length(colors))),stringsAsFactors = F
  )
  tbl <- na.omit(tbl)
  p <- ggplot(tbl,aes_(~cols,y =~rows))+
    geom_tile(aes_(fill = ~colors),color = "black",show.legend = F) +
    geom_text(aes_(label = ~colors)) +
    scale_fill_manual(values = tbl$colors,breaks = tbl$colors) +
    theme_void()
  class <- attributes(p)$class
  attr(p,"class") <- c(class,"show_color")
  p
}
#' show color S3
#' @rdname print
#' @export
#' @method print show_color
print.show_color <- function(x,...){
  print(x,...)
}

