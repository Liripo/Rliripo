#' @title rmb pal
#' @rdname scale_color_rmb
#' @export rmb_pal
rmb_pal <- function(money = "100",alpha = 1,reverse = TRUE){
  money <- match.arg(money,names(rmb))
  pal <- rmb[[money]]
  if (reverse){
    pal <- rev(pal)
  }
  return(colorRampPalette(pal, alpha))
}


#' @title Theme with rmb
#' @description rmb color
#' @author liripo
#' @param money rmb money character,It's can be set "100","50",..."1hairs"
#' @param discrete logical;whether discrete,the deault is T.
#' @param reverse logical
#' @param alpha alpha
#' @param ... see scale_color_gradientn
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @export scale_color_rmb
scale_color_rmb <- function(...,
                            discrete = TRUE,
                            money = "100",
                            alpha = 1,
                            reverse = TRUE){
  if (discrete){
    discrete_scale("color","rmb",
                   palette = rmb_pal(money = money,alpha = alpha,reverse = reverse),...)
  }else{
    scale_color_gradientn(...,
                          colours = rmb_pal(money = money,alpha = alpha, reverse = reverse)(256))
  }
}
#' @rdname scale_color_rmb
#' @export
scale_colour_rmb <- scale_color_rmb
#' @rdname scale_color_rmb
#' @importFrom ggplot2 scale_fill_gradientn
#' @export
scale_fill_rmb <- function(...,
                           discrete = TRUE,
                           money = "100",
                           alpha = 1,
                           reverse = TRUE){
  if (discrete){
    discrete_scale("fill","rmb",
                   palette = rmb_pal(money = money,alpha = alpha,reverse = reverse),...)
  }else{
    scale_fill_gradientn(...,
                         colours = rmb_pal(money = money,alpha = alpha, reverse = reverse)(256))
  }
}
