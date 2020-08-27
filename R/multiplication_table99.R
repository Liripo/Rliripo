#' This the 99 multiplication table
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom ggplot2 guides aes_string
#' @importFrom rlang .data
#' @export print_99
#' @importFrom ggplot2 guides
print_99 <- function(money = "100"){
  table <- expand.grid(a = 1:9,b = 1:9) %>%
    filter(quote(a) <= quote(b))
  p <- ggplot(table,aes_(~a,9 - ~b)) +
    geom_tile(aes_(fill = as.factor(~a*~b)),color = "black")+
    geom_text(aes_(label = num_to_chinese_label(~a,~b)))
  p+scale_fill_rmb(money = money) +theme_void() + guides()
}
#'
num_to_chinese <- function(num){
  chinese <- "一二三四五六七八九十"
  chinese <- str_split(chinese,"")[[1]]
  if(num < 10)last <- chinese[[num]]
  else{last <- paste0(chinese[[num%/%10]],"十",
    if(num%%10 !=0){
    chinese[[num%%10]]
    })
  }
  last
}

num_to_chinese_label <- function(num_1,num_2){
  num_1_chinese <- num_to_chinese(num_1)
  num_2_chinese <- num_to_chinese(num_2)
  product <- num_1 * num_2
  product_chinese <- num_to_chinese(product)
  last <- glue("{num_1} x {num_2} = {product}\n",
    "{num_1_chinese}{num_2_chinese}",
    "{ifelse(product < 10,'得','')}{product_chinese}")
  last
}

num_to_chinese_label <- Vectorize(num_to_chinese_label)
