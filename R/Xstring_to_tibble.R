#' NULL
to_string_split <- function(i,fasta,align = T,max = NULL){
  bstring <- fasta[i]
  name <- names(bstring)
  chr <- bstring %>% Biostrings::toString() %>%
    stringr::str_split("") %>% .[[1]]
  if(!align){
    make <- rep(NA,(max - length(chr)))
    chr <- c(chr,make)
  }
  tbl <- tibble::tibble(name = chr)
  names(tbl) <- name
  tbl
}
#' @title XStringset objects to tibble
#' @description Biostrings XStringset objects and else objects to tibble
#' @importFrom Biostrings toString
#' @importFrom stringr str_split
#' @importFrom purrr map_dfc
#' @export to_tibble
to_tibble <- function(fasta){
  widths <- Biostrings::width(fasta)
  max <- max(widths)
  idx <- which(widths != max)
  if(length(idx) != 0){
    warning("The length is inconsistent, determine whether it is an aligned sequence, and the insufficient length is filled with NA\n--By Liripo",
      call. = FALSE)
    purrr::map_dfc(seq_along(fasta),to_string_split,fasta = fasta,align = F,max = max)
  }else{
    purrr::map_dfc(seq_along(fasta),to_string_split,fasta = fasta)
  }
}

