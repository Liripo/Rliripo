#' This is the function for dna to protein
#' @author liripo
#' @param dna vector of dna chr
#' @importFrom stringr str_sub
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble

dna_protein <- function(dna){
  dna <- toupper(dna)
  n <- nchar(dna)
  if (n %% 3 != 0 )stop("The dna length is not Multiple of 3!!!",call. = F)
  seq1 <- seq(1,n,by = 3)
  seq2 <- seq(3,n,by = 3)
  dna_vector <- stringr::str_sub(dna,start = seq1,end = seq2)
  pro_vector <- purrr::map_chr(dna_vector,function(x){
    if(x %in% dna_pro_table$dna){
      index <- which(x == dna_pro_table$dna)
      pro <- dna_pro_table$pro[index]
    }else{
      stop("DNA SEQ should be ATCG combination.")
    }
  })
  pro_vector
}

#' This is the function for dna to protein
#' @export dna_to_protein
#' @author liripo
#' @examples
#' dna_to_protein(c("ATG","GCC"))
dna_to_protein <- Vectorize(dna_protein)
