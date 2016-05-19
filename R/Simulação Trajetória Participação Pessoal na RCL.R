library(magrittr)

baseline <- c(dtp = 31998010464, rcl = 47333828612)

participacao_pessoal_rcl(baseline, indice = 0.441, n = 10, dtp = 0.054) %>% p("%")
participacao_pessoal_rcl(baseline, indice = 0.441, n = 10, rcl = 0.1) %>% p("%")
participacao_pessoal_rcl(baseline, n = 10, dtp = 0.054, rcl = 0.1) %>% p("%")
participacao_pessoal_rcl(baseline, indice = 0.441, dtp = 0.054, rcl = 0.1)


participacao_pessoal_rcl <- function(baseline, indice = NULL, n = NULL, dtp = NULL, rcl = NULL) {
  if(is.null(indice) & !is.null(n) & !is.null(dtp) & !is.null(rcl)) {
    return(solve_indice(n, dtp, rcl))
  }
  
  if(!is.null(indice) & is.null(n) & !is.null(dtp) & !is.null(rcl)) {
    return(solve_n(indice, dtp, rcl))
  }
  
  if(!is.null(indice) & !is.null(n) & is.null(dtp) & !is.null(rcl)) {
    return(solve_dtp(indice, n, rcl))
  }
  
  if(!is.null(indice) & !is.null(n) & !is.null(dtp) & is.null(rcl)) {
    return(solve_rcl(indice, n, dtp))
  }
  stop("Somente um dos valores deve ser NULL.")
}


solve_indice <- function(n, dtp, rcl) {
  numerator <- baseline["dtp"] * (1+dtp)^n
  denominator <- baseline["rcl"] * (1+rcl)^n
  indice <- numerator / denominator
  as.numeric(indice)
}

solve_n <- function(indice, dtp, rcl) {
  numerator <- log((indice * baseline["rcl"]) / baseline["dtp"])
  denominator <- log(1+dtp) - log(1+rcl)
  n <- numerator / denominator
  as.numeric(n)
}

solve_dtp <- function(indice, n, rcl) {
  base <- (baseline["rcl"] * (1+rcl)^n) * indice / baseline["dtp"]
  exponent <- 1/n
  dtp <- base^exponent-1
  as.numeric(dtp)
}

solve_rcl <- function(indice, n, dtp) {
  base <- ((baseline["dtp"]*(1+dtp)^n) / indice) / baseline["rcl"]
  exponent <- 1/n
  rcl <- base^exponent-1
  as.numeric(rcl)
}


