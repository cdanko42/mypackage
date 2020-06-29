#' Confidence interval program
#'
#' @param x @description A sample goes here
#' @param alpha @description The percentage for the confidence interval
#'
#' @return
#' @export
#'
#' @examples myci(iris)
myci <- function(x, alpha=.05){
  L = mean(x) - qt(1-(alpha/2), df=length(x)-1)*(sd(x)/sqrt(length(x)))
  U = mean(x) + qt(1-(alpha/2), df=length(x)-1)*(sd(x)/sqrt(length(x)))

  print(paste("(" , L, "," , U, ")"))
}
