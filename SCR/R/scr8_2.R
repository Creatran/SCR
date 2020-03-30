#' scr8_2
#' @description
#' The solution to the exercise 8_2 for Rizzo's Book.
#'
#'Implement the bivariate Spearman rank correlation test for independence [255] as a permutation test. The Spearman rank correlation test statistic can be obtained from function cor with method = "spearman". Compare the achieved significance level of the permutation test with the p-value reported by cor.test on the same samples.
#'
#' @references Maria L. Rizzo. (2016). Statistical computing with rep.cor.
#'
#' @param rep number of replicates.  Defaults to 1000
#'
#' @examples scr8_2(rep=1000)
#' @export
#'
scr8_2<-function(rep=1000){
  data(chickwts)
  soybean = chickwts$weight[chickwts$feed=="soybean"]
  linseed = chickwts$weight[chickwts$feed=="linseed"]
  n1 = length(soybean)
  n2 = length(linseed)
  n = min(n1, n2)
  soybean = sort(soybean[1:n])
  linseed = sort(linseed[1:n])
  zs = c(soybean, linseed)
  cor= cor.test(x = soybean, y = linseed, method = "spearman")
  k = length(zs)
  rep.cor = numeric(rep)
  for (rep in 1:rep) {
    i = sample(1:k, k/2, replace = FALSE)
    xs = zs[i]
    ys = zs[-i]
    rep.cor[rep] = cor(x = xs, y = ys, method = "spearman")
  }
  hist(rep.cor)
  if (cor$p.value >= 0.001)
    p.txt <- cor$p.value
  else
    p.txt <- '< 0.001'
  if (mean(abs(rep.cor) > abs(cor$estimate)) >= 0.001)
    p.txt1 <- mean(abs(rep.cor) > abs(cor$estimate))
  else
    p.txt1 <- '<0.001'
  print(list( p.value=p.txt, p.hat = p.txt1))
}

