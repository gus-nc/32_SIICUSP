# after checking the goodness of fit in the script 'sp50_fitting_goodness_curve.r' this does *much* better than the curve
sp50_linear <- function(dat, level = 0.5, method = "proportion"){
  dat <- rev(sort(dat))
  if (method == "percent") {
    dat <- dat/100
  }
  else if (method == "proportion"){
  }
  else { stop("choose appropriate method")}
  d2 <- c(0, dat[1])
  for(i in 2:length(dat)){
    d2 <- c(d2, sum(dat[1:i]))
  }
  df <- data.frame(x=0:length(dat), y=d2)
  sides <- df[c(which(df$y == max(df$y[which(df$y < level)])), which(df$y > level)[1]),]
  fit <- lm(y ~ x, sides)
  sp50 <- (level-coef(fit)[[1]])/coef(fit)[[2]]
  return(sp50)
}
