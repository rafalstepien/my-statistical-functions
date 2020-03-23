data = iris[1:4]

get_stars <- function(p.val.vector){
  stars_vector <- c()
  for (i in 1:length(p.val.vector)){
    if (p.val.vector[i] > 0.1){
      stars_vector[i] <- ""
    } else if (p.val.vector[i] > 0.05 & p.val.vector[i] < 0.1){
      stars_vector[i] <- "."
    } else if (p.val.vector[i] > 0.01 & p.val.vector[i] < 0.05){
      stars_vector[i] <- "*"
    } else if (p.val.vector[i] > 0.001 & p.val.vector[i] < 0.01){
      stars_vector[i] <- "**"
    } else if (p.val.vector[i] < 0.001){
      stars_vector[i] <- "***"
    }
  }
  stars_vector
}

linear_model = function(data, dependent_variable, ...) {
  independents = list(...)
  nrows = length(dependent_variable)
  ncols = length(independents)
  X=matrix(1, nrows, ncols+1)
  for (i in 1:ncols+1){
    X[, i] <- data[, i]
  }
  y = as.matrix(dependent_variable)
  alpha = solve(t(X)%*%X)%*%t(X)%*%y
  y2 = X%*%alpha
  res = y - y2
  S2e = sum(res^2)/(nrows-ncols-1)
  D = S2e*solve(t(X)%*%X)
  Salpha = sqrt(diag(D))
  testT = alpha/Salpha
  print(testT)
  p.val = 2*pt(abs(testT), nrows-ncols-1, lower.tail=FALSE)
  print(p.val)
  R2 = 1 - sum(res^2)/sum((y-mean(y))^2)
  F_stat = (R2/(1-R2)) * (nrows-ncols-1)/(ncols)
  F_stat_p.val = pf(F, ncols, nrows-ncols-1, lower.tail=FALSE)
  names <-  as.vector(match.call(expand.dots = FALSE)$...)
  cat("Call:\nformula = ", deparse(substitute(dependent_variable)), "~", names[[1]], "+",  names[[2]], "+", names[[3]])
  cat("\n\nResiduals:\n", summary(res))
  cat("\n\nCoefficients:\n")
  df <- data.frame(X=c("(Intercept)", as.character(names[[1]]), as.character(names[[2]]), as.character(names[[3]])),
                   Estimate.Std=as.vector(alpha),
                   Std.Error=as.vector(Salpha),
                   t.value=as.vector(testT),
                   Pr=as.vector(p.val),
                   stars=get_stars(as.vector(p.val)))
  format(df, justify="left", digit=2)
}

linear_model(data, dependent_variable=Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width)
summary(model)
