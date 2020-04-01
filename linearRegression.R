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

unpack_names <- function(names_list){
  new_names_vect <- c()
  new_names_vect[1] <- "(Intercept)"
  for (i in 1:length(names_list)){
    new_names_vect[i+1] <- as.character(names_list[[i]])
  }
  new_names_vect
}

linear_model = function(data, dependent_variable, ...) {
  
  # ARGUMENTS CONFIG
  args <- as.list(match.call())
  args_n <- length(as.list(match.call()))
  independents <- unpack_names(args[4:args_n])
  
  nrows = length(dependent_variable)
  ncols = length(independents)-1
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
  p.val = 2*pt(abs(testT), nrows-ncols-1, lower.tail=FALSE)
  R2 = 1 - sum(res^2)/sum((y-mean(y))^2)
  F_stat = (R2/(1-R2)) * (nrows-ncols-1)/(ncols)
  F_stat_p.val = pf(F_stat, ncols, nrows-ncols-1, lower.tail=FALSE)
  names <-  as.vector(match.call(expand.dots = FALSE)$...)
  cat("\n\nResiduals:\n", summary(res))
  cat("\n\nCoefficients:\n")
  df <- data.frame(X=independents,
                   Estimate.Std=as.vector(alpha),
                   Std.Error=as.vector(Salpha),
                   t.value=as.vector(testT),
                   Pr=as.vector(p.val),
                   stars=get_stars(as.vector(p.val)))
  print(format(df, justify="centre", digit=2))
  cat("---\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
  cat("\nResidual standard error:", round(sqrt(S2e), 4),  "on", nrows-ncols-1,  "degrees of freedom\nMultiple R-squared::", round(R2, 4), "\nF-statistic:", F_stat, "on", ncols, "and", nrows-ncols-1, "DF, F-statistics p-value:", F_stat_p.val)
}

linear_model(data, dependent_variable=data$Sepal.Length, data$Sepal.Width, data$Petal.Length, data$Petal.Width)
model <- lm(data$Sepal.Length ~ data$Sepal.Width + data$Petal.Length + data$Petal.Width)
summary(model)
