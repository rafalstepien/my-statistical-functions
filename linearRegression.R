data = iris

model=lm(Sepal.Length~.-Species, data=iris)
summary(model)

n=dim(iris)[1]
X=matrix(1,n,4)
X[,2] = iris[,2]
X[,3] = iris[,3]
X[,4] = iris[,4]

k = dim(iris)[2]-2

y = as.matrix(iris[,1])

alpha = solve(t(X)%*%X)%*%t(X)%*%y

y2 = X%*%alpha
residua = y -y2

S2e = sum(residua^2)/(n-k-1)

D = S2e*solve(t(X)%*%X)
Salpha = sqrt(diag(D))

testT = alpha/Salpha
(p_wartosc = 2*pt(abs(testT), n-k-1, lower.tail=FALSE))

R2=1 - sum(residua^2)/sum((y-mean(y))^2)
F=(R2/(1-R2))*(n-k-1)/(k)
(p_wartoscF = pf(F, k,n-k-1, lower.tail=FALSE))


linear_model = function(data, dependent_variable, ...) {
  # print("DEPENDENT")
  # cat(dependent_variable)
  # for (var in list(...)) {
  #   cat("\n\n INDEPENDENT", var)
  # }

  independents = list(...)
  nrows = length(dependent_variable)
  ncols = length(independents)
  X=matrix(1, nrows, ncols)
  for (i in 2:ncols){
    X[, i] <- data[, i]
  }
  print(X)

  
}

linear_model(data, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
