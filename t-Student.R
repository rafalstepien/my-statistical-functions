is.float=function(x){
  if (abs(x) - abs(trunc(x)) == 0){
    return (FALSE)
  } else {
    return (TRUE)
  }
}

X_conditions <- function(X){
  if (!is.vector(X) | !is.numeric(X) | length(X) < 2) {
    print("X must be a numeric vector with length at least 2")
    return (FALSE)
  } else {
    return (TRUE)
  }
}

Y_conditions <- function(Y){
  if (!is.vector(Y) | !is.numeric(Y) | length(Y) < 2) {
    print("Y must be a numeric vector with length at least 2")
    return (FALSE)
  } else {
    return (TRUE)
  }
}

alpha_conditions <- function(alpha){
  if (!is.float(alpha) | alpha > 1 | alpha < 0) {
    print("Alpha must be a float number between 0 and 1")
    return (FALSE)
  } else {
    return (TRUE)
  }
}

mu0_conditions <- function(mu0){
  if (!is.numeric(mu0)){
    print("mu0 must be a number")
    return (FALSE)
  } else {
    return (TRUE)
  }
}

alternative_conditions <- function(alternative){
  if (!alternative %in% c(1, 2, 3)) {
    print("Alternative must be either 1, 2 or 3")
    return (FALSE)
  } else {
    return (TRUE)
  }
}

test1=function(X, alpha, mu0, alternative) {
    m=mean(X)
    s=sd(X)
    n=length(X)
    T=sqrt(n)*(m-mu0)/s
    if (alternative==1) {
      p=pt(T,n-1, lower.tail=F) 
    } else if (alternative==2) {
      p=pt(T,n-1) 
    } else {
      p=2*pt(abs(T),n-1, lower.tail=F) 
    }
    if (p>alpha) {
      dec=0
    } else {
      dec=1
    }
    list(stat=T, p_value=p, dec=dec)
}

test2=function(X, Y, alpha, alternative) {
  m1=mean(X)
  m2=mean(Y)
  v1=var(X)
  v2=var(Y)
  n1=length(X)
  n2=length(Y)
  T=(m1-m2)/sqrt((n1-1)*v1+(n2-1)*v2)*sqrt(n1*n2*(n1+n2-2)/(n1+n2))
  if (alternative==1) {
    p=pt(T,n1+n2-2, lower.tail=F) 
  } else if (alternative==2) {
    p=pt(T,n1+n2-2) 
  } else {
    p=2*pt(abs(T),n1+n2-2, lower.tail=F) 
  }
  if (p>alpha) {
    dec=0
  } else {
    dec=1
  }
  list(stat=T, p_value=p, dec=dec)
}

test3=function(X, Y, alpha) {
  D=X-Y
  m=mean(D)
  s=sd(D)
  n=length(D)
  T=sqrt(n)*(m)/s
  p=pt(T,n-1, lower.tail=T) 
  
  if (p>alpha) {
    dec=0
  } else {
    dec=1
  }
  list(stat=T, p_value=p, dec=dec)
}


my.ttest=function(X, Y, mu0, alternative, alpha) {
  stopifnot(X_conditions(X))
  stopifnot(alpha_conditions(alpha))
  
  arguments <- as.list(sys.call())
  
  if (is.null(arguments$Y)) {
    stopifnot(mu0_conditions(mu0))
    stopifnot(alternative_conditions(alternative))
    print("RUNNING TEST FOR ONE SAMPLE")
    test1(X, alpha, mu0, alternative)
  
    } else if (is.null(arguments$alternative)) {
    stopifnot(length(X) == length(Y))
    print("RUNNING TEST FOR INDEPENDENT SAMPLES")
    test3(X, Y, alpha)
    
    } else {
    print("RUNNING TEST FOR DEPENDENT SAMPLES")
    test2(X, Y, alpha, alternative)
    }
}


