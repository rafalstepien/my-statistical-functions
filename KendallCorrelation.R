concordant <- function(ranks.df){
  C_sum <- 0
  for (i in 1:length(ranks.df$a)) {
    C_sum <- C_sum + sum(ranks.df$a[i] < ranks.df$b[i+1:length(ranks.df$b)], na.rm = TRUE)
  }
  C_sum
}

discordant <- function(ranks.df){
  D_sum <- 0
  for (i in 1:length(ranks.df$b)) {
    D_sum <- D_sum + sum(ranks.df$a[i] >= ranks.df$b[i+1:length(ranks.df$b)], na.rm = TRUE)
  }
  D_sum
}

get.ranks <- function(x, y){
  a <- rank(x)
  b <- rank(y)
  tmp.df <- data.frame(a, b)
  tmp.df <- tmp.df[order(tmp.df$a), ]
  tmp.df
}

kendall=function(x, y){
  ranks.df <- get.ranks(x, y)
  C <- concordant(ranks.df)
  D <- discordant(ranks.df)
  result <- (C-D)/(C+D)
  print(result)
}

