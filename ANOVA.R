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



# ONE WAY ANOVA
one_way_anova <- function(data, dependent_var, independent_var){
  
  # CREATE HELPER DATAFRAME
  helper_dataframe <- cbind(data[dependent_var], c(data[independent_var]))
  helper_dataframe[, 2] <- as.factor(helper_dataframe[, 2])
  
  n <- length(helper_dataframe[, 2])
  k <- length(unique(helper_dataframe[, 2]))


  # MEANS
  srY = mean(helper_dataframe[, 1])
  srG = aggregate(helper_dataframe[, 1], by = list(helper_dataframe[, 2]), FUN = "mean")
  colnames(srG) = c("Divider", "srG")
  SSt = sum((helper_dataframe[, 1] - srY)^2)

  # SPLIT SET TO GET NUMBER OF OBSERVATIONS WHEN DIVIDED
  spl <- split(helper_dataframe, helper_dataframe[, 2])
  # number_in_group <- length(spl[[1]][[1]])
  
  lengths_vect = c()
  for (i in 1:length(spl)){
    lengths_vect[i] <- length(spl[i][[1]][[1]])
  }
  
  # MEANS C.D.
  SSb = sum((srG$srG-srY)^2)*unique(lengths_vect)

  SSw = SSt - SSb
  q <- qtukey(0.95, k, n-k)

  # F-STATISTICS

  F_stat = (SSb / (k-1)) / (SSw/ (dim(helper_dataframe)[1] - k))
  
  # P-VALUE
  p_wartosc = pf(F_stat, k-1, dim(helper_dataframe)[1] - k, lower.tail = FALSE)
  
  # MEAN SQUARE BETWEEN
  MSw <- SSw/(n-k)

  names <- c(independent_var, "Residuals")
  degrees_of_freedom <- c(k-1, n-k)
  sosq <- c(SSb, SSw)
  stars <- get_stars(c(p_wartosc))
  stars <- append(stars, "")
  F_vect <- c(round(F_stat, 2), "")
  P_vect <- c(p_wartosc, "")
  
  # PRINT
  cat("Analysis of Variance Table\n\nResponse: ", dependent_var, "\n")
  df <- data.frame(X=names,
                   Df=degrees_of_freedom,
                   Sum_of_squares=sosq,
                   F_value=F_vect,
                   P.value=P_vect,
                   stars=stars
                   )
  print(format(df, justify="left"), right=F)
  cat("---\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n\n\n")
  if (p_wartosc <= 0.05){
    cat("----------------------- POST HOC -----------------------------\n")
    post_hoc(q, MSw, srG, k, n)
  }
  
}
# one_way_anova(data, Sepal.Length, Species)

# POST-HOC
post_hoc <- function(q, MSw, srG, k, n){
  W <- q * sqrt(MSw/(n-k))
  names <- as.vector(srG$Divider)
  means <- as.vector(srG$srG)
  m <- cbind(names, means)
  
  # DATAFRAME NAMES 
  # MAYBE AS A MATRIX?
  mat <- data.frame(diag("NA", nrow=length(names), ncol=length(names)))
  mat_values <- data.frame(diag("NA", nrow=length(names), ncol=length(names)))
  rownames(mat) <- names
  colnames(mat) <- names
  rownames(mat_values) <- names
  colnames(mat_values) <- names

  for (i in 1:length(m[, 1])){
    for (j in 1:length(m[, 1])){
      if (i != j){
        name1 <- m[i, 1]
        name2 <- m[j, 1]
        diff <- abs(as.double(m[i, 2]) - as.double(m[j, 2]))
        if (W > diff){
          mat[name1, name2] <- ""
          mat_values[name1, name2] <- diff
        } else {
          mat[name1, name2] <-  "***"
          mat_values[name1, name2] <- diff
        }
      }
    }
  }
  
  print(mat)
  cat("\n")
  cat("W-STAT ", W, "\n")
  print(mat_values)
}
# THERE ARE TWO TABLES IN POST-HOC TESTS
# FIRST TABLE SHOWS SIGNIFICANT DIFFERENCES BETWEEN GROUPS
# SECOND TABLE SHOWS DIFFERENCE BETWEEN GROUPS TO COMPARE WITH W STATISTICS VALUE

# TWO WAY ANOVA
two_way_anova <- function(data, dependent_var, independent_var_1, independent_var_2){
  # SETTING UP ARGUMENTS
  
  # CREATE HELPER DATAFRAME
  helper_dataframe <- cbind(data[dependent_var], 
                            c(data[independent_var_1]), 
                            c(data[independent_var_2])
                            )
  
  helper_dataframe[, 2] <- as.factor(helper_dataframe[, 2])
  helper_dataframe[, 3] <- as.factor(helper_dataframe[, 3])
  
  srY = mean(helper_dataframe[, 1])
  
  mAlpha = aggregate(helper_dataframe[, 1], by = list(helper_dataframe[, 2]), FUN="mean")
  colnames(mAlpha) = c("Species", "srAlpha")
  
  mBeta = aggregate(helper_dataframe[, 1], by = list(helper_dataframe[, 3]), FUN="mean")
  colnames(mBeta) = c("Species", "srBeta")
  
  SSt = sum((helper_dataframe[, 1] - srY)^2)
  
  # SPLIT SET TO GET NUMBER OF OBSERVATIONS WHEN DIVIDED
  spl1 <- split(helper_dataframe, helper_dataframe[, 2])
  spl2 <- split(helper_dataframe, helper_dataframe[, 3])
  
  # GET SIZE OF SUBSETS DIVIDED BY FIRST VARIABLE
  lengths_vect_1 = c()
  for (i in 1:length(spl1)){
    lengths_vect_1[i] <- length(spl1[i][[1]][[1]])
  }
  SSa = sum((mAlpha$srAlpha - srY)^2) * unique(lengths_vect_1)
    
  # GET SIZE OF SUBSETS DIVIDED BY SECOND VARIABLE
  lengths_vect_2 = c()
  for (i in 1:length(spl2)){
    lengths_vect_2[i] <- length(spl2[i][[1]][[1]])
  }
  SSb = sum((mBeta$srBeta - srY)^2 * unique(lengths_vect_2))
  
  SSe = SSt - SSa - SSb
  
  # F STATISTICS
  Fa = (SSa / (length(unique(helper_dataframe[, 2])) - 1)) / (SSe / (-1+dim(data)[1]-length(unique(helper_dataframe[, 3]))))
  Fb = (SSb / (length(unique(helper_dataframe[, 3])) - 1)) / (SSe / (-1+dim(data)[1]-length(unique(helper_dataframe[, 3]))))
  
  # P VALUES
  pA = pf(Fa, 146, 2, lower.tail = FALSE)
  pB = pf(Fb, 146, 1, lower.tail = FALSE)
  
  
  names <- c(independent_var_1, independent_var_2, "Residuals")
  degrees_of_freedom <- c(length(unique(helper_dataframe[, 2]))-1, length(unique(helper_dataframe[, 3]))-1 , length(helper_dataframe[, 2]) - length(unique(helper_dataframe[, 2])))
  sosq <- c(SSa, SSb, SSe)
  stars <- get_stars(c(pA, pB))
  stars <- append(stars, "")
  F_vect <- c(Fa, Fb, "")
  P_vect <- c(pA, pB, "")
  
  # PRINT
  cat("Analysis of Variance Table\n\nResponse: ", dependent_var, "\n")
  df <- data.frame(X=names,
                   Df=degrees_of_freedom,
                   Sum_of_squares=sosq,
                   F_value=F_vect,
                   P.value=P_vect,
                   stars=stars
  )
  print(format(df, justify="left"), right=F)
  cat("---\nSignif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")

}
# two_way_anova(data, Sepal.Length, Species, season)