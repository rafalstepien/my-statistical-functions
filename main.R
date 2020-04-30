statistical.package <- function(type, ...){
  if (type == "t-test"){
    source('t-Student.R')
    my.ttest(...)
  } else if (type == 'kendall'){
    source('KendallCorrelation.R')
    kendall(...)
  } else if (type == 'oneway'){
    source('ANOVA.R')
    one_way_anova(...)
  } else if (type == 'twoway'){
    source('ANOVA.R')
    two_way_anova(...)
  } else if (type == 'linearreg'){
    source('linearRegression.R')
    linear_model(...)
  } else if (type == 'help') {
    cat('Help for avilable functions usage. \n\n')
    cat('t-test: statistical.package("t-test", x, y, mu0, alternative, alpha)\n')
    cat('    x - vector type of x values\n')
    cat('    y - vector type of y values\n')
    cat('    mu0 - value of mu0\n')
    cat('    alternative - number of alternative hypothesis\n')
    cat('    alpha - double type number of significance level\n')
    cat('statistical.package("t-test", c(10, 20, 50, 70), c(10, 89, 20, 100), 40, 1, 0.05)\n\n')
    cat('kendall: statistical.package("kendall", x, y)\n')
    cat('    x - vector type of x values\n')
    cat('    y - vector type of y values\nEXAMPLE:\n')
    cat('statistical.package("kendall", c(10, 20, 50, 70), c(10, 89, 20, 100))\n\n')
    cat('linearreg: statistical.package("linearreg", data, dependent, ...)\n')
    cat('    data - dataframe type of data to analysis\n')
    cat('    dependent - dependent variable\n')
    cat('    ... - independent varialbes\nEXAMPLE:\n')
    cat('statistical.package("linearreg", data, dependent_variable=data$Sepal.Length, data$Sepal.Width, data$Petal.Length, data$Petal.Width)\n\n')
    cat('oneway: statistical.package("oneway", data, dependent_variable, independent_variable)\n')
    cat('    data - dataframe type of data to analysis\n')
    cat('    dependent_variable - dependent variable\n')
    cat('    independent_variable - independent variable\nEXAMPLE:\n')
    cat('statistical.package("oneway", data, "Sepal.Length", "Species")\n\n')
    cat('twoway: statistical.package("twoway", data, dependent_var, independent_var_1, independent_var_2 )\n')
    cat('    data - dataframe type of data to analysis\n')
    cat('    dependent_variable - dependent variable\n')
    cat('    independent_var_1 - independent variable number one\n')
    cat('    independent_var_2 - independent variable number two\nEXAMPLE:\n')
    cat('statistical.package("twoway", data, "Sepal.Length", "Species", "season")')
    
  } else {
    cat("There is no type", type, "please specify one of the following:\n")
    cat('    > t-test\n')
    cat('    > kendall\n')
    cat('    > oneway\n')
    cat('    > twoway\n')
    cat('    > linearreg\n')
    cat('Please use statistical.package("help") to get more info\n')
    cat(' ----------------------------------')
    
  }
}

# EXAMPLES FOR ALL FUNCTIONS FROM "HELP"
statistical.package("help")

data = iris
data$season = rbinom(150,1,prob=0.5)
statistical.package("t-test", c(10, 20, 50, 70), c(10, 89, 20, 100), 40, 1, 0.05)
statistical.package("kendall", c(10, 20, 50, 70), c(10, 89, 20, 100))
statistical.package("linearreg", data, dependent_variable=data$Sepal.Length, data$Sepal.Width, data$Petal.Length, data$Petal.Width)
statistical.package("oneway", data, "Sepal.Length", "Species")
statistical.package("twoway", data, "Sepal.Length", "Species", "season")
