library(testthat)
library(ybLN)
library(stats)
data_iris = iris
fit1 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,data = data_iris)
fit2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + 0,data = data_iris)

a1 = anova(fit1)
a2 = anova(fit2)

X = cbind(data_iris$Sepal.Width,data_iris$Petal.Length,data_iris$Petal.Width)
Y = data_iris$Sepal.Length

fityb1 = yblm(X,Y,T)
fityb2 = yblm(X,Y)

yba1 = ybanova(fityb1)
yba2 = ybanova(fityb2)
#----------------------------------------Test result when we have intercept--------------------
test_that("yblm returns correct table", {
  tolerance = 1e-5
  anova_table = a1[1,] + a1[2,] + a1[3,]
  anova_table[3] = anova_table[2]/anova_table[1]
  anova_table[4] = anova_table[3]/a1[4,3]
  anova_table[5] <- 1 - pf(as.numeric(anova_table[4]), as.numeric(anova_table[1]), as.numeric(a1[4,1]))
  temp = as.matrix(rbind(anova_table,a1[4,]))


  expect_equal(as.numeric(unname(as.matrix(yba1))[,-1]), as.numeric(unname(temp)),tolerance = tolerance)
})

#----------------------------------------Test result when we have no intercept--------------------
test_that("yblm returns correct table", {
  tolerance = 1e-5
  anova_table2 = a2[1,] + a2[2,] + a2[3,]
  anova_table2[3] = anova_table2[2]/anova_table2[1]
  anova_table2[4] = anova_table2[3]/a2[4,3]
  anova_table2[5] <- 1 - pf(as.numeric(anova_table2[4]), as.numeric(anova_table2[1]), as.numeric(a2[4,1]))
  temp2 = as.matrix(rbind(anova_table2,a2[4,]))
  expect_equal(as.numeric(unname(as.matrix(yba2))[,-1]), as.numeric(unname(temp2)),tolerance = tolerance)
})

