library(testthat)
library(ybLN)
data_iris = iris
fit1 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,data = data_iris)
fit2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + 0,data = data_iris)

X = cbind(data_iris$Sepal.Width,data_iris$Petal.Length,data_iris$Petal.Width)
Y = data_iris$Sepal.Length

fityb1 = yblm(X,Y,T)
fityb2 = yblm(X,Y)

#----------------------------------------Test result when we have intercept--------------------
test_that("yblm returns correct coefficient", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$coef), unname(fit1$coefficients),tolerance = tolerance)
})

test_that("yblm returns correct predicted value", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$predict), unname(fit1$fitted.values),tolerance = tolerance)
})

test_that("yblm returns correct residual value", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$residual), unname(fit1$residuals),tolerance = tolerance)
})

test_that("yblm returns correct residual degree of freedom", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$R_df), unname(fit1$df.residual),tolerance = tolerance)
})

test_that("yblm returns correct degree of freedom between groups", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$M_df), unname(sum(anova(fit1)[1]) - fit1$df.residual),tolerance = tolerance)
})

test_that("yblm returns correct R_square", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$R_square),as.numeric(summary(fit1)[8]) ,tolerance = tolerance)
})

test_that("yblm returns correct Adj_R_square", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$Adj_R_square), as.numeric(summary(fit1)[9]),tolerance = tolerance)
})

test_that("yblm returns correct F_statistic", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb1$F_statistic), unname(summary(fit1)$fstatistic["value"]),tolerance = tolerance)
})

test_that("yblm returns correct F_statistic p value", {
  tolerance = 1e-12
  F_p_value <- pf(summary(fit1)$fstatistic[1], summary(fit1)$fstatistic[2], summary(fit1)$fstatistic[3], lower.tail = FALSE)
  expect_equal(as.numeric(fityb1$p_value_F), unname(F_p_value),tolerance = tolerance)
})

test_that("yblm returns correct coefficient matrix", {
  tolerance = 1e-12
  expect_equal(unname(fityb1$coef_matrix), unname(summary(fit1)$coefficients),tolerance = tolerance)
})

#----------------------------------------Test result when we have no intercept--------------------
test_that("yblm returns correct coefficient", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$coef), unname(fit2$coefficients),tolerance = tolerance)
})

test_that("yblm returns correct predicted value", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$predict), unname(fit2$fitted.values),tolerance = tolerance)
})

test_that("yblm returns correct residual value", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$residual), unname(fit2$residuals),tolerance = tolerance)
})

test_that("yblm returns correct residual degree of freedom", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$R_df), unname(fit2$df.residual),tolerance = tolerance)
})

test_that("yblm returns correct degree of freedom between groups", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$M_df), unname(sum(anova(fit2)[1]) - fit2$df.residual),tolerance = tolerance)
})

test_that("yblm returns correct R_square", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$R_square),as.numeric(summary(fit2)[8]) ,tolerance = tolerance)
})

test_that("yblm returns correct Adj_R_square", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$Adj_R_square), as.numeric(summary(fit2)[9]),tolerance = tolerance)
})

test_that("yblm returns correct F_statistic", {
  tolerance = 1e-12
  expect_equal(as.numeric(fityb2$F_statistic), unname(summary(fit2)$fstatistic["value"]),tolerance = tolerance)
})

test_that("yblm returns correct F_statistic p value", {
  tolerance = 1e-12
  F_p_value <- pf(summary(fit2)$fstatistic[1], summary(fit2)$fstatistic[2], summary(fit2)$fstatistic[3], lower.tail = FALSE)
  expect_equal(as.numeric(fityb2$p_value_F), unname(F_p_value),tolerance = tolerance)
})

test_that("yblm returns correct coefficient matrix", {
  tolerance = 1e-12
  expect_equal(unname(fityb2$coef_matrix), unname(summary(fit2)$coefficients),tolerance = tolerance)
})

