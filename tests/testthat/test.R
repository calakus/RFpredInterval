## Load data
data(BostonHousing, package = "RFpredInterval")

## Split data set into train and test
set.seed(2345)
trainindex <- base::sample(nrow(BostonHousing),round(nrow(BostonHousing)*0.7), replace = FALSE)
traindata <- BostonHousing[trainindex, ]
testdata <- BostonHousing[-trainindex, ]

yvar.index <- which(names(BostonHousing) == "medv")
xvar.names <- names(BostonHousing)[-yvar.index]

formula <- as.formula(medv ~ .)
alpha <- 0.05
ntree <- 20
px <- length(xvar.names)

params_rfsrc <- list(mtry = ceiling(px/3),
                     ntree = ntree,
                     nodesize = 3,
                     samptype = "swr")
params_ranger <- list(mtry = ceiling(px/3),
                      num.trees = ntree,
                      min.node.size = 5,
                      replace = TRUE)

test_that("calibration",{
  expect_error(pibf(formula,
                    traindata = traindata,
                    testdata = testdata,
                    alpha = alpha,
                    calibration = "oob",
                    coverage_range = c(0.96,0.99)),
               "1-alpha is not within the limits of 'coverage_range'.")
  expect_error(pibf(formula,
                    traindata = traindata,
                    testdata = testdata,
                    alpha = alpha,
                    calib = "oob",
                    coverage_range = c(0.96)),
               "'coverage_range' should be a numeric vector of length 2.")
})


test_that("split.rule",{
  split_rule <- "l1"
  expect_error(rfpi(formula,
                    traindata = traindata,
                    testdata = testdata,
                    alpha = alpha,
                    split_rule = split_rule,
                    rf_package = "ranger"),
               paste0(split_rule," split rule cannot be applied with ranger package. Change rf_package to rfsrc."))
  split.rule <- "spi"
  expect_error(rfpi(formula,
                    traindata = traindata,
                    testdata = testdata,
                    alpha = alpha,
                    split_rule = split_rule,
                    rf_package = "ranger"),
               paste0(split_rule," split rule cannot be applied with ranger package. Change rf_package to rfsrc."))
})

test_that("boperror",{
  params_ranger[["num.trees"]] <- 2
  expect_error(pibf(formula,
                    traindata = traindata,
                    testdata = testdata,
                    alpha = alpha,
                    calibration = "oob",
                    params_ranger = params_ranger),
               "Some of the OOB predictions are NaN. Increase the number of trees, 'num.trees' in params_ranger.")
})
