context("MMC")
library(QinR)

test_that("mmc performs properly when c = 1", {
  expect_equal(mmc.summary(0.5, 2, 1, FALSE, FALSE, FALSE)$W_q, 0.5 / (2 * (2 - 0.5)))
})
