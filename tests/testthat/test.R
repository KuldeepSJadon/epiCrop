context("BlightR_function")
library(epiCrop)

# Create an object to be saved as initial test
out <- BlightR(weather)

expect_equal(nrow(out), 140)
expect_is(out, "data.frame")

check <- out[3,5:8] %>% unlist() %>% as.numeric()
expect_equal(
  check,
  c(2.325133e-03, 3.162054e-05, 1.989748e-03, 7.352198e-08) ,
  tolerance = 1e-09
)
