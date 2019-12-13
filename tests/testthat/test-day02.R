# default state settings for day02
state <- list()
state$value <- 0
state$relative_base <- 0
state$index <- 1

# tests
test_that("example one", {
    state$input <- c(1,9,10,3,2,3,11,0,99,30,40,50)
    result <- single_intcode_computer(state)
    expected <- c(3500,9,10,70,2,3,11,0,99,30,40,50)
    expect_equal(result$input, expected)
})

test_that("example two", {
    state$input <- c(1,0,0,0,99)
    result <- single_intcode_computer(state)
    expected <- c(2,0,0,0,99)
    expect_equal(result$input, expected)
})

test_that("example three", {
    state$input <- c(2,3,0,3,99)
    result <- single_intcode_computer(state)
    expected <- c(2,3,0,6,99)
    expect_equal(result$input, expected)
})

test_that("example four", {
    state$input <- c(2,4,4,5,99,0)
    result <- single_intcode_computer(state)
    expected <- c(2,4,4,5,99,9801)
    expect_equal(result$input, expected)
})

test_that("example five", {
    state$input <- c(1,1,1,4,99,5,6,0,99)
    result <- single_intcode_computer(state)
    expected <- c(30,1,1,4,2,5,6,0,99)
    expect_equal(result$input, expected)
})
