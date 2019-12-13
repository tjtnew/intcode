# default state settings for day05
state <- list()
state$relative_base <- 0
state$index <- 1

# tests
test_that("example one", {
    state$input <- c(3,0,4,0,99)
    expected <- 1
    state$value <- expected
    result <- single_intcode_computer(state)
    expect_equal(result$output, expected)

    expected <- -1
    state$value <- expected
    result <- single_intcode_computer(state)
    expect_equal(result$output, expected)
})

test_that("example two", {
    state$input <- c(1002,4,3,4,33)
    state$value = 0
    result <- single_intcode_computer(state)
    expected <- c(1002,4,3,4,99)
    expect_equal(result$input, expected)
})


# -------------------------------------------------------------------------
test_that("example three", {
    state$input <- c(3,9,8,9,10,9,4,9,99,-1,8)
    state$value <- 7
    result <- single_intcode_computer(state)
    expect_equal(result$output, 0)

    state$input <- c(3,9,8,9,10,9,4,9,99,-1,8)
    state$value <- 8
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1)
})

test_that("example four", {
    state$input <- c(3,9,7,9,10,9,4,9,99,-1,8)
    state$value <- 7
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1)

    state$input <- c(3,9,7,9,10,9,4,9,99,-1,8)
    state$value <- 8
    result <- single_intcode_computer(state)
    expect_equal(result$output, 0)
})

test_that("example five", {
    state$input <- c(3,3,1108,-1,8,3,4,3,99)
    state$value <- 7
    result <- single_intcode_computer(state)
    expect_equal(result$output, 0)

    state$input <- c(3,3,1108,-1,8,3,4,3,99)
    state$value <- 8
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1)
})

test_that("example six", {
    state$input <- c(3,3,1107,-1,8,3,4,3,99)
    state$value <- 7
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1)

    state$input <- c(3,3,1107,-1,8,3,4,3,99)
    state$value <- 8
    result <- single_intcode_computer(state)
    expect_equal(result$output, 0)
})


# -------------------------------------------------------------------------
test_that("example seven", {
    state$input <- c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    state$value <- 0
    result <- single_intcode_computer(state)
    expect_equal(result$output, 0)

    state$input <- c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    state$value <- 1
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1)
})

test_that("example eight", {
    state$input <- c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    state$value <- 0
    result <- single_intcode_computer(state)
    expect_equal(result$output, 0)

    state$input <- c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    state$value <- 1
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1)
})


# -------------------------------------------------------------------------
test_that("example nine", {
    state$input <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    state$value <- 7
    result <- single_intcode_computer(state)
    expect_equal(result$output, 999)

    state$input <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    state$value <- 8
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1000)

    state$input <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
               1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    state$value <- 9
    result <- single_intcode_computer(state)
    expect_equal(result$output, 1001)
})




