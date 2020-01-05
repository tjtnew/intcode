# default state settings for day09
state <- list()
state$value <- 0
state$relative_base <- 0
state$index <- 1

# run the intcode computer with no additional input
run_intcode <- function(state) {
    finished = FALSE
    output <- NULL
    while(!finished) {
        state <- intcode(state)
        finished <- state$finished
        if(!finished) {
            output <- c(output, state$output)
        }
    }
    state$all_output <- output
    state
}

# tests
test_that("example one", {
    state$input <- c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    result <- run_intcode(state)
    expect_equal(result$all_output, state$input)
})

test_that("example two", {
    state$input <- c(1102,34915192,34915192,7,4,7,99,0)
    result <- run_intcode(state)
    expect_equal(nchar(result$all_output), 16)
})

test_that("example three", {
    state$input <- c(104,1125899906842624,99)
    result <- run_intcode(state)
    expect_equal(result$all_output, 1125899906842624)
})
