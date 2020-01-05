# function to ensure blog stuff still works after refactoring
#' @export
generic_intcode_computer <- function(state) {
    intcode(state)
}

#' @export
single_intcode_computer <- function(state) {
    finished = FALSE
    output <- NULL
    while(!finished) {
        state <- generic_intcode_computer(state)
        finished <- state$finished
        if(!finished) {
            output <- c(output, state$output)
        }
    }
    state$all_output <- output
    state
}
