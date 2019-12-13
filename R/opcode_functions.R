# Function to retrieve the value for use by opcode function
get_value <- function(input, parcodes, parameters, n, relative_base) {
    if (parcodes[n] == 0) {
        # double memory if needed
        while((parameters[n] + 1) > length(input)) {
            input <- c(input, rep(0, length(input)))
        }
        input[parameters[n] + 1]
    } else if (parcodes[n] == 1) {
        parameters[n]
    } else if (parcodes[n] == 2) {
        # double memory if needed
        while((parameters[n] + 1 + relative_base) > length(input)) {
            input <- c(input, rep(0, length(input)))
        }
        input[parameters[n] + 1 + relative_base]
    }
    else {
        stop("unknown parameter code")
    }
}

# Function to write a value for use by opcode function
write_value <- function(input, parcodes, parameters, n, relative_base, value) {
    if (parcodes[n] == 0) {
        input[parameters[n] + 1] <- value
    } else if (parcodes[n] == 2) {
        input[parameters[n] + 1 + relative_base] <- value
    } else {
        stop("That shouldn't happen here")
    }

    # if we write to an area above current vector length it will fill the
    # empty spaces with NA.  Replace these by zero's.
    input[is.na(input)] <- 0
    input
}

# Function to update state
state_update <- function(state, input, index, output = NULL, relative_base = NULL) {
    state$input <- input
    state$index <- index
    if (!is.null(output)) {
        state$output <- output
    }
    if (!is.null(relative_base)) {
        state$relative_base <- relative_base
    }
    state
}

# opcode functions
opcode_1 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    input <- write_value(input, parcodes, parameters, 3, relative_base, x + y)
    index <- index + 4

    state <- state_update(state, input, index)
}

opcode_2 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    input <- write_value(input, parcodes, parameters, 3, relative_base, x * y)
    index <- index + 4

    state <- state_update(state, input, index)
}

opcode_3 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    value <- state$value
    state$value <- NULL

    parameters <- input[index + 1]
    input <- write_value(input, parcodes, parameters, 1, relative_base, value)
    index <- index + 2

    state <- state_update(state, input, index)
}

opcode_4 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base
    output <- state$output

    parameter <- input[index + 1]
    x <- get_value(input, parcodes, parameter, 1, relative_base)
    index <- index + 2

    state$global_output <- x
    state <- state_update(state, input, index, output = x)
}

opcode_5 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameters <- input[index + 1:2]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    if (x != 0) {
        y <- get_value(input, parcodes, parameters, 2, relative_base)
        index <- y + 1
    } else {
        index <- index + 3
    }

    state <- state_update(state, input, index)
}

opcode_6 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameters <- input[index + 1:2]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    if (x == 0) {
        y <- get_value(input, parcodes, parameters, 2, relative_base)
        index <- y + 1
    } else {
        index <- index + 3
    }

    state <- state_update(state, input, index)
}

opcode_7 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    if (x < y)  {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 1)
    } else {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 0)
    }
    index <- index + 4

    state <- state_update(state, input, index)
}

opcode_8 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameters <- input[index + 1:3]
    x <- get_value(input, parcodes, parameters, 1, relative_base)
    y <- get_value(input, parcodes, parameters, 2, relative_base)
    if (x == y)  {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 1)
    } else {
        input <- write_value(input, parcodes, parameters, 3, relative_base, 0)
    }
    index <- index + 4

    state <- state_update(state, input, index)
}

opcode_9 <- function(state, parcodes) {
    input <- state$input
    index <- state$index
    relative_base <- state$relative_base

    parameter <- input[index + 1]
    x <- get_value(input, parcodes, parameter, 1, relative_base)
    relative_base <- relative_base + x
    index <- index + 2

    state <- state_update(state, input, index, relative_base = relative_base)
}
