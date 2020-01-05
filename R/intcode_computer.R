#' @export
intcode <- function(state) {
    code <- digits(state$input[state$index], 10^(4:0))
    opcode <- as.integer(paste0(code[4],code[5]))
    parcodes <- code[3:1]

    while(opcode != 99) {
        if (opcode == 1) {
            state <- opcode_1(state, parcodes)
        } else if (opcode == 2) {
            state <- opcode_2(state, parcodes)
        } else if (opcode == 3) {
            if (is.null(state$value)) {
                break
            }
            state <- opcode_3(state, parcodes)
        } else if (opcode == 4) {
            state <- opcode_4(state, parcodes)
            break
        } else if (opcode == 5) {
            state <- opcode_5(state, parcodes)
        } else if (opcode == 6) {
            state <- opcode_6(state, parcodes)
        } else if (opcode == 7) {
            state <- opcode_7(state, parcodes)
        } else if (opcode == 8) {
            state <- opcode_8(state, parcodes)
        } else if (opcode == 9) {
            state <- opcode_9(state, parcodes)
        } else {
            stop("unknown code")
        }

        code <- digits(state$input[state$index], 10^(4:0))
        opcode <- as.integer(paste0(code[4],code[5]))
        parcodes <- code[3:1]
    }

    if (opcode == 3) {
        state$finished = FALSE
        state$io = "in"
    } else if (opcode == 4) {
        state$finished = FALSE
        state$io = "out"
    } else {
        state$finished = TRUE
    }

    state
}
