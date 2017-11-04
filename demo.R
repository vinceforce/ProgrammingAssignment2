source("cachematrix.R")
demo <- function() {
        mm <- matrix(c(0, 1, -1, 0), 2, 2)
        print(mm)
        mmc <- makeCacheMatrix(mm)
        mminv <- cacheSolve(mmc)
        print(mminv)
        mminv2 <- cacheSolve(mmc)
        print(mminv2)
}