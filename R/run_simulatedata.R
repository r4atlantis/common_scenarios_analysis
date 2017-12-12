#' Simulate data from a MARSS model
#'
#' @param parobj A list of parameters for the simulation. The list can
#' be extracted from a model fit to data using
#' \code{MARSS:::parmat(MARSS::MARSS(data))} or generated and augmented using
#' \code{get_parmat}.
#' @param tSteps The number of time steps desired in the simulated data
#' @param nsim The number of replicates desired
#'
#' @author Kelli Faye Johnson
#'
#' @returns A list of input and output metrics for the simulation along
#' with the simulated data. The list contains the simulated states as well
#' as the simulated states with observation error. If there is no observation
#' error than the states and the simulated data are the same. The parameter
#' list used to simulate the data is also returned.
#' Data output is in an array, with the first dimension being the number of
#' states or observations, the second dimension being the number of years,
#' and the third dimension being the number of simulations (i.e., \code{nsim}).
#'
run_simulatedata <- function (parobj, tSteps = 1, nsim = 1, seed = nsim) {

 # https://stats.stackexchange.com/questions/38856/how-to-generate-correlated-random-numbers-given-means-variances-and-degree-of
 # https://stackoverflow.com/questions/13291308/generate-numbers-with-specific-correlation
 # https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variable

    n = dim(parobj$A)[1]
    m = dim(parobj$x0)[1]

    sim.data = array(as.numeric(NA), dim = c(n, tSteps, nsim))
    sim.states = array(as.numeric(NA), dim = c(m, tSteps, nsim))

    newData = matrix(NA, n, tSteps + 1)
    newStates = matrix(NA, m, tSteps + 1)

    if (!is.null(seed)) set.seed(seed)

    par1 = parobj
    Omg1 = t.Omg1 = n.not0 = Omg0 = t.Omg0 = list()
    for (elem in c("Q", "R", "V0")) {
        dim.par = dim(parobj[[elem]])[1]
        Omg1[[elem]] = t.Omg1[[elem]] = Omg0[[elem]] = t.Omg0[[elem]] = array(0,
            dim = c(dim.par, dim.par))
        n.not0[[elem]] = c()
        the.par = par1[[elem]]
        diag.par = diag(the.par)
        n.not0[[elem]] = sum(diag.par != 0)
        I.mat = diag(1, dim.par)
        if (n.not0[[elem]] == dim.par) {
            Omg1[[elem]] = t.Omg1[[elem]] = I.mat
            Omg0[[elem]] = t.Omg0[[elem]] = matrix(0, dim.par,
                dim.par)
        }
        else {
            if (n.not0[[elem]] == 0) {
                Omg0[[elem]] = t.Omg0[[elem]] = I.mat
                Omg1[[elem]] = t.Omg1[[elem]] = matrix(0, dim.par,
                  dim.par)
            }
            else {
                Omg1[[elem]] = I.mat[diag.par != 0, , drop = FALSE]
                Omg0[[elem]] = I.mat[diag.par == 0, , drop = FALSE]
                t.Omg1[[elem]] = t(Omg1[[elem]])
                t.Omg0[[elem]] = t(Omg0[[elem]])
            }
        }
    }
    x0.mat = par1$x0
    pari = par1
    for (i in 1:nsim) {
        newStates[, 1] = x0.mat
        if (n.not0$V0 != 0) {
            V0.mat = Omg1$V0 %*% par1$V0 %*% t.Omg1$V0
            x0.new = array(mvtnorm::rmvnorm(1, mean = Omg1$V0 %*% x0.mat,
                sigma = V0.mat, method = "chol"), dim = dim(x0.mat))
            newStates[, 1] = t.Omg1$V0 %*% x0.new + t.Omg0$V0 %*%
                Omg0$V0 %*% x0.mat
        }
        else {
            newStates[, 1] = x0.mat
        }
        for (j in 2:(tSteps + 1)) {
            if (n.not0$R[min(j - 1, length(n.not0$R))] != 0) {
                R.mat = Omg1$R %*% pari$R %*% t.Omg1$R
                obs.error = t(mvtnorm::rmvnorm(1, mean = rep(0, n.not0$R),
                  sigma = R.mat, method = "chol"))
                obs.error = t.Omg1$R %*% obs.error
            }
            else {
                obs.error = matrix(0, n, 1)
            }

            if (n.not0$Q != 0) {
                Q.mat = Omg1$Q %*% pari$Q %*% t.Omg1$Q
                pro.error = t(mvtnorm::rmvnorm(1, mean = rep(0, n.not0$Q),
                  sigma = Q.mat, method = "chol"))
                pro.error = t.Omg1$Q %*% pro.error
            }
            else {
                pro.error = matrix(0, m, 1)
            }

            newStates[, j] = pari$B %*% newStates[, j - 1] +
                pari$U + pro.error
            newData[, j] = pari$Z %*% newStates[, j] + pari$A +
                obs.error
        }
        newStates = newStates[, 2:(tSteps + 1)]
        newData = newData[, 2:(tSteps + 1)]
        sim.data[, , i] = as.matrix(newData)
        sim.states[, , i] = as.matrix(newStates)
        newStates = matrix(NA, m, tSteps + 1)
        newData = matrix(NA, n, tSteps + 1)
    }

    return(list(
      sim.states = sim.states,
      sim.data = sim.data,
      sim.obj = parobj
    ))
}
