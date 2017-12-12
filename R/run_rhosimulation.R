#'
#'
#' @param B11
#' @param B21
#' @param B22
#' @param Q11
#' @param Q22
#' @param Qrho
#' @param R
#' @param Bem
#' @param Qem
#' @param Rem
#' @param nstates
#' @param nyears
#' @param nsimulations
#'
#' @return
#'
#' @author Kelli Faye Johnson

rhosimulation <- function(
  B11 = 0, B21 = 0, B12 = 0, B22 = 0,
  Q11 = 1, Q22 = Q11, Qrho = 0,
  R = 0.1,
  Bem = "unconstrained", Qem = "unconstrained", Rem = "zero",
  prewhiten = "AIC",
  nstates = 2, nyears = 100, nsimulations = 1) {

  #### Simulate data
  # Generate parameter matrix
  pars <- get_parmat(m = 2, n = nstates)
  # Specify the interaction matrix
  pars$B[1] <- as.numeric(B11)
  pars$B[2] <- as.numeric(B21)
  pars$B[3] <- as.numeric(B12)
  pars$B[4] <- as.numeric(B22)
  # Specify the process error matrix
  Q21 <- Qrho * sqrt(Q11) * sqrt(Q22)
    # Check that it worked
    if(!all.equal(
      Qrho,
      cov2cor(matrix(c(Q11, rep(Q21, 2), Q22), 2))[2],
      tolerance = 10^-10)) {
      stop("Your specified Qrho does not equal the ",
        "calculated value")
    }
  pars$Q[1] <- as.numeric(Q11)
  pars$Q[2:3] <- as.numeric(Q21)
  pars$Q[4] <- as.numeric(Q22)
  # Specify observation error
  if (is.matrix(R)) {
    if (all(dim(R) == dim(pars$R))) {
      pars$R <- R
    } else (stop("R matrix is the wrong size."))
  } else {
    if (NROW(pars$R) < length(R)) stop("Too many R values were specified.")
    diag(pars$R) <- R
  }

  # Simulate data
  sims <- run_simulatedata(pars, tSteps = nyears, nsim = nsimulations)

  #### Estimation
  ests <- apply(aperm(sims$sim.data, c(2:1, 3)), 3,
    run_analysis, Bem = Bem, Qem = Qem, Rem = Rem, prewhiten = prewhiten)

  parout <- data.frame(t(sapply(ests, "[[", "pars")))
  parout$iteration <- 1:NROW(parout)
  temp <- unlist(pars)
  names(temp) <- paste0("om__", names(unlist(temp)))
  if (NROW(parout) == 1) {
    parout <- cbind(parout, t(temp))
  } else {
    parout <- data.frame(parout, sapply(temp, rep, NROW(parout)))
  }

  return(list(
    "sims" = sims,
    "ests" = ests,
    "outs" = parout
  ))

}

run_rhosimulation <- function(width = 0.9,
  autocorrelation = 0.9,
  obserror = c(0, 1.0),
  percapita = 1.35,
  estimateobs = "zero",
  nstates = 2, n = c(50, 100, 1000), nsimulations = 100) {

  if (length(percapita) == 1) {
    percapita <- seq(-1 * percapita, percapita, width)
  }

  allspecs <- expand.grid(
    "B1" = seq(-1 * autocorrelation, autocorrelation, width),
    "B2" = percapita,
    "Q1" = c(1),
    "Q2" = seq(-1 * autocorrelation, autocorrelation, width),
    "Rom" = obserror,
    "Zom" = nstates,
    "Bem" = "unconstrained",
    "Rem" = estimateobs,
    "tslength" = n,
    stringsAsFactors = FALSE)

  anal <- apply(allspecs, 1, function(x) {
    rhosimulation(
      B11 = as.numeric(x["B1"]),
      B21 = as.numeric(x["B2"]),
      B12 = 0,
      B22 = as.numeric(x["B1"]),
      Q11 = as.numeric(x["Q1"]),
      Qrho = as.numeric(x["Q2"]),
      R = as.numeric(x["Rom"]),
      Bem = x["Bem"],
      Qem = "unconstrained",
      Rem = x["Rem"],
      prewhiten = "AIC",
      nstates = as.numeric(x["Zom"]),
      nyears = as.numeric(x["tslength"]),
      nsimulations = nsimulations)
  })

  return(anal)
}

plotsquare <- function(data, pars, filter = TRUE,
  xlab = expression(i[e]), ylab, legend = "none",
  height = 4, width = 4, fn = NULL, positive = TRUE) {
  temp <- reshape(data,
    direction = "long", varying = pars,
    timevar = "par", sep = substring(pars[1], 2, 4))
  if (positive) {
    temp <- temp[temp$om__B1 >= 0 & temp$om__B2 >= 0 & temp$om__Q2 >= 0, ]
  }
  temp$om__B1 <- paste0("rho==", temp$om__B1)
  temp$om__B2 <- paste0("i[p]==", temp$om__B2)
  temp$om__Q2 <- factor(temp$om__Q2)
  temp$par <- gsub("no\\.(.+)", "a\\1\\.no", temp$par, perl = TRUE)
  temp$par <- factor(temp$par)
  temp <- temp[!is.na(temp[, substring(pars[1], 1, 1)]), ]
  if (filter) {
    # This only filters out the MARSS results
    check <- apply(temp, 1, function(x) {
      if(grepl("[A-Z]", x["par"])) {
        add <- ifelse(names(x)[length(x) - 1] == "r", "raw", "std")
        use <- grep(paste0(add, ".",
          substring(x["par"], 1, 12), "hess"), names(x))
        returnme <- ifelse(gsub("\\s", "", x[use]) %in% c(0, NA), TRUE, FALSE)
      } else {returnme <- TRUE}
      return(returnme)
    })
    temp <- temp[check, ]
  }
  g <- ggplot(temp) +
    geom_violin(
      aes_string(x = "om__Q2", y = substring(pars[1], 1, 1),
        fill = "par", col = "par"), lwd = 0.5) +
      facet_grid(om__B1 ~ om__B2, labeller = label_parsed) +
      scale_fill_grey() +
      scale_color_grey() +
      xlab(xlab) +
      ylab(ylab) +
      theme_bw() + theme(
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      strip.text.y = element_text(angle = -270),
      legend.key = element_rect(colour = NA),
      legend.position = legend,
      text = element_text(size = 10))
  if (!is.null(fn)) {
    ggsave(plot = g, filename = fn, height = height, width = width, dpi = 600)
  }
  invisible(g)
}

plotobs <- function(data, x, pars, filter = TRUE,
  xlab = expression(i[e]), ylab, legend = "none",
  height = 2, width = 4, fn = NULL) {
  temp <- reshape(data,
    direction = "long", varying = pars,
    timevar = "par", sep = substring(pars[1], 2, 4))
  temp <- temp[temp$om__B1 >= 0 & temp$om__B2 >= 0 & temp$om__Q2 >= 0, ]
  temp$om__B1 <- paste0("rho==", temp$om__B1)
  temp$om__B2 <- factor(temp$om__B2)
  temp$om__Q2 <- factor(temp$om__Q2)
  temp$par <- gsub("no\\.(.+)", "a\\1\\.no", temp$par, perl = TRUE)
  temp$par <- factor(temp$par, labels = c("prewhitened", "MARSS", "MAR"))
  temp$par <- factor(temp$par, levels = c("prewhitened", "MAR", "MARSS"))
  temp <- temp[!is.na(temp[, substring(pars[1], 1, 1)]), ]
  if (filter) {
    # This only filters out the MARSS results
    check <- apply(temp, 1, function(x) {
      if(grepl("[A-Z]", x["par"])) {
        add <- ifelse(names(x)[length(x) - 1] == "r", "raw", "std")
        use <- grep(paste0(add, ".",
          substring(x["par"], 1, 12), "hess"), names(x))
        returnme <- ifelse(gsub("\\s", "", x[use]) %in% c(0, NA), TRUE, FALSE)
      } else {returnme <- TRUE}
      return(returnme)
    })
    if (is.list(check)) check <- unlist(c(check))
    temp <- temp[check, ]
  }
  g <- ggplot(temp, aes(col = factor(n), fill = factor(n))) +
    geom_violin(
      aes_string(x = x, y = substring(pars[1], 1, 1)), lwd = 0.5) +
      facet_grid(. ~ par, labeller = label_parsed) +
      scale_fill_grey() +
      scale_color_grey() +
      xlab(xlab) +
      ylab(ylab) +
      theme_bw() + theme(
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      strip.text.y = element_text(angle = -270),
      legend.key = element_rect(colour = NA),
      legend.position = legend,
      text = element_text(size = 10))
  if (!is.null(fn)) {
    ggsave(plot = g, filename = fn, height = height, width = width, dpi = 600)
  }
  invisible(g)
}

plotsquare3 <- function(data, name, simname, dir, raw = TRUE, ...) {
  a <- c("raw.unc.unc.zer.Qrho", "raw.no.ccf.p.0", "raw.ccf.p.0")
  b <- c("raw.unc.unc.zer.B2", "raw.no.ccf.p..1", "raw.ccf.p..1")
  c <- c("raw.unc.unc.zer.B1", "raw.unc.unc.zer.B4", "raw.ar1")
  if (!raw) {
    a <- gsub("raw", "std", a)
    b <- gsub("raw", "std", b)
    c <- gsub("raw", "std", c)
  }
  plotsquare(data = data,
    pars = a,
    ylab = expression(hat(i[e])), xlab = expression(i[e]),
    fn = file.path(dir, paste0(simname, "_", name, "_Q.jpeg")),
    ...)
  plotsquare(data = data,
    pars = b,
    ylab = expression(hat(i[p])), xlab = expression(i[e]),
    fn = file.path(dir, paste0(simname, "_", name, "_B.jpeg")),
    ...)
  plotsquare(data = data,
    pars = c,
    ylab = expression(hat(rho)), xlab = expression(i[e]),
    fn = file.path(dir, paste0(simname, "_", name, "_rho.jpeg")),
    ...)
}

table_convergence <- function(data, grepme = "hess") {
  cols <- grep(grepme, colnames(data), value = TRUE)
  check <- c("n", "om__B2", "om__Q2", "om__B1")
  shit <- check
  shit <- gsub("om__B2", "i_{p}", shit)
  shit <- gsub("om__Q2", "i_{e}", shit)
  shit <- gsub("om__B1", "rho", shit)
  forms <- sapply(cols,
    function(x) as.formula(paste(x, paste(check, collapse = "+"), sep = "~")))

  if (grepme == "hess") {
    aa <- lapply(forms, function(x) aggregate(x, data = data,
      function(x) sum(x == 0) / length(x)))
  }
  if (grepl("ar", grepme)) {
    aa <- lapply(forms, function(x) aggregate(x, data = data, median))
  }
  if (grepl("\\.[a-z]", grepme)) {
    aa <- lapply(forms, function(x) aggregate(x, data = data,
      function(y) c(sum(y == 0), sum(y == 1), sum(y == 2), sum(y == 3)) / length(y)))
  }
  out <- Reduce(function(...) merge(..., all = TRUE), aa)
  if (!grepl("\\.[a-z]", grepme)) {
    colnames(out) <- c(shit, substring(cols, 1, 3))
  }
  return(out)
}
table_convergence(basic.par, "raw.d")
table_convergence(states.par)
table_convergence(obserror.par, "raw.ar1")

###############################################################################
###############################################################################
###############################################################################
###############################################################################
run_rhosimulation_all <- function() {
#### Basic
# 25 and 100 years of the main dimensions
basic.base <- run_rhosimulation(
  width = 0.9,
  percapita = 0.9,
  obserror = 0, n = c(25, 50, 100), nsimulations = 100)
# merge shit with basic.par
basic.par <- as.data.frame(apply(do.call("rbind",
  lapply(basic.base, "[[", "outs")), 2, unlist))

plotsquare3(data = basic.par[basic.par$n == max(basic.par$n), ],
  name = "basic_100", simname = my.name, dir = dir.results)
plotsquare3(data = basic.par[basic.par$n == min(basic.par$n), ],
  name = "basic_25", simname = my.name, dir = dir.results,
  positive = FALSE, width = 6, height = 6)
plotsquare3(data = basic.par[basic.par$n == max(basic.par$n), ],
  name = "basic_100_negative", simname = my.name, dir = dir.results,
  positive = FALSE, width = 6, height = 6)
plotsquare3(data = basic.par[basic.par$n == max(basic.par$n), ],
  name = "basic_100_std", simname = my.name, dir = dir.results,
  raw = FALSE, positive = FALSE, width = 6, height = 6)

#### Extreme
# Interaction strength larger than 1.0
extreme.base <- run_rhosimulation(
  width = 0.9,
  percapita = c(-1.35, 1.35, 1.8),
  obserror = 0, n = c(100), nsimulations = 100)
extreme.par <- as.data.frame(apply(do.call("rbind",
  lapply(extreme.base, "[[", "outs")), 2, unlist))

plotsquare3(data = extreme.par,
  name = "extreme_100", simname = my.name, dir = dir.results)

#### Obserror
# Addition of observation error
obserror.base <- run_rhosimulation(
  width = 0.9,
  percapita = c(0, 0.9),
  obserror = c(1.0), estimateobs = c("equal", "zero"),
  n = c(25, 100), nsimulations = 100)
obserror.par <- Reduce(function(...) merge(..., all=TRUE),
  lapply(lapply(obserror.base, "[[", "outs"), apply, 2, unlist))

plotobs(data = obserror.par[
  obserror.par$om__B2 == max(obserror.par$om__B2) &
  obserror.par$om__B1 == max(obserror.par$om__B1), ],
  x = "om__Q2",
  pars = c("raw.unc.unc.zer.Qrho", "raw.unc.unc.equ.Qrho", "raw.ccf.p.0"),
  ylab = expression(hat(i[e])),
  fn = file.path(dir.results, paste0(my.name, "_obserror_Q.jpeg")))
plotobs(data = obserror.par[
  obserror.par$om__Q2 == max(obserror.par$om__Q2) &
  obserror.par$om__B1 == max(obserror.par$om__B1), ],
  x = "om__B2",
  pars = c("raw.unc.unc.zer.B1", "raw.unc.unc.equ.B1", "raw.ccf.p..1"),
  xlab = expression(i[e]), ylab = expression(hat(i[p])),
  fn = file.path(dir.results, paste0(my.name, "_obserror_B.jpeg")))

#### States
# two observations per state
states.base <- run_rhosimulation(
  width = 0.9,
  percapita = c(0, 0.9),
  obserror = c(1.0), estimateobs = c("equal"),
  nstates = 4, n = c(25, 100), nsimulations = 100)
states.par <- Reduce(function(...) merge(..., all=TRUE),
  lapply(lapply(states.base, "[[", "outs"), apply, 2, unlist))

plotsquare(data = states.par[states.par$n == min(states.par$n), ],
  pars = c("raw.unc.unc.equ.B1", "raw.unc.unc.equ.B2", "raw.unc.unc.equ.Qrho"),
  ylab = "MARSS",
  fn = file.path(dir.results, paste0(my.name, "_states_25.jpeg")))
plotsquare(data = states.par[states.par$n == max(states.par$n), ],
  pars = c("raw.unc.unc.equ.B1", "raw.unc.unc.equ.B2", "raw.unc.unc.equ.Qrho"),
  ylab = "MARSS",
  fn = file.path(dir.results, paste0(my.name, "_states_100.jpeg")))

}
