###############################################################################
###############################################################################
#### Author: Kelli Faye Johnson
#### Coauthors: Isaac C. Kaplan and Andre E. Punt
#### Short title: rhosimulation
#### Description: A simulation to test the effects of autocorrelation on
####   the estimation of correlation and per-capita changes
#### Date: 2017-10-18
#### Notes: "####" is a section header and "#" is a comment
###############################################################################
###############################################################################

#### Set up inputs
my.name <- "rhosimulation"
my.nparallel <- 20
my.graycolours <- c("#bdbdbd", "#969696", "#636363")
my.select <- c("n", "ngroups",
  "autocorrelation", "correlation", "process",
  "processdiag", "observation")

#### Setup libraries
library(doParallel, quietly = TRUE, verbose = FALSE)
library(forecast, quietly = TRUE, verbose = FALSE)
library(foreach, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(MARSS, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(pander, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rmarkdown, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

#### Setup file structure
dir.main <- file.path(paste0(letters, ":"),
  file.path("atlantis", "common_scenarios_analysis"))
dir.main <- dir.main[file.exists(dir.main)]
if (length(dir.main) > 1) stop(length(dir.main),
  " atlantis/common_scenarios_analysis directories were found.")
dir.results <- file.path(dir.main, "results")
dir.create(dir.results, showWarnings = FALSE)
# Source the files to run the analysis
ignore <- sapply(
  dir(file.path(dir.main, "R"), full.names = TRUE, pattern = "\\.r|\\.R"),
  source)

#### Setup plots
my.theme <- plot_theme(get = "1")
my.theme2 <- plot_theme(get = "2")
my.theme3 <- plot_theme(get = "3")

#### Set up parallel
if (my.nparallel > 1) {
  cl <- makeCluster(my.nparallel)
  registerDoParallel(cl)
}

#### Functions
range0 <- function(data) {
  return(c(range(data), 0))
}

#### Run simulation

# test1 <- apply(allspecs[1:2, ], 1, run_simulation, file = "specs")
# test <- run_simulation(allspecs[1, ], file = "specs")
test <- foreach::foreach(
    it_ = seq_len(NROW(allspecs)),
    .combine = "rbind",
    .export = ls(),
    .verbose = FALSE) %dopar%
    run_simulation(specs = allspecs[it_, ])
keep <- test
withstd <- foreach::foreach(
    it_ = seq_len(NROW(stdspecs)),
    .combine = "rbind",
    .export = ls(),
    .verbose = FALSE) %dopar%
    run_simulation(specs = stdspecs[it_, ])
string <- "_[[:alpha:]][[:digit:]]+$"
strings <- unique(gsub(string, "", grep(string, colnames(test), value = TRUE)))

#### Tables

diffs <- aggregate(test$cor.p - test$cor.s,
                   list("error.p" = test$processdiag,
                        "error.o" = test$observation,
                        "interaction" = test$correlation,
                        "correlation" = test$process,
                        "autocorrelation" = test$autocorrelation,
                        "ngroups" = test$ngroups,
                        "n" = test$n),
                   median)
summary(diffs$x)

#### Table_bestworst
filename <- file.path(dir.results,
  paste0(my.name, "_table_bestworst.tex"))
keep <- c("correlation", "process", "cor.p", "ccf.p..1", "ccf.p.0",
  "fixed.unconstrained.zero_b21", "fixed.unconstrained.zero_q21")
temp <- subset(test,
  correlation %in% range0(test$process) &
  process %in% range0(test$process) &
  ngroups == 1 &
  fixed.unconstrained.zero == 0)

worst <- aggregate(. ~ correlation + process,
  data = subset(temp,
  observation == max(test$observation) &
  processdiag == max(test$processdiag) &
  autocorrelation == -0.9 &
  n == min(test$n),
  select = keep),
  median, na.rm = TRUE)
best <- aggregate(. ~ correlation + process,
  data = subset(temp,
  observation == min(test$observation) &
  processdiag == max(test$processdiag) &
  autocorrelation == 0 &
  n == max(test$n) ,
  select = keep),
  median, na.rm = TRUE)
colnames(worst) <- gsub(".+unconstrained.+_", "", colnames(worst))
colnames(best) <- gsub(".+unconstrained.+_", "", colnames(best))
temp <- rbind(
  data.frame("cat" = "worst", worst),
  data.frame("cat" = "best", best))
temp <- temp[order(temp$cat, temp$correlation, temp$process), ]

xtable::print.xtable(xtable::xtable(temp),
  include.rownames = FALSE, include.colnames = TRUE,
  sanitize.colnames.function = identity,
  file = filename)
system(paste("pandoc ", filename, "-o", gsub("tex", "doc", filename)))

#### Table_all
filename <- file.path(dir.results,
  paste0(my.name, "_table_all.tex"))

temp <- list()
for(ii_string in strings) {
  temp[[length(temp) + 1]] <-
  aggregate(. ~ correlation + process + observation + ngroups,
  data = subset(test[test[[ii_string]] == 0, ],
    n == max(test$n) &
    processdiag == 1.0 &
    autocorrelation == 0 &
    correlation %in% range0(test$correlation) &
    process %in% range0(test$process),
    select = c(
      grep(ii_string, colnames(test), value = TRUE),
      "process", "observation", "correlation", "ngroups")),
  median)
  temp[[length(temp)]]$name <- ii_string
  colnames(temp[[length(temp)]]) <- gsub(ii_string, "", colnames(temp[[length(temp)]]))
}
temp <- do.call("rbind", temp)
temp <- temp[ -c(grep("r[[:digit:]]", colnames(temp))[-1])]
temp <- temp[, -which(colnames(temp) == "")]
temp <- temp[, -grep("b11|.22|q11|q12", colnames(temp))]
colnames(temp) <-gsub("^_(.)(.)(.)", "\\$\\1_\\{\\2,\\3\\}\\$", colnames(temp))
temp$name <- gsub("\\.unconstrained\\.", " ", temp$name)
temp <- with(temp, temp[order(correlation, process, observation, name), ])

xtable::print.xtable(xtable::xtable(temp),
  include.rownames = FALSE, include.colnames = TRUE,
  sanitize.colnames.function = identity,
  file = filename,
  sanitize.text.function=function(x){x})
system(paste("pandoc ", filename, "-o", gsub("tex", "doc", filename)))

#### Table_MARSSconvergence
filename <- file.path(dir.results,
  paste0(my.name, "_table_MARSSconvergence.tex"))

groups <- c("n", "ngroups", "correlation", "processdiag", "process", "observation")
temp <- lapply(strings, function(x) {
  returnme <- aggregate(test[, x], (test[, groups]), FUN = function(x) {
    length(which(x == 0)) / length(x)
  })
  colnames(returnme)[NCOL(returnme)] <- x
  return(returnme)
})

temp <- Reduce(function(...) merge(..., all = TRUE), temp)
temp <- aggregate(. ~ n + ngroups + processdiag + observation,
  data = temp[, !colnames(temp) %in% c("correlation", "process")],
  median)
temp <- temp[with(temp, order(n, ngroups, processdiag)), ]
colnames(temp)[1:4] <- c("n", "n observations",
  "process error", "observation error")
colnames(temp) <- gsub("\\.unconstrained\\.", " ", colnames(temp))
xtable::print.xtable(xtable::xtable(temp),
  include.rownames = FALSE, include.colnames = TRUE,
  sanitize.colnames.function = identity,
  file = filename)
system(paste("pandoc ", filename, "-o", gsub("tex", "doc", filename)))

#### Plots

#### Plot_exampletimeseries
plot_exampletimeseries(forvalues = c(-0.9, 0, 0.9),
  plotcolours = my.graycolours,
  directory = dir.results)

#### Plot_runningmean
temp <- subset(test,
  process == min(test$process) &
  autocorrelation == min(test$autocorrelation) &
  n == min(test$n) &
  observation == max(test$observation) &
  processdiag == max(test$processdiag) &
  ngroups == 1)
for (ii_string in c("ccf.p..1", "fixed.unconstrained.zero_b21")) {
xx <- tapply(temp[, ii_string], temp$correlation, function(x) cumsum(x)/seq(along=x))
png(file.path(dir.results,
  paste0(my.name, "_runningmean_", ii_string, ".png")))
matplot(t(do.call("rbind", xx)), type = "l", las = 1,
  ylab = "cummulative mean", xlab = "iteration",
  col = seq_along(unique(test$correlation)), lty = 1,
  main = ifelse(grepl("ccf", ii_string), "cross correlation", "MARSS"))
legend(legend = unique(test$correlation), bty = "n", "topright",
  lty = 1, col = seq_along(unique(test$correlation)))
dev.off()
}

#### Plot_generalcomparison
for(ii_year in unique(test$n)) {
temp <- subset(test,
  autocorrelation %in% range0(test$autocorrelation) &
  correlation %in% range0(test$correlation) &
  process %in% range0(test$process) &
  fixed.unconstrained.zero == 0 &
  ngroups == 1 &
  observation == min(test$observation) &
  processdiag == max(test$processdiag) &
  n == ii_year)
temp$autocorrelation2 <- paste("a =", temp$autocorrelation)
temp$correlation2 <- paste("i =", temp$correlation)
temp$process2 <- paste("c =", temp$process)
df2 <- aggregate(ccf.p.0 ~ autocorrelation2 + correlation2 + process2, temp, median)
df1 <- aggregate(cor.p ~ autocorrelation2 + correlation2 + process2, temp, median)
df3 <- aggregate(fixed.unconstrained.zero_q21 ~
  autocorrelation2 + correlation2 + process2 + process + autocorrelation, temp, median)
df4 <- aggregate(ar1 ~ autocorrelation2 + correlation2 + process2, temp, median)
df5 <- aggregate(fixed.unconstrained.zero_b11 ~ autocorrelation2 + correlation2 + process2, temp, median)
df6 <- aggregate(fixed.unconstrained.zero_b22 ~ autocorrelation2 + correlation2 + process2, temp, median)

g <- ggplot(temp) +
  geom_freqpoly(binwidth = 0.1, aes(x = ccf.p.0), lty = 2) +
  geom_freqpoly(binwidth = 0.1, aes(x = fixed.unconstrained.zero_q21)) +
  geom_freqpoly(binwidth = 0.1, aes(x = cor.p), lty = 3) +
  xlab("") +
  #facet_grid(y ~ x) +
  facet_grid(correlation2 + autocorrelation2 ~ process2) +
  geom_text(data = df1, y = Inf, vjust = 1, aes(x = -1, label = round(cor.p,2))) +
  geom_text(data = df2, y = Inf, vjust = 1, aes(x = 0, label = round(ccf.p.0,2))) +
  geom_text(data = df3, y = Inf, vjust = 1, aes(x = 1, label = round(fixed.unconstrained.zero_q21,2))) +
  geom_vline(data = df3, aes(xintercept = process), lty = 2, col = "red") +
  my.theme3 +
  geom_rect(data = data.frame(
    autocorrelation2 = rep(unique(temp$autocorrelation2), length.out = 9),
    correlation2 = "i = 0",
    process2 = rep(unique(temp$process2), each = 3)),
    fill = "black",
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.09,
    show.legend = FALSE)
filename <- file.path(dir.results,
  paste0(my.name, "_figure_generalcomparison", ii_year, ".png"))
ggsave(filename = filename,
  plot = g, height = 8, width = 15)

g <- ggplot(temp) +
  geom_freqpoly(binwidth = 0.1, aes(x = ar1), lty = 2) +
  geom_freqpoly(binwidth = 0.1, aes(x = fixed.unconstrained.zero_b11)) +
  geom_freqpoly(binwidth = 0.1, aes(x = fixed.unconstrained.zero_b22), lty = 3) +
  xlab("") +
  #facet_grid(y ~ x) +
  facet_grid(correlation2 + process2 ~ autocorrelation2, scales = "free") +
  geom_vline(data = df3, aes(xintercept = autocorrelation), lty = 2, col = "red") +
  my.theme3 +
  geom_rect(data = data.frame(
    autocorrelation2 = rep(unique(temp$autocorrelation2), length.out = 9),
    correlation2 = "i = 0",
    process2 = rep(unique(temp$process2), each = 3)),
    fill = "black",
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.09,
    show.legend = FALSE)
filename <- file.path(dir.results,
  paste0(my.name, "_figure_autocorrelation", ii_year, ".png"))
ggsave(filename = filename,
  plot = g, height = 8, width = 15)
}

#### Plot Pearson's correlation
for (ii in unique(test$n)) {
for (iii in unique(test$processdiag)) {
  temp <- droplevels(subset(test,
    observation == min(test$observation) &
    ngroups == 1 &
    processdiag == iii &
    n == ii))
  temp <- aggregate(temp$cor.s,
    list(temp$autocorrelation, temp$correlation, temp$process),
    median)
  g <- ggplot(data =  temp,
    aes(x = factor(Group.2), y = factor(Group.3))) +
    geom_raster(aes(fill = x)) +
    facet_grid(. ~ Group.1) +
    scale_fill_gradient2(low = "#f1a340", midpoint = 0,
      mid = "#f7f7f7", high = "#998ec3",
      limits = c(-1, 1)) +
    my.theme + my.theme2 +
    theme(strip.text.y = element_text(size = rel(0))) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    xlab("interaction (inner) x autocorrelation") +
    ylab("process error correlation") +
    labs(fill = "Pearson\ncorrelation") +
    geom_rect(aes(xmin = 3.5, xmax = 4.5, ymin = 0.5, ymax = 5.5),
      fill = NA, colour = "black", lty = 1, lwd = 1.5) +
    geom_rect(data = temp[temp$Group.1 == 0, ], aes(xmin = 0.5, xmax = 7.45, ymin = 0.5, ymax = 5.5),
      fill = NA, colour = "black", lty = 2, lwd = 1.5)
    ggsave(
      filename = file.path(dir.results, paste0("correlation_n-", ii, "_perror-", iii,  ".png")),
      plot = g, height = 8, width = 15)
}}

#### Plot_ngroupsmarss
for (ii_string in c(
  "fixed.unconstrained.zero_b11",
  "fixed.unconstrained.zero_r11")) {
  ii_string2 <- gsub("zero", "equal", ii_string)
  ii_string3 <- ifelse(grepl("_b21", ii_string), "corelation",
    ii_string)
  ii_string3 <- ifelse(grepl("_q", ii_string), "process", ii_string3)
  ii_string3 <- ifelse(grepl("_b11|_b22", ii_string3), "autocorrelation", ii_string3)
  ii_string3 <- ifelse(grepl("_r", ii_string3), "observation", ii_string3)

temp <- subset(test,
  observation == max(test$observation) &
  processdiag == max(test$processdiag) &
  process == 0)
temp <- rbind(
  temp[temp[,
    strsplit(ii_string2, "_")[[1]][1]] == 0 &
    temp$ngroups == 1 &
    temp$n == max(temp$n), ],
  temp[temp[,
    strsplit(ii_string2, "_")[[1]][1]] == 0 &
    temp$ngroups == 2 &
    temp$n == min(temp$n), ]
)
temp$autocorrelation2 <- paste("autocorrelation = ", temp$autocorrelation)
if (grepl("_r11$", ii_string)) {
  temp <- temp[temp[, ii_string2] < 100, ]
}
g <- ggplot(temp, aes(x = as.factor(correlation), fill = as.factor(ngroups))) +
  geom_boxplot(aes_string(y = ii_string2)) +
  facet_grid(~ autocorrelation2) +
  my.theme +
  # ylim(c(0.5,1)) +
  ylab("") + xlab("interaction") +
  scale_fill_grey(start = 0.8, end = 0.4,
    guide_legend(title = "observations"))
if (ii_string3 == "observation") {
  g <- g + ylab("observation error variance") +
    geom_hline(aes(yintercept = observation), lty = 2, col = "red") +
    theme(legend.position = c(0.15, 0.05), legend.direction = "horizontal")
} else {
  g <- g +
  geom_point(aes_string(y = ii_string3), col = "red", cex = 2,
    show.legend = FALSE) +
  ylab(bquote(b[1][1]))
}
ggsave(filename = file.path(dir.results,
  paste0(my.name, "_ngroupsmarss_process0_", ii_string3, ".png")),
  plot = g, height = 5, width = 8)
}

#### Plot_estimatevsfixed
for (ii_string in c("fixed.unconstrained.zero_b21", "fixed.unconstrained.zero_q21")) {
  ii_string2 <- gsub("zero", "equal", ii_string)
  ii_string3 <- ifelse(grepl("_b21", ii_string), "correlation",
    ii_string)
  ii_string3 <- ifelse(grepl("_q", ii_string), "process", ii_string3)
  ii_string3 <- ifelse(grepl("_b11|_b22", ii_string3), "autocorrelation", ii_string3)
  ii_string3 <- ifelse(grepl("_r", ii_string3), "observation", ii_string3)

temp <- subset(test,
  # process == 0 &
  processdiag == max(temp$processdiag)
  )
temp$process2 <- paste("cor due to\nprocess error = ", temp$process)
temp$autocorrelation2 <- paste("autocorrelation = ", temp$autocorrelation)

g <- ggplot(temp, aes(x = as.factor(correlation), fill = interaction(n, as.factor(ngroups)))) +
  geom_boxplot(data = temp[
    temp[, strsplit(ii_string2, "_")[[1]][1]] == 0, ],
    aes_string(y = ii_string2))+
  scale_fill_grey(start = 0.8, end = 0.4,
    guide_legend(title = "n.observations")) +
  geom_boxplot(data = temp[
    temp[, strsplit(ii_string, "_")[[1]][1]] == 0 &
    temp$ngroups == 1 &
    temp$n == max(temp$n), ],
    aes_string(y = ii_string), fill = "white", alpha = 0.2, col = "blue", width = 0.5) +
  facet_grid(process2 ~ autocorrelation2) +
  geom_point(aes_string(y = ii_string3), col = "red", cex = 2, show.legend = FALSE) +
  guides(shape = "none", colour = "none", size = "none") +
  my.theme3 +
  ylab(paste("estimated",
    ifelse(ii_string3 == "correlation", "interactions", "cor due to process error"))) +
  xlab("interaction") +
  theme(strip.text.y = element_text(angle = 90))

ggsave(filename = file.path(dir.results,
  paste0(my.name, "_figure_estimatevsfixed_", ii_string3, ".png")),
  plot = g, height = 8, width = 8)
}

#### Plot_ccfvsmarsswobserror
temp <- test[test$fixed.unconstrained.zero == 0 & test$ngroups == 1 & test$n == max(test$n),
  -which(grepl("ccf\\.s", colnames(test)))]
temp <- temp[, apply(temp, 2, function(x) !all(is.na(x)))]
temp <- temp[, -which(grepl("p\\.[1-2]|equal|arima|cor$|r[[:digit:]]{2}", colnames(temp)))]
colnames(temp) <- gsub(".+\\.([[:digit:]])", "par_\\1", colnames(temp))
colnames(temp) <- gsub("fixed.unconstrained.zero_(.+)", "par_\\1", colnames(temp))
yes <- reshape(data = temp, direction = "long",
  varying = grep("_", colnames(temp)),
  idvar = c("correlation", "process"), sep = "_",
  new.row.names = 1:100000000)
yes <- yes[!grepl("11|2$", yes$time), ]

yes$process2 <- yes$process
yes$process <- paste("cor due to\nprocess error = ", yes$process)
yes$autocorrelation <- paste("autocorrelation = ", yes$autocorrelation)

for (ii_set in c("q|^0$", "b|^1$")) {
g <- ggplot(data = yes[!grepl(ii_set, yes$time), ],
  aes(x = as.factor(correlation), y = par)) +
  geom_boxplot(aes(lty = as.factor(time), fill = as.factor(observation)), lwd = 0.5) +
  facet_grid(process ~ autocorrelation, scales = "fixed") +
  scale_linetype_manual(values = c(1, 3)) +
  guides(linetype = "none") +
  #ylim(c(-1.75, 1.75)) +
  scale_fill_grey(start = 0.8, end = 0.4,
    guide_legend(title = "obs error")) +
  xlab("interaction") +
  ylab("parameter estimates") +
  theme(legend.position = c(0.15, 0.3)) +
  my.theme3 +
  theme(legend.direction = "horizontal",
    strip.text.y = element_text(angle = 90))
if (ii_set == "q|^0$") {
  g <- g + geom_boxplot(aes(y = correlation), col = "red", cex = 0.25, lty = 2, show.legend = FALSE) +
    theme(legend.position = c(0.15, 0.2605))
} else {
  g <- g + geom_hline(aes(yintercept = process2), col = "red", lty = 2, show.legend = FALSE)  +
    theme(legend.position = c(0.15, 0.06))
}
ggsave(file.path(dir.results,
  paste0(my.name, "_figure_ccfvsmarsswobserror_", ifelse(substring(ii_set, 1, 1) == "b", "q", "b"), ".png")),
  plot = g, height = 8, width = 8)
}

#### Plot_stdvsraw
temp <- rbind(
  data.frame(test, "std" = "raw"),
  data.frame(withstd, "std" = "standardized"))
temp <- subset(temp,
  n == max(test$n) &
  observation == min(test$observation) &
  processdiag == max(test$processdiag) &
  ngroups == 1 &
  fixed.unconstrained.zero == 0)
temp <- temp[, c("process", "autocorrelation", "correlation",
  "observation", "processdiag", "n", "ngroups", "iteration", "std",
  "ccf.p..1", "ccf.p.0", "fixed.unconstrained.zero_b21", "fixed.unconstrained.zero_q21")]
colnames(temp) <- gsub("ccf.+([[:digit:]])|fixed.+_(.+)", "par_\\1\\2", colnames(temp))
yes <- reshape(data = temp, direction = "long",
  varying = grep("_", colnames(temp)),
  idvar = c("std", "iteration"), sep = "_", new.row.names = 1:10000000)

yes$process2 <- paste("cor due to\nprocess error = ", yes$process)
yes$correlation2 <- paste("interaction = ", yes$correlation)
yes$autocorrelation2 <- paste("autocorrelation = ", yes$autocorrelation)

g <- ggplot(yes[yes$time %in% c("1", "b21"), ]) +
  geom_boxplot(aes(x = as.factor(process),
    lty = time, fill = as.factor(std), y = par), lwd = 0.5) +
  scale_linetype_manual(values = c(1, 3)) +
  scale_fill_grey(start = 0.8, end = 0.4,
    guide_legend(title = "")) +
  guides(linetype = "none") +
  facet_grid(correlation2 ~ autocorrelation2) +
  xlab("correlations due to process error") +
  ylab(paste("parameter estimates")) +
  my.theme3 +
  theme(legend.position = c(0.15, 0.07),
    legend.direction = "horizontal",
    strip.text.y = element_text(angle = 90)) +
  geom_hline(aes(yintercept = correlation), col = "red", lty = 2, show.legend = FALSE)
ggsave(file.path(dir.results,
  paste0(my.name, "_figure_stdvsraw_b.png")),
  plot = g, height = 8, width = 8)

g <- ggplot(yes[yes$time %in% c("0", "q21"), ]) +
  geom_boxplot(aes(x = as.factor(correlation),
    lty = time, fill = as.factor(std), y = par), lwd = 0.5) +
  scale_linetype_manual(values = c(1, 3)) +
  scale_fill_grey(start = 0.8, end = 0.4,
    guide_legend(title = "")) +
  guides(linetype = "none") +
  facet_grid(process2 ~ autocorrelation2) +
  xlab("interaction") +
  ylab(paste("parameter estimates")) +
  my.theme3 +
  theme(legend.position = c(0.15, 0.07), legend.direction = "horizontal",
    strip.text.y = element_text(angle = 90)) +
  geom_hline(aes(yintercept = process), col = "red", lty = 2, show.legend = FALSE)
  ggsave(file.path(dir.results,
    paste0(my.name, "_figure_stdvsraw_q.png")),
    plot = g, height = 8, width = 8)


