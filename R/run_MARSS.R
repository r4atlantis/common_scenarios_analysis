#' Run a MARSS model
#'
#' todo: place more details here.
#'
#' @param data
#'
#' @author Kelli Faye Johnson
#'
#' @examples
#' #data <- subset(temp, Code %in% c("FVS") & region == "CalCu_" &
#' #scenario == "BC" & Time > 0)[, c("Time", "Mass.catch", "Mass.bio")]
#' #test <- run_MARSS(data = data,
#' #attributes = "Mass.bio", indicators = "Mass.catch")

run_MARSS <- function(data, attributes, indicators) {

  # Missing values should be NA
  # todo: think about the need to standardize values
  # (x - mean) / sd
  # (data - apply(data, 1, mean, na.rm = TRUE)) /
  # sqrt(apply(data, 1, var, na.rm = TRUE))

  # Control variables for the MARSS model
  cntl <- list(allow.degen = FALSE, maxit = 2000,
    safe = TRUE)

  # Use the attribute and indicator names to determine
  # the B matrix, where attributes can be correlated
  # with attributes, but indicators cannot affect attributes,
  # i.e., a = attribute i = indicator c = correlation
  # a 0
  # c i
  combined <- c(attributes, indicators)
  B <- list()
  for (it_col in 1:length(combined)) {
    for (it_row in 1:length(combined)) {
      # if (it_col == it_row) next
      if (combined[it_col] %in% indicators &
          combined[it_row] %in% attributes) {
          B <- append(B, 0)
      } else {
        B <- append(B, sprintf("%s:%s", combined[it_row], combined[it_col]))
      }
    }
  }
  B <- matrix(B, nrow = length(combined), ncol = length(combined))

  model <- list(
    #tinitx = 1,
    Z = "identity",
    B = B,
    # B = matrix(list("B:1,1","B:2,1",0,"B:2,2"),2,2),
    Q = "diagonal and unequal", # Proc error ~MVN(0,Q)
    R = "zero" # Obs error ~MVN(0,R)
    )
  # todo: decide if the U and A matrices should be used for
  # region or scenario.

  # reshape the data
  if (!"Time" %in% colnames(data)) stop("Time needs to be a",
    " column in your data frame.")
  data <- data[order(data$Time), ]
  data_model <- t(data[, c("Time", combined)])[-1, ]

  results <- MARSS::MARSS(data_model, model = model, control = cntl,
    silent = TRUE, fit = TRUE, method = "kem")
  #todo: save the results to the disk
  cis <- MARSS::MARSSparamCIs(results)

  return(cis)
}
