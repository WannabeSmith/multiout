#' Perform multiple outputation
#'
#' Perform multiple outputation on a dataset to obtain estimates of generic parameters
#'
#' @importFrom purrr transpose
#' @param fn a function to apply to each sub-data set
#' @param M the (positive integer) number of outputations to perform
#' @param data the data.frame
#' @param id a string of the name describing each cluster
#' @param leave.as.list a logical indicating whether the results should be left as a list
#' @return A vector (or list of vectors in the case of leave.as.list = TRUE) containing the outputated estimates
#' @export
multiout <- function(fn, M, data, id, leave.as.list = FALSE)
{
  setDT(data)

  print("Generating and transposing list of samples...")
  # For each id, sample M times from the subdataset for that id (with replacement)
  samples.DT <- data[, .SD[sample(nrow(.SD), M, replace = TRUE)], by = id]
  # Assign a multiple outputation index to each row within each id group
  # (Note: can just do 1:.N since sampling was random)
  samples.DT[, MO_idx := 1:.N, by = id]

  # Get a list of sub-datasets to apply fn over
  MO.list <- split(samples.DT, by = "MO_idx")
  # Coerce to data.frame just in case the user passes a function that can't deal with data.tables
  MO.list.DF <- lapply(MO.list, as.data.frame)
  print("Generating and transposing list of samples - DONE")

  print("Performing outputation...")
  # Apply the function to each sub-dataset
  outputated <- lapply(MO.list.DF, fn)
  print("Performing outputation - DONE")

  # TODO: Deal with variance estimate

  if (leave.as.list)
  {
    return(outputated)
  } else
  {
    outputated <- do.call(rbind, outputated)
    Sigma <- var(outputated)
    mu <- colMeans(outputated)
    return(list(mu = mu, Sigma = Sigma))
  }
}
