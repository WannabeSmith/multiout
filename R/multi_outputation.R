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
  print("Generating and transposing list of samples...")
  samples.list <- lapply(split(data, data[, id]), function(sub.data){
    sub.sample <- sub.data[sample(nrow(sub.data), M, replace = TRUE),]

    # Turn each sub-sample into a list of rows
    # so the final samples.list can be transposed efficiently
    return(split(sub.sample, seq(nrow(sub.sample))))
  })

  samples.list.tr <- transpose(samples.list)
  print("Generating and transposing list of samples - DONE")

  build.and.analyze <- function(x){
    # Create the outputation sub-dataset
    out.data <- do.call(rbind, x)

    # Analyze the data
    fn(out.data)
  }

  print("Performing outputation...")
  outputated <- lapply(samples.list.tr, build.and.analyze)
  print("Performing outputation - DONE")

  # TODO: Deal with variance estimate (not necessary for our project)

  if (leave.as.list)
  {
    return(outputated)
  } else
  {
    return(colMeans(do.call(rbind, outputated)))
  }
}
