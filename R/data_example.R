#' This method caculates the e-values for the input data
#'
#' @param input.data of the form list("firstset" = dataset1, "secondset" = dataset2,...) where the dataset has the form
#' icu     los date                idr            rq             cox
#' <fct> <dbl> <dttm>              <list>         <list>         <list>
#' and each prediction model has at least the form
#' data[1,]$rq
#' [[1]]
#' points   cdf
#' @param method = list("GRAPA", "lambda", "alt-conf", "alt-cons", "alt-more-cons"), is a list containing all the method names for calculating the different lambdas.
#' @param lambda = 0.5, lambda entry for a fixed value.
#' @param p.value.method = "t", can be "t" for t-test and "dm" for dm.test of the package forecast.
#' @param file.folder = getwd(), where to save the output to.
#' @param f name of first forecaster
#' @param g name of second forecaster
#'
#' @export
calculate_e_values_data_example_for_each <- function(input.data, crps.F.para, crps.G.para, method = list("alt-cons"),
                                                     lambda = 0.5, p.value.method = NA, file.folder = getwd(), f, g) {
  n.it <- nrow(input.data)
  input.data <- input.data %>% arrange(date)

  start.time <- Sys.time()

  print(paste("Starting to caclulate the e-values at:", start.time))
  pb <- progress::progress_bar$new(total = n.it, show_after = 0, force = T, clear = F)
  # allowing progress bar to be used in dopar
  pb$tick(0)

  sequential.run <- rep(NA, n.it)
  first.run <- e_value(y = input.data[1,]$los, crps.F.para = list("points.cdf" = crps.F.para[1]), crps.G.para = list("points.cdf" = crps.G.para[1]), method = method, lambda = lambda, p.value.method = p.value.method)
  sequential.run[1] <- list(first.run)
  pb$tick(1)
  for (t in 2:n.it) {
    first.run <- e_value(old.run.e.value = first.run, new.y = input.data[t,]$los, new.crps.F.para = crps.F.para[t], new.crps.G.para = crps.G.para[t])
    sequential.run[t] <- list(first.run)
    pb$tick(1)
  }
  temp <- tibble("names.F" = f, "names.G" = g, "date" = input.data[, 3]$date, sequential.run)
  result <- temp %>%
    tidyr::unnest_wider(sequential.run)%>%
    select(names.F, names.G, date, e.value.alt.cons.prod)

  timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H-%M-%S")
  print(Sys.time() - start.time)

  fst::write.fst(result, path = paste0(file.folder, "/target/run-data-example-", (input.data[1,]$icu), "-", f, "vs", g, "-", timestamp, ".fst"))

  return(result)
}