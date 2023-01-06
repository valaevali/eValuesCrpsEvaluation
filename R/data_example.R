#' This method caculates the e-values for the input data
#'
#' @param input.data of the form list("firstset" = dataset1, "secondset" = dataset2,...) where the dataset has the form
#' icu     los date                idr            rq             cox
#' <fct> <dbl> <dttm>              <list>         <list>         <list>
#' and each prediction model has at least the form
#' data[1,]$rq
#' [[1]]
#' points   cdf
#' @param method = list("GRAPA", "lambda", "alternative", "alternative-mean"), is a list containing all the method names for calculating the different lambdas.
#' @param lambda = 0.5, lambda entry for a fixed value.
#' @param p.value.method = "t", can be "t" for t-test and "dm" for dm.test of the package forecast.
#' @param file.folder = getwd(), where to save the output to.
#'
#' @export
calculate_e_values_data_example <- function(input.data, method = list("GRAPA", "lambda", "alternative"),
                                            lambda = 0.5, p.value.method = "t", file.folder = getwd()) {
  n.it <- length(input.data)
  start.time <- Sys.time()
  cl <- parallel::makeCluster(parallel::detectCores() - 2, type = "PSOCK")
  doSNOW::registerDoSNOW(cl = cl)
  on.exit(parallel::stopCluster(cl = cl))
  pb <- progress::progress_bar$new(total = n.it + 1)

  # allowing progress bar to be used in dopar
  opts <- list(progress = \() { pb$tick(1) })

  pb$tick(1)
  print(paste("Starting to caclulate the e-values at:", start.time))
  result <- foreach::foreach(i = seq(1, n.it), .combine = \(x, y) { data.table::rbindlist(list(x, y)) }, .options.snow = opts, .packages = c("dplyr", "eValuesCrps")) %dopar% {

    data.run <- input.data[i][[1]]
    obs <- data.run$los

    forecasts <- list("idr" = list("points.cdf" = data.run$idr),
                      "rq" = list("points.cdf" = data.run$rq),
                      "cox" = list("points.cdf" = data.run$cox))

    temp <- outer(forecasts, forecasts, Vectorize(\(f, g) {
      if (!identical(f, g)) {
        return(e_value(y = obs, crps.F.para = f, crps.G.para = g, it = i, method = method, lambda = lambda, p.value.method = p.value.method))
      }
    }))
    temp.tibble <- tibble::as_tibble(temp)
    temp.tibble <- temp.tibble %>%
      dplyr::mutate(names.F = rownames(temp),
                    icu = names(input.data[i])) %>%
      tidyr::pivot_longer(cols = colnames(temp.tibble), names_to = "names.G") %>%
      tidyr::unnest_wider(value) %>%
      na.omit(p.value)
    temp.tibble
  }

  timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H-%M-%S")
  result.eval <- result %>%
    group_by(names.F, names.G, icu) %>%
    mutate(across(contains(c("p.value", "prod")), ~100 * sum(.x <= 0.05), .names = "{.col}.H0.rej")) %>%
    select(contains(c("names", "icu", "H0"))) %>%
    distinct() %>%
    arrange(icu, names.F, names.G)

  print(Sys.time() - start.time)
  result.fin <- list("uncompacted" = result, "evaluated" = result.eval)

  saveRDS(result.fin, paste0(file.folder, "/target/run-data-example-", nrow(input.data[1][[1]]),"-", timestamp, ".rds"))

  return(result.fin)
}