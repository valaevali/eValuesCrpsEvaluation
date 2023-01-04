# detach("package:eValuesCrps", unload = TRUE)
# devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")
library("dplyr")
library("scoringRules")
library("DT")
library("doParallel")
library("doSNOW")
library("progress")
library("data.table")


data.example.dt <- load(file=paste0(getwd(), "/data/dim_data_master_thesis.rda"))

# Data model
#  icu     los date                idr            rq             cox
#  <fct> <dbl> <dttm>              <list>         <list>         <list>
# los are the observations

# 3 prediction models
# data[1,]$idr --
# [[1]]
# points lower   cdf upper

# data[1,]$rq -- quantile regression?
# [[1]]
# points   cdf

# data[1,]$cox -- cox regression
# [[1]]
# points   cdf

# points => where the cdfs jump

data.first <- data[1:10,]

obs <- data.first$los
idr <- list("points" = points.idr <- cbind(sapply(1:10, \(i) {data.first$idr[[i]]$points})), "cdf" = points.idr <- cbind(sapply(1:10, \(i) {data.first$idr[[i]]$cdf})))
rq <- list("points" = points.idr <- cbind(sapply(1:10, \(i) {data.first$rq[[i]]$points})), "cdf" = points.idr <- cbind(sapply(1:10, \(i) {data.first$rq[[i]]$cdf})))
cox <- list("points" = points.idr <- cbind(sapply(1:10, \(i) {data.first$cox[[i]]$points})), "cdf" = points.idr <- cbind(sapply(1:10, \(i) {data.first$cox[[i]]$cdf})))
forecasts.data.first <- list("idr" = list("points" = idr$points, "cdf" = idr$cdf),
                             "rq" = list("points" = rq$points, "cdf" = rq$cdf),
                             "cox" = list("points" = cox$points, "cdf" = cox$cdf))
e.value.data.first <- e_value(obs, forecasts.data.first$idr, forecasts.data.first$rq, p.value.method = "t")

crps.F.para <- forecasts.data.first$idr
crps.G.para <- forecasts.data.first$rq


##

data.to.calc <- data %>% select(-date)
input.list <- list("ICU10" = data.to.calc %>% filter(icu == "ICU10"),
                   "ICU44" = data.to.calc %>% filter(icu == "ICU44"),
                   "ICU65" = data.to.calc %>% filter(icu == "ICU65"),
                   "ICU76" = data.to.calc %>% filter(icu == "ICU76"),
                   "ICU77" = data.to.calc %>% filter(icu == "ICU77")
                   )
result <- calculate_e_values_data_example(input.list)