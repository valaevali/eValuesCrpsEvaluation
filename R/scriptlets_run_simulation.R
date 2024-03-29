# detach(name = "package:eValuesCrps", unload = TRUE)
devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")
library("dplyr")
library("scoringRules")
library("DT")
library("doParallel")
library("doSNOW")
library("progress")
library("data.table")

n.it <- 1000

dt.l.p.f.10 <- sim_e_values(n.obs = 10, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE, prefix = "l-p")
dt.u.f.10 <- sim_e_values(n.obs = 10, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.l.p.f.25 <- sim_e_values(n.obs = 25, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE, prefix = "l-p")
dt.u.f.25 <- sim_e_values(n.obs = 25, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.l.p.f.50 <- sim_e_values(n.obs = 50, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE, prefix = "l-p")
dt.u.f.50 <- sim_e_values(n.obs = 50, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.l.p.f.100 <- sim_e_values(n.obs = 100, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE, prefix = "l-p")
dt.u.f.100 <- sim_e_values(n.obs = 100, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.l.p.f.300 <- sim_e_values(n.obs = 300, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE, prefix = "l-p")
dt.u.f.300 <- sim_e_values(n.obs = 300, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)

f.dt.l.p.o.10 <- getFile("/target/run-l-p-10-1000-2023-01-26T16-27-39.rds")
f.dt.u.f.10 <- getFile("/target/run-u-f-10-1000-2023-01-26T16-17-18.rds")
f.dt.l.p.o.25 <- getFile("/target/run-l-p-25-1000-2023-01-26T16-28-25.rds")
f.dt.u.f.25 <- getFile("/target/run-u-f-25-1000-2023-01-26T16-18-07.rds")
f.dt.l.p.o.50 <- getFile("/target/run-l-p-50-1000-2023-01-26T16-29-21.rds")
f.dt.u.f.50 <- getFile("/target/run-u-f-50-1000-2023-01-26T16-19-15.rds")
f.dt.l.p.o.100 <- getFile("/target/run-l-p-100-1000-2023-01-26T16-30-13.rds")
f.dt.u.f.100 <- getFile("/target/run-u-f-100-1000-2023-01-26T16-21-09.rds")
f.dt.l.p.o.300 <- getFile("/target/run-l-p-300-1000-2023-01-26T16-31-25.rds")
f.dt.u.f.300 <- getFile("/target/run-u-f-300-1000-2023-01-26T16-25-34.rds")

dt.l.p.f.10 <- f.dt.l.p.o.10
dt.u.f.10 <- f.dt.u.f.10
dt.l.p.f.25 <- f.dt.l.p.o.25
dt.u.f.25 <- f.dt.u.f.25
dt.l.p.f.50 <- f.dt.l.p.o.50
dt.u.f.50 <- f.dt.u.f.50
dt.l.p.f.100 <- f.dt.l.p.o.100
dt.u.f.100 <- f.dt.u.f.100
dt.l.p.f.300 <- f.dt.l.p.o.300
dt.u.f.300 <- f.dt.u.f.300

printPlot(paste0("plot-300-", n.it, "-l-p-o-rej-rate"), print_rej_rate_perfect_loosing_power, dt.l.p.f.300)
printPlot(paste0("plot-300-", n.it, "-l-p-o-e-values-hist"), print_e_values_histogram_loosing_power, dt.l.p.f.300)
printPlot(paste0("plot-300-", n.it, "-l-p-o-crps-dif-hist"), print_crps_diff_histogram_loosing_power, dt.l.p.f.300)