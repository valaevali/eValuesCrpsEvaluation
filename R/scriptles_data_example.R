detach("package:eValuesCrps", unload = TRUE)
devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")
library("dplyr")
library("scoringRules")
library("DT")
library("doParallel")
library("doSNOW")
library("progress")
library("data.table")


data.example.dt <- load(file = paste0(getwd(), "/data/dim_data_master_thesis.rda"))

# Data model
#  icu     los date                idr            rq             cox
#  <fct> <dbl> <dttm>              <list>         <list>         <list>
# los are the observations

# 3 prediction models
# data[1,]$idr -- isotonic distributional regression
# [[1]]
# points lower   cdf upper

# data[1,]$rq -- quantile regression
# [[1]]
# points   cdf

# data[1,]$cox -- cox regression
# [[1]]
# points   cdf

# points => where the cdfs jump

##

data.to.calc <- data %>% select(-date)

get_randomized_input_of_dataset <- function(data, n.obs.max) {
  indx <- sample(nrow(data), n.obs.max)
  data[indx,]
}

calculate_e_values_randomized_dataset <- function(data.to.calc, n.obs.max) {
  input.list.small <- list("ICU10" = get_randomized_input_of_dataset(data.to.calc %>% filter(icu == "ICU10"), n.obs.max),
                           "ICU44" = get_randomized_input_of_dataset(data.to.calc %>% filter(icu == "ICU44"), n.obs.max),
                           "ICU65" = get_randomized_input_of_dataset(data.to.calc %>% filter(icu == "ICU65"), n.obs.max),
                           "ICU76" = get_randomized_input_of_dataset(data.to.calc %>% filter(icu == "ICU76"), n.obs.max),
                           "ICU77" = get_randomized_input_of_dataset(data.to.calc %>% filter(icu == "ICU77"), n.obs.max)
  )
  result <- calculate_e_values_data_example(input.list.small)
}

log_threshold(DEBUG)
calculate_e_values_randomized_dataset(data.to.calc, 10)
# n.obs.max = 10: Time difference of 6.523377 secs
calculate_e_values_randomized_dataset(data.to.calc, 25)
# n.obs.max = 25: Time difference of 12.11244 secs
calculate_e_values_randomized_dataset(data.to.calc, 50)
# n.obs.max = 50: Time difference of 26.14363 secs
calculate_e_values_randomized_dataset(data.to.calc, 100)
# n.obs.max = 100: Time difference of 59.26821 secs
calculate_e_values_randomized_dataset(data.to.calc, 300)
# n.obs.max = 300: Time difference of 6.801105 mins

######## print

print_data_example_rej_rate <- function(dt.f, f, g) {
  per.icu <- as.data.frame(t(dt.f %>%
                               filter(names.F == f & names.G == g) %>%
                               select(contains("value"))))
  to.print <- per.icu %>%
    mutate(which = rownames(per.icu),
           n_obs = as.numeric(stringr::str_extract(which, "[0-9]{2,3}")),
           which = stringr::str_replace(which, "(.prod|).H0.rej.[0-9]{2,3}", "")
    ) %>%
    select(which, n_obs, rej_rate = V1) %>%
    arrange(n_obs)
  p <- ggplot2::ggplot(to.print, ggplot2::aes(x = n_obs, y = rej_rate, color = which)) +
    ggplot2::geom_line(ggplot2::aes(group = which), size = 0.5) +
    ggplot2::scale_x_continuous(limits = c(10, 300), breaks = c(10, 25, 50, 100, 300)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::geom_line(data = filter(to.print, which == "p.value"), size = 1) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(paste(f, "vs", g)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("k") +
    ggplot2::ylab("Rej. rate")
  return(p)
}

print_for_each_k_data_rej_rate <- function(dt.f.input, icu, n.it) {
  dt.f <- dt.f.input %>%
    filter(icu == icu) %>%
    select(-(icu))
  idr.rq <- print_data_example_rej_rate(dt.f, 'idr', 'rq')
  idr.cox <- print_data_example_rej_rate(dt.f, 'idr', 'cox')

  rq.idr <- print_data_example_rej_rate(dt.f, 'rq', 'idr')
  rq.cox <- print_data_example_rej_rate(dt.f, 'rq', 'cox')

  cox.idr <- print_data_example_rej_rate(dt.f, 'cox', 'idr')
  cox.rq <- print_data_example_rej_rate(dt.f, 'cox', 'rq')

  png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_data_example_rej_rate_it-", n.it, "-icu-", icu, "_", format(Sys.time(), format = "%d-%m"), ".png"), height = 650, width = 600)
  g <- ggpubr::ggarrange(idr.rq, idr.cox,
                         rq.idr, rq.cox,
                         cox.idr, cox.rq,
                         ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom")
  t <- ggpubr::annotate_figure(g, top = ggpubr::text_grob(icu,
                                                          color = "red", face = "bold", size = 24))
  print(t)
  dev.off()
}

dt.data.ex.10 <- getFile("/target/run-data-example-10-2023-01-05T15-30-28.rds")
dt.data.ex.25 <- getFile("/target/run-data-example-25-2023-01-05T15-30-38.rds")
dt.data.ex.50 <- getFile("/target/run-data-example-50-2023-01-05T15-31-04.rds")
dt.data.ex.100 <- getFile("/target/run-data-example-100-2023-01-05T15-32-03.rds")
dt.data.ex.300 <- getFile("/target/run-data-example-300-2023-01-05T15-38-54.rds")

dt.data.ex.mut.10 <- dt.data.ex.10$evaluated %>%
  mutate(it = 10) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".10")))
dt.data.ex.mut.25 <- dt.data.ex.25$evaluated %>%
  mutate(it = 25) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".25")))
dt.data.ex.mut.50 <- dt.data.ex.50$evaluated %>%
  mutate(it = 50) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".50")))
dt.data.ex.mut.100 <- dt.data.ex.100$evaluated %>%
  mutate(it = 100) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".100")))
dt.data.ex.mut.300 <- dt.data.ex.300$evaluated %>%
  mutate(it = 300) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".300")))
dt.data.ex <- merge(dt.data.ex.mut.10, dt.data.ex.mut.25, by = c("names.F" = "names.F", "names.G" = "names.G", "icu" = "icu"))
dt.data.ex <- merge(dt.data.ex, dt.data.ex.mut.50, by = c("names.F" = "names.F", "names.G" = "names.G", "icu" = "icu"))
dt.data.ex <- merge(dt.data.ex, dt.data.ex.mut.100, by = c("names.F" = "names.F", "names.G" = "names.G", "icu" = "icu"))
dt.data.ex <- merge(dt.data.ex, dt.data.ex.mut.300, by = c("names.F" = "names.F", "names.G" = "names.G", "icu" = "icu"))
dt.data.ex <- dt.data.ex %>%
  select(names.F, names.G, icu, p.value.H0.rej.10, p.value.H0.rej.25, p.value.H0.rej.50, p.value.H0.rej.100, p.value.H0.rej.300,
         e.value.lambda.prod.H0.rej.10, e.value.lambda.prod.H0.rej.25, e.value.lambda.prod.H0.rej.50, e.value.lambda.prod.H0.rej.100, e.value.lambda.prod.H0.rej.300,
         e.value.grapa.prod.H0.rej.10, e.value.grapa.prod.H0.rej.25, e.value.grapa.prod.H0.rej.50, e.value.grapa.prod.H0.rej.100, e.value.grapa.prod.H0.rej.300,
         e.value.alt.conf.prod.H0.rej.10, e.value.alt.conf.prod.H0.rej.25, e.value.alt.conf.prod.H0.rej.50, e.value.alt.conf.prod.H0.rej.100, e.value.alt.conf.prod.H0.rej.300,
         e.value.alt.cons.prod.H0.rej.10, e.value.alt.cons.prod.H0.rej.25, e.value.alt.cons.prod.H0.rej.50, e.value.alt.cons.prod.H0.rej.100, e.value.alt.cons.prod.H0.rej.300,
         e.value.alt.more.cons.prod.H0.rej.10, e.value.alt.more.cons.prod.H0.rej.25, e.value.alt.more.cons.prod.H0.rej.50, e.value.alt.more.cons.prod.H0.rej.100, e.value.alt.more.cons.prod.H0.rej.300
  )

print_for_each_k_data_rej_rate(dt.data.ex, "ICU10", 300)
print_for_each_k_data_rej_rate(dt.data.ex, "ICU44", 300)
print_for_each_k_data_rej_rate(dt.data.ex, "ICU65", 300)
print_for_each_k_data_rej_rate(dt.data.ex, "ICU76", 300)
print_for_each_k_data_rej_rate(dt.data.ex, "ICU77", 300)

###################
# These should return a n x n.obs matrix return
crps.F.para$sample.fun <- \(n) { sapply(seq_along(crps.F.para$points.cdf), \(i) { rcdf_rf(points.cdf = crps.F.para$points.cdf[i][[1]], n = n) }) }
crps.G.para$sample.fun <- \(n) { sapply(seq_along(crps.F.para$points.cdf), \(i) { rcdf_rf(points.cdf = crps.G.para$points.cdf[i][[1]], n = n) }) }

crps.F.para$crps.fun.y.matrix <- \(y) { result <- matrix(nrow = dim(y.sim.conf)[1], ncol = dim(y.sim.conf)[2])
  for (k in 1:dim(y.sim.conf)[1]) {
    for (l in 1:dim(y.sim.conf)[2]) {
      fun.y <- \(i, j) { scoringRules::crps_sample(y = y.sim.conf[i, j], dat = crps.F.para$points.cdf[j][[1]]$points, w = crps.F.para$points.cdf[j][[1]]$cdf) }
      result[k, l] <- fun.y(k, l)
    }
  }
  return(result)
}

crps.G.para$crps.fun.y.matrix <- \(y) { result <- matrix(nrow = dim(y.sim.conf)[1], ncol = dim(y.sim.conf)[2])
  for (k in 1:dim(y.sim.conf)[1]) {
    for (l in 1:dim(y.sim.conf)[2]) {
      fun.y <- \(i, j) { scoringRules::crps_sample(y = y.sim.conf[i, j], dat = crps.F.para$points.cdf[j][[1]]$points, w = crps.F.para$points.cdf[j][[1]]$cdf) }
      result[k, l] <- fun.y(k, l)
    }
  }
  return(result)
}


### inf
dt.run.inf.check <- dt.data.ex.10$uncompacted %>% filter(names.F == 'idr' &
                                                           names.G == 'rq' &
                                                           icu == 'ICU10')
crps.F.para <- dt.run.inf.check$crps.F.fun[[1]]
crps.G.para <- dt.run.inf.check$crps.G.fun[[1]]
inf <- get_inf_crps(crps.F.para, crps.G.para, 10)

print_inf <- function(line, from = -10, to = 5) {
  print(line)
  x <- seq(from, to, by = 0.01)
  y <- sapply(x, \(y) { crps.F.para$fun(y) - crps.G.para$fun(y) })
  plot(x, sort(y), type = "l", col = "red")
  abline(h = inf)
  abline(h = -(inf))
}

print_inf(3, -50, 50)
mins <- sapply(1:n.obs, \(i) { optim_inf_value(\(x) { crps.F.para$inf.fun(x, i) - crps.G.para$inf.fun(x, i) },
                                               min.value = -50, max.value = 50) })
abline(h = mins)


### boxplot

create_boxplot_for_data_example <- function(dt.data.boxplot, icu) {
  p <- ggplot2::ggplot(dt.data.boxplot, ggplot2::aes(x = which, y = as.factor(rejecting), color = which, fill = which)) +
    ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA, lwd = 2) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::scale_fill_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylim("0", "1") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, color = "red"), axis.text = ggplot2::element_text(size = 12),
                   strip.text.x = ggplot2::element_text(size = 12), strip.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(angle = 70, vjust = 0.9, hjust = 1)) +
    ggplot2::xlab("") +
    ggplot2::ylab(latex2exp::TeX("Rejecting $H_0$")) +
    ggplot2::facet_grid(name ~ n_obs) +
    ggplot2::ggtitle(icu)
  return(p)
}

prepare_data_for_boxplot <- function (dt.f.input, f, g) {
  dt.f.input <- dt.f.input %>%
    filter(names.F == f & names.G == g) %>%
    select(-c(names.F, names.G))
  dt.f.input <- as.data.frame(t(dt.f.input))
  dt.f.input <- dt.f.input %>%
    mutate(which = rownames(dt.f.input),
           n_obs = as.numeric(stringr::str_extract(which, "[0-9]{2,3}")),
           rejecting = as.numeric(V1) / 100,
           which = stringr::str_replace(which, "(.prod|).H0.rej.[0-9]{2,3}", ""),
           name = paste(f, 'vs', g)
    ) %>%
    select(which, n_obs, rejecting, name)
  return(dt.f.input)
}

print_for_each_icu_boxplot <- function(dt.f.input, icu) {
  dt.f <- dt.f.input %>%
    filter(icu == icu) %>%
    select(-(icu))

  dt.f.idr.rq <- prepare_data_for_boxplot(dt.f, 'idr', 'rq')
  dt.f.idr.cox <- prepare_data_for_boxplot(dt.f, 'idr', 'cox')

  dt.f.rq.idr <- prepare_data_for_boxplot(dt.f, 'rq', 'idr')
  dt.f.rq.cox <- prepare_data_for_boxplot(dt.f, 'rq', 'cox')

  dt.f.cox.idr <- prepare_data_for_boxplot(dt.f, 'cox', 'idr')
  dt.f.cox.rq <- prepare_data_for_boxplot(dt.f, 'cox', 'rq')

  dt.all <- dt.f.idr.rq %>%
    add_row(dt.f.idr.cox) %>%
    add_row(dt.f.rq.idr) %>%
    add_row(dt.f.rq.cox) %>%
    add_row(dt.f.cox.idr) %>%
    add_row(dt.f.cox.rq)

  png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_data_example_boxplot_rej_rate_it-icu-", icu, "_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 900)
  g <- create_boxplot_for_data_example(dt.all, icu)
  print(g)
  dev.off()
}
print_for_each_icu_boxplot(dt.data.ex, 'ICU10')
print_for_each_icu_boxplot(dt.data.ex, 'ICU44')
print_for_each_icu_boxplot(dt.data.ex, 'ICU65')
print_for_each_icu_boxplot(dt.data.ex, 'ICU76')
print_for_each_icu_boxplot(dt.data.ex, 'ICU77')
