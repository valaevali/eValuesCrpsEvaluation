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

data.date <- data %>% arrange(date)
input.list <- list("ICU10" = data.date %>% filter(icu == "ICU10"),
                   "ICU44" = data.date %>% filter(icu == "ICU44"),
                   "ICU65" = data.date %>% filter(icu == "ICU65"),
                   "ICU76" = data.date %>% filter(icu == "ICU76"),
                   "ICU77" = data.date %>% filter(icu == "ICU77")
)

# memory problem accessing
data.ex.icu10.idr.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU10, crps.F.para = input.list$ICU10$idr, crps.G.para = input.list$ICU10$rq, f = 'idr', g = 'rq')
data.ex.icu10.idr.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU10, crps.F.para = input.list$ICU10$idr, crps.G.para = input.list$ICU10$cox, f = 'idr', g = 'cox')
data.ex.icu10.rq.Cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU10, crps.F.para = input.list$ICU10$rq, crps.G.para = input.list$ICU10$cox, f = 'rq', g = 'cox')
data.ex.icu10.rq.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU10, crps.F.para = input.list$ICU10$rq, crps.G.para = input.list$ICU10$idr, f = 'rq', g = 'idr')
data.ex.icu10.cox.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU10, crps.F.para = input.list$ICU10$cox, crps.G.para = input.list$ICU10$rq, f = 'cox', g = 'rq')
data.ex.icu10.cox.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU10, crps.F.para = input.list$ICU10$cox, crps.G.para = input.list$ICU10$idr, f = 'cox', g = 'idr')

data.ex.icu44.idr.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU44, crps.F.para = input.list$ICU44$idr, crps.G.para = input.list$ICU44$rq, f = 'idr', g = 'rq')
data.ex.icu44.idr.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU44, crps.F.para = input.list$ICU44$idr, crps.G.para = input.list$ICU44$cox, f = 'idr', g = 'cox')
data.ex.icu44.rq.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU44, crps.F.para = input.list$ICU44$rq, crps.G.para = input.list$ICU44$cox, f = 'rq', g = 'cox')
data.ex.icu44.rq.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU44, crps.F.para = input.list$ICU44$rq, crps.G.para = input.list$ICU44$idr, f = 'rq', g = 'idr')
data.ex.icu44.cox.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU44, crps.F.para = input.list$ICU44$cox, crps.G.para = input.list$ICU44$rq, f = 'cox', g = 'rq')
data.ex.icu44.cox.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU44, crps.F.para = input.list$ICU44$cox, crps.G.para = input.list$ICU44$idr, f = 'cox', g = 'idr')

data.ex.icu65.idr.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU65, crps.F.para = input.list$ICU65$idr, crps.G.para = input.list$ICU65$rq, f = 'idr', g = 'rq')
data.ex.icu65.idr.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU65, crps.F.para = input.list$ICU65$idr, crps.G.para = input.list$ICU65$cox, f = 'idr', g = 'cox')
data.ex.icu65.rq.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU65, crps.F.para = input.list$ICU65$rq, crps.G.para = input.list$ICU65$cox, f = 'rq', g = 'cox')
data.ex.icu65.rq.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU65, crps.F.para = input.list$ICU65$rq, crps.G.para = input.list$ICU65$idr, f = 'rq', g = 'idr')
data.ex.icu65.cox.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU65, crps.F.para = input.list$ICU65$cox, crps.G.para = input.list$ICU65$rq, f = 'cox', g = 'rq')
data.ex.icu65.cox.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU65, crps.F.para = input.list$ICU65$cox, crps.G.para = input.list$ICU65$idr, f = 'cox', g = 'idr')

data.ex.icu76.idr.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU76, crps.F.para = input.list$ICU76$idr, crps.G.para = input.list$ICU76$rq, f = 'idr', g = 'rq')
data.ex.icu76.idr.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU76, crps.F.para = input.list$ICU76$idr, crps.G.para = input.list$ICU76$cox, f = 'idr', g = 'cox')
data.ex.icu76.rq.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU76, crps.F.para = input.list$ICU76$rq, crps.G.para = input.list$ICU76$cox, f = 'rq', g = 'cox')
data.ex.icu76.rq.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU76, crps.F.para = input.list$ICU76$rq, crps.G.para = input.list$ICU76$idr, f = 'rq', g = 'idr')
data.ex.icu76.cox.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU76, crps.F.para = input.list$ICU76$cox, crps.G.para = input.list$ICU76$rq, f = 'cox', g = 'rq')
data.ex.icu76.cox.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU76, crps.F.para = input.list$ICU76$cox, crps.G.para = input.list$ICU76$idr, f = 'cox', g = 'idr')

data.ex.icu77.idr.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU77, crps.F.para = input.list$ICU77$idr, crps.G.para = input.list$ICU77$rq, f = 'idr', g = 'rq')
data.ex.icu77.idr.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU77, crps.F.para = input.list$ICU77$idr, crps.G.para = input.list$ICU77$cox, f = 'idr', g = 'cox')
data.ex.icu77.rq.cox <- calculate_e_values_data_example_for_each(input.data = input.list$ICU77, crps.F.para = input.list$ICU77$rq, crps.G.para = input.list$ICU77$cox, f = 'rq', g = 'cox')
data.ex.icu77.rq.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU77, crps.F.para = input.list$ICU77$rq, crps.G.para = input.list$ICU77$idr, f = 'rq', g = 'idr')
data.ex.icu77.cox.rq <- calculate_e_values_data_example_for_each(input.data = input.list$ICU77, crps.F.para = input.list$ICU77$cox, crps.G.para = input.list$ICU77$rq, f = 'cox', g = 'rq')
data.ex.icu77.cox.idr <- calculate_e_values_data_example_for_each(input.data = input.list$ICU77, crps.F.para = input.list$ICU77$cox, crps.G.para = input.list$ICU77$idr, f = 'cox', g = 'idr')


data.ex.icu10.idr.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU10-idrvsrq-2023-01-11T16-54-30.fst"))
data.ex.icu10.idr.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU10-idrvscox-2023-01-11T16-56-04.fst"))
data.ex.icu10.rq.Cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU10-rqvscox-2023-01-11T16-58-01.fst"))
data.ex.icu10.rq.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU10-rqvsidr-2023-01-11T17-00-36.fst"))
data.ex.icu10.cox.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU10-coxvsrq-2023-01-11T17-02-18.fst"))
data.ex.icu10.cox.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU10-coxvsidr-2023-01-11T17-04-55.fst"))
data.ex.icu44.idr.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU44-idrvsrq-2023-01-11T09-00-53.fst"))
data.ex.icu44.idr.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU44-idrvscox-2023-01-11T09-01-15.fst"))
data.ex.icu44.rq.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU44-rqvscox-2023-01-11T09-01-50.fst"))
data.ex.icu44.rq.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU44-rqvsidr-2023-01-11T09-02-32.fst"))
data.ex.icu44.cox.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU44-coxvsrq-2023-01-11T09-02-54.fst"))
data.ex.icu44.cox.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU44-coxvsidr-2023-01-11T09-03-35.fst"))
data.ex.icu65.idr.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU65-idrvsrq-2023-01-11T09-04-23.fst"))
data.ex.icu65.idr.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU65-idrvscox-2023-01-11T09-05-15.fst"))
data.ex.icu65.rq.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU65-rqvscox-2023-01-11T09-06-46.fst"))
data.ex.icu65.rq.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU65-rqvsidr-2023-01-11T09-08-17.fst"))
data.ex.icu65.cox.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU65-coxvsrq-2023-01-11T09-09-09.fst"))
data.ex.icu65.cox.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU65-coxvsidr-2023-01-11T09-10-39.fst"))
data.ex.icu76.idr.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU76-idrvsrq-2023-01-11T09-11-11.fst"))
data.ex.icu76.idr.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU76-idrvscox-2023-01-11T09-11-43.fst"))
data.ex.icu76.rq.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU76-rqvscox-2023-01-11T09-12-22.fst"))
data.ex.icu76.rq.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU76-rqvsidr-2023-01-11T09-13-23.fst"))
data.ex.icu76.cox.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU76-coxvsrq-2023-01-11T09-14-22.fst"))
data.ex.icu76.cox.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU76-coxvsidr-2023-01-11T09-15-19.fst"))
data.ex.icu77.idr.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU77-idrvsrq-2023-01-11T09-15-39.fst"))
data.ex.icu77.idr.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU77-idrvscox-2023-01-11T09-15-57.fst"))
data.ex.icu77.rq.cox <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU77-rqvscox-2023-01-11T09-16-34.fst"))
data.ex.icu77.rq.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU77-rqvsidr-2023-01-11T09-17-14.fst"))
data.ex.icu77.cox.rq <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU77-coxvsrq-2023-01-11T09-17-34.fst"))
data.ex.icu77.cox.idr <- fst::read_fst(paste0(getwd(), "/target/run-data-example-ICU77-coxvsidr-2023-01-11T09-18-13.fst"))

###################

# icu 10
dt.data.ex.icu10 <- data.ex.icu10.cox.idr %>%
  add_row(data.ex.icu10.cox.rq) %>%
  add_row(data.ex.icu10.idr.cox) %>%
  add_row(data.ex.icu10.idr.rq) %>%
  add_row(data.ex.icu10.rq.Cox) %>%
  add_row(data.ex.icu10.rq.idr) %>%
  mutate(which = paste0(names.F, " vs ", names.G), icu = 'ICU10') %>%
  select(-c(names.F, names.G)) %>%
  arrange(date)
dt.data.ex.icu10.early <- dt.data.ex.icu10 %>%
  filter(as.Date(date) < as.Date('2016-09-14')) %>%
  mutate(icu = paste('ICU10 - first', n(), 'obs'))

# icu 44
dt.data.ex.icu44 <- data.ex.icu44.cox.idr %>%
  add_row(data.ex.icu44.cox.rq) %>%
  add_row(data.ex.icu44.idr.cox) %>%
  add_row(data.ex.icu44.idr.rq) %>%
  add_row(data.ex.icu44.rq.cox) %>%
  add_row(data.ex.icu44.rq.idr) %>%
  mutate(which = paste0(names.F, " vs ", names.G), icu = 'ICU44') %>%
  select(-c(names.F, names.G)) %>%
  arrange(date)
dt.data.ex.icu44.early <- dt.data.ex.icu44 %>%
  filter(as.Date(date) < as.Date('2016-10-23')) %>%
  mutate(icu = paste('ICU44 - first', n(), 'obs'))

# icu 65
dt.data.ex.icu65 <- data.ex.icu65.cox.idr %>%
  add_row(data.ex.icu65.cox.rq) %>%
  add_row(data.ex.icu65.idr.cox) %>%
  add_row(data.ex.icu65.idr.rq) %>%
  add_row(data.ex.icu65.rq.cox) %>%
  add_row(data.ex.icu65.rq.idr) %>%
  mutate(which = paste0(names.F, " vs ", names.G), icu = 'ICU65') %>%
  select(-c(names.F, names.G)) %>%
  arrange(date)
dt.data.ex.icu65.early <- dt.data.ex.icu65 %>%
  filter(as.Date(date) < as.Date('2017-03-11')) %>%
  mutate(icu = paste('ICU65 - first', n(), 'obs'))

# icu 76
dt.data.ex.icu76 <- data.ex.icu76.cox.idr %>%
  add_row(data.ex.icu76.cox.rq) %>%
  add_row(data.ex.icu76.idr.cox) %>%
  add_row(data.ex.icu76.idr.rq) %>%
  add_row(data.ex.icu76.rq.cox) %>%
  add_row(data.ex.icu76.rq.idr) %>%
  mutate(which = paste0(names.F, " vs ", names.G), icu = 'ICU76') %>%
  select(-c(names.F, names.G)) %>%
  arrange(date)
dt.data.ex.icu76.early <- dt.data.ex.icu76 %>%
  filter(as.Date(date) < as.Date('2016-10-18')) %>%
  mutate(icu = paste('ICU76 - first', n(), 'obs'))

# icu 77
dt.data.ex.icu77 <- data.ex.icu77.cox.idr %>%
  add_row(data.ex.icu77.cox.rq) %>%
  add_row(data.ex.icu77.idr.cox) %>%
  add_row(data.ex.icu77.idr.rq) %>%
  add_row(data.ex.icu77.rq.cox) %>%
  add_row(data.ex.icu77.rq.idr) %>%
  mutate(which = paste0(names.F, " vs ", names.G), icu = 'ICU77') %>%
  select(-c(names.F, names.G)) %>%
  arrange(date)
dt.data.ex.icu77.early <- dt.data.ex.icu77 %>%
  filter(as.Date(date) < as.Date('2017-12-11')) %>%
  mutate(icu = paste('ICU77 - first', n(), 'obs'))

dt.data.ex <- dt.data.ex.icu10.early %>%
  add_row(dt.data.ex.icu10) %>%
  add_row(dt.data.ex.icu44.early) %>%
  add_row(dt.data.ex.icu44) %>%
  add_row(dt.data.ex.icu65.early) %>%
  add_row(dt.data.ex.icu65) %>%
  add_row(dt.data.ex.icu76.early) %>%
  add_row(dt.data.ex.icu76) %>%
  add_row(dt.data.ex.icu77.early) %>%
  add_row(dt.data.ex.icu77) %>%
  mutate(across(icu, factor, levels = c(dt.data.ex.icu10.early$icu[1], "ICU10", dt.data.ex.icu44.early$icu[1], "ICU44", dt.data.ex.icu65.early$icu[1], "ICU65", dt.data.ex.icu76.early$icu[1], "ICU76", dt.data.ex.icu77.early$icu[1], "ICU77"))) %>%
  filter(is.finite(e.value.alt.cons.prod))

g <- ggplot2::ggplot(dt.data.ex, ggplot2::aes(x = date, y = e.value.alt.cons.prod, group = which)) +
  ggplot2::geom_line(ggplot2::aes(linetype = which, color = which), linewidth = 1) +
  ggplot2::scale_color_manual(values = c("green", "blue", "orange", "red", "purple", "black")) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::coord_cartesian(ylim = c(0, 5)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, color = "red"), axis.text = ggplot2::element_text(size = 12),
                 strip.text.x = ggplot2::element_text(size = 12), strip.text.y = ggplot2::element_text(size = 12),
                 axis.text.x = ggplot2::element_text(angle = 70, vjust = 0.9, hjust = 1)) +
  ggplot2::ylab("E-process") +
  ggplot2::facet_wrap(. ~ icu, ncol = 2, scales = "free_x") +
  ggplot2::xlab("")

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_data_example_all-icus_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 600)
print(g)
dev.off()

g <- ggplot2::ggplot(dt.data.ex %>% filter(!grepl("first", icu)), ggplot2::aes(x = date, y = e.value.alt.cons.prod, group = which)) +
  ggplot2::geom_line(ggplot2::aes(linetype = which, color = which), linewidth = 1) +
  ggplot2::scale_color_manual(values = c("green", "blue", "orange", "red", "purple", "black")) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::coord_cartesian(ylim = c(0.995, 1.005)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, color = "red"), axis.text = ggplot2::element_text(size = 12),
                 strip.text.x = ggplot2::element_text(size = 12), strip.text.y = ggplot2::element_text(size = 12),
                 axis.text.x = ggplot2::element_text(angle = 70, vjust = 0.9, hjust = 1)) +
  ggplot2::ylab("E-process") +
  ggplot2::facet_wrap(. ~ icu, ncol = 1, scales = "free_x") +
  ggplot2::xlab("")

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_data_example_all-icus_around_1_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 600)
print(g)
dev.off()

dt.data.ex.icu10.early.log <- dt.data.ex.icu10 %>%
  mutate(e.value.alt.cons.prod.log = log(e.value.alt.cons.prod)) %>%
  filter(as.Date(date) < as.Date('2016-09-16')) %>%
  mutate(icu = paste('ICU10 - log - first', n(), 'obs'))
dt.data.ex.icu44.early.log <- dt.data.ex.icu44 %>%
  mutate(e.value.alt.cons.prod.log = log(e.value.alt.cons.prod)) %>%
  filter(as.Date(date) < as.Date('2016-11-14')) %>%
  mutate(icu = paste('ICU44 - log - first', n(), 'obs'))
dt.data.ex.icu65.early.log <- dt.data.ex.icu65 %>%
  mutate(e.value.alt.cons.prod.log = log(e.value.alt.cons.prod)) %>%
  filter(as.Date(date) < as.Date('2017-03-21')) %>%
  mutate(icu = paste('ICU65 - log - first', n(), 'obs'))
dt.data.ex.icu76.early.log <- dt.data.ex.icu76 %>%
  mutate(e.value.alt.cons.prod.log = log(e.value.alt.cons.prod)) %>%
  filter(as.Date(date) < as.Date('2016-10-30')) %>%
  mutate(icu = paste('ICU76 - log - first', n(), 'obs'))
dt.data.ex.icu77.early.log <- dt.data.ex.icu77 %>%
  mutate(e.value.alt.cons.prod.log = log(e.value.alt.cons.prod)) %>%
  filter(as.Date(date) < as.Date('2017-12-16')) %>%
  mutate(icu = paste('ICU77 - log - first', n(), 'obs'))
dt.data.ex.log <- dt.data.ex.icu10.early.log %>%
  add_row(dt.data.ex.icu44.early.log) %>%
  add_row(dt.data.ex.icu65.early.log) %>%
  add_row(dt.data.ex.icu76.early.log) %>%
  add_row(dt.data.ex.icu77.early.log) %>%
  mutate(across(icu, factor, levels = c(dt.data.ex.icu10.early.log$icu[1], dt.data.ex.icu44.early.log$icu[1], dt.data.ex.icu65.early.log$icu[1], dt.data.ex.icu76.early.log$icu[1], dt.data.ex.icu77.early.log$icu[1]))) %>%
  filter(is.finite(e.value.alt.cons.prod.log))

g <- ggplot2::ggplot(dt.data.ex.log, ggplot2::aes(x = date, y = e.value.alt.cons.prod.log, group = which)) +
  ggplot2::geom_line(ggplot2::aes(linetype = which, color = which), linewidth = 1) +
  ggplot2::scale_color_manual(values = c("green", "blue", "orange", "red", "purple", "black")) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::coord_cartesian(ylim = c(0, 50)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, color = "red"), axis.text = ggplot2::element_text(size = 12),
                 strip.text.x = ggplot2::element_text(size = 12), strip.text.y = ggplot2::element_text(size = 12),
                 axis.text.x = ggplot2::element_text(angle = 70, vjust = 0.9, hjust = 1)) +
  ggplot2::ylab("E-process") +
  ggplot2::facet_wrap(. ~ icu, ncol = 1, scales = "free_x") +
  ggplot2::xlab("")

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_data_example_all-icus_log_scale_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 600)
print(g)
dev.off()

###################################################################
n <- 100
sequential.run <- rep(NA, n)
system.time({
  data.run <- data.date[1,]
  first.run <- e_value(y = data.run$los, crps.F.para = list("points.cdf" = data.run$idr), crps.G.para = list("points.cdf" = data.run$rq))
  sequential.run[1] <- list(first.run)
  for (i in 2:n) {
    data.run <- data.date[i,]
    first.run <- e_value(old.run.e.value = first.run, new.y = data.run$los, new.crps.F.para = data.run$idr, new.crps.G.para = data.run$rq)
    sequential.run[i] <- list(first.run)
  }
})

whole.run <- rep(NA, n)
system.time({
  for (i in 1:n) {
    whole.run[i] <- list(e_value(y = data.date[1:i,]$los, crps.F.para = list("points.cdf" = data.date[1:i,]$idr), crps.G.para = list("points.cdf" = data.date[1:i,]$rq)))
  }
})

######################################################################################################################
dt.data.ex.dt <- dt.data.ex.icu10.early %>%
  add_row(dt.data.ex.icu10) %>%
  add_row(dt.data.ex.icu44.early) %>%
  add_row(dt.data.ex.icu44) %>%
  add_row(dt.data.ex.icu65.early) %>%
  add_row(dt.data.ex.icu65) %>%
  add_row(dt.data.ex.icu76.early) %>%
  add_row(dt.data.ex.icu76) %>%
  add_row(dt.data.ex.icu77.early) %>%
  add_row(dt.data.ex.icu77) %>%
  mutate(across(icu, factor, levels = c(dt.data.ex.icu10.early$icu[1], "ICU10", dt.data.ex.icu44.early$icu[1], "ICU44", dt.data.ex.icu65.early$icu[1], "ICU65", dt.data.ex.icu76.early$icu[1], "ICU76", dt.data.ex.icu77.early$icu[1], "ICU77"))) %>%
  filter(!grepl("first", icu)) %>%
  group_by(icu, which) %>%
  slice(which.max(date)) %>%
  select(-date) %>%
  mutate(e.value.alt.cons.prod = ifelse(is.infinite(e.value.alt.cons.prod), "Inf", toString(round(e.value.alt.cons.prod, digits = 3))))
dt.data.ex.dt <- dt.data.ex.dt %>%
  tidyr::pivot_wider(names_from = which, values_from = e.value.alt.cons.prod) %>%
  select(icu, 'cox vs idr', 'cox vs rq', 'idr vs cox', 'idr vs rq', 'rq vs cox', 'rq vs idr')

dt <- DT::datatable(dt.data.ex.dt, escape = FALSE, rownames = FALSE, options = list(dom = 'Bfrtip', pageLength = 15, bFilter = 0, bInfo = 0, bPaginate = 0))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)


######## Comparing the p-value for the whole set
p.value.icu10.idrvsrq <- e_value(y = input.list$ICU10$los, crps.F.para = list("points.cdf" = input.list$ICU10$idr), crps.G.para = list("points.cdf" = input.list$ICU10$rq), method = NA, p.value.method = "t")
p.value.icu10.idrvscox <- e_value(y = input.list$ICU10$los, crps.F.para = list("points.cdf" = input.list$ICU10$idr), crps.G.para = list("points.cdf" = input.list$ICU10$cox), method = NA, p.value.method = "t")
p.value.icu10.rqvsidr <- e_value(y = input.list$ICU10$los, crps.F.para = list("points.cdf" = input.list$ICU10$rq), crps.G.para = list("points.cdf" = input.list$ICU10$idr), method = NA, p.value.method = "t")
p.value.icu10.rqvscox <- e_value(y = input.list$ICU10$los, crps.F.para = list("points.cdf" = input.list$ICU10$rq), crps.G.para = list("points.cdf" = input.list$ICU10$cox), method = NA, p.value.method = "t")
p.value.icu10.coxvsidr <- e_value(y = input.list$ICU10$los, crps.F.para = list("points.cdf" = input.list$ICU10$cox), crps.G.para = list("points.cdf" = input.list$ICU10$idr), method = NA, p.value.method = "t")
p.value.icu10.coxvsrq <- e_value(y = input.list$ICU10$los, crps.F.para = list("points.cdf" = input.list$ICU10$cox), crps.G.para = list("points.cdf" = input.list$ICU10$rq), method = NA, p.value.method = "t")

p.value.icu44.idrvsrq <- e_value(y = input.list$ICU44$los, crps.F.para = list("points.cdf" = input.list$ICU44$idr), crps.G.para = list("points.cdf" = input.list$ICU44$rq), method = NA, p.value.method = "t")
p.value.icu44.idrvscox <- e_value(y = input.list$ICU44$los, crps.F.para = list("points.cdf" = input.list$ICU44$idr), crps.G.para = list("points.cdf" = input.list$ICU44$cox), method = NA, p.value.method = "t")
p.value.icu44.rqvsidr <- e_value(y = input.list$ICU44$los, crps.F.para = list("points.cdf" = input.list$ICU44$rq), crps.G.para = list("points.cdf" = input.list$ICU44$idr), method = NA, p.value.method = "t")
p.value.icu44.rqvscox <- e_value(y = input.list$ICU44$los, crps.F.para = list("points.cdf" = input.list$ICU44$rq), crps.G.para = list("points.cdf" = input.list$ICU44$cox), method = NA, p.value.method = "t")
p.value.icu44.coxvsidr <- e_value(y = input.list$ICU44$los, crps.F.para = list("points.cdf" = input.list$ICU44$cox), crps.G.para = list("points.cdf" = input.list$ICU44$idr), method = NA, p.value.method = "t")
p.value.icu44.coxvsrq <- e_value(y = input.list$ICU44$los, crps.F.para = list("points.cdf" = input.list$ICU44$cox), crps.G.para = list("points.cdf" = input.list$ICU44$rq), method = NA, p.value.method = "t")

p.value.icu65.idrvsrq <- e_value(y = input.list$ICU65$los, crps.F.para = list("points.cdf" = input.list$ICU65$idr), crps.G.para = list("points.cdf" = input.list$ICU65$rq), method = NA, p.value.method = "t")
p.value.icu65.idrvscox <- e_value(y = input.list$ICU65$los, crps.F.para = list("points.cdf" = input.list$ICU65$idr), crps.G.para = list("points.cdf" = input.list$ICU65$cox), method = NA, p.value.method = "t")
p.value.icu65.rqvsidr <- e_value(y = input.list$ICU65$los, crps.F.para = list("points.cdf" = input.list$ICU65$rq), crps.G.para = list("points.cdf" = input.list$ICU65$idr), method = NA, p.value.method = "t")
p.value.icu65.rqvscox <- e_value(y = input.list$ICU65$los, crps.F.para = list("points.cdf" = input.list$ICU65$rq), crps.G.para = list("points.cdf" = input.list$ICU65$cox), method = NA, p.value.method = "t")
p.value.icu65.coxvsidr <- e_value(y = input.list$ICU65$los, crps.F.para = list("points.cdf" = input.list$ICU65$cox), crps.G.para = list("points.cdf" = input.list$ICU65$idr), method = NA, p.value.method = "t")
p.value.icu65.coxvsrq <- e_value(y = input.list$ICU65$los, crps.F.para = list("points.cdf" = input.list$ICU65$cox), crps.G.para = list("points.cdf" = input.list$ICU65$rq), method = NA, p.value.method = "t")

p.value.icu76.idrvsrq <- e_value(y = input.list$ICU76$los, crps.F.para = list("points.cdf" = input.list$ICU76$idr), crps.G.para = list("points.cdf" = input.list$ICU76$rq), method = NA, p.value.method = "t")
p.value.icu76.idrvscox <- e_value(y = input.list$ICU76$los, crps.F.para = list("points.cdf" = input.list$ICU76$idr), crps.G.para = list("points.cdf" = input.list$ICU76$cox), method = NA, p.value.method = "t")
p.value.icu76.rqvsidr <- e_value(y = input.list$ICU76$los, crps.F.para = list("points.cdf" = input.list$ICU76$rq), crps.G.para = list("points.cdf" = input.list$ICU76$idr), method = NA, p.value.method = "t")
p.value.icu76.rqvscox <- e_value(y = input.list$ICU76$los, crps.F.para = list("points.cdf" = input.list$ICU76$rq), crps.G.para = list("points.cdf" = input.list$ICU76$cox), method = NA, p.value.method = "t")
p.value.icu76.coxvsidr <- e_value(y = input.list$ICU76$los, crps.F.para = list("points.cdf" = input.list$ICU76$cox), crps.G.para = list("points.cdf" = input.list$ICU76$idr), method = NA, p.value.method = "t")
p.value.icu76.coxvsrq <- e_value(y = input.list$ICU76$los, crps.F.para = list("points.cdf" = input.list$ICU76$cox), crps.G.para = list("points.cdf" = input.list$ICU76$rq), method = NA, p.value.method = "t")

p.value.icu77.idrvsrq <- e_value(y = input.list$ICU77$los, crps.F.para = list("points.cdf" = input.list$ICU77$idr), crps.G.para = list("points.cdf" = input.list$ICU77$rq), method = NA, p.value.method = "t")
p.value.icu77.idrvscox <- e_value(y = input.list$ICU77$los, crps.F.para = list("points.cdf" = input.list$ICU77$idr), crps.G.para = list("points.cdf" = input.list$ICU77$cox), method = NA, p.value.method = "t")
p.value.icu77.rqvsidr <- e_value(y = input.list$ICU77$los, crps.F.para = list("points.cdf" = input.list$ICU77$rq), crps.G.para = list("points.cdf" = input.list$ICU77$idr), method = NA, p.value.method = "t")
p.value.icu77.rqvscox <- e_value(y = input.list$ICU77$los, crps.F.para = list("points.cdf" = input.list$ICU77$rq), crps.G.para = list("points.cdf" = input.list$ICU77$cox), method = NA, p.value.method = "t")
p.value.icu77.coxvsidr <- e_value(y = input.list$ICU77$los, crps.F.para = list("points.cdf" = input.list$ICU77$cox), crps.G.para = list("points.cdf" = input.list$ICU77$idr), method = NA, p.value.method = "t")
p.value.icu77.coxvsrq <- e_value(y = input.list$ICU77$los, crps.F.para = list("points.cdf" = input.list$ICU77$cox), crps.G.para = list("points.cdf" = input.list$ICU77$rq), method = NA, p.value.method = "t")

p.value.total <-
  tibble(icu = "ICU10", which = "cox vx idr", p.value = p.value.icu10.coxvsidr$p.value) %>%
    add_row(icu = "ICU10", which = "cox vs rq", p.value = p.value.icu10.coxvsrq$p.value) %>%
    add_row(icu = "ICU10", which = "idr vs rq", p.value = p.value.icu10.idrvsrq$p.value) %>%
    add_row(icu = "ICU10", which = "idr vs cox", p.value = p.value.icu10.idrvscox$p.value) %>%
    add_row(icu = "ICU10", which = "rq vs idr", p.value = p.value.icu10.rqvsidr$p.value) %>%
    add_row(icu = "ICU10", which = "rq vs cox", p.value = p.value.icu10.rqvscox$p.value) %>%

    add_row(icu = "ICU44", which = "cox vx idr", p.value = p.value.icu44.coxvsidr$p.value) %>%
    add_row(icu = "ICU44", which = "cox vs rq", p.value = p.value.icu44.coxvsrq$p.value) %>%
    add_row(icu = "ICU44", which = "idr vs rq", p.value = p.value.icu44.idrvsrq$p.value) %>%
    add_row(icu = "ICU44", which = "idr vs cox", p.value = p.value.icu44.idrvscox$p.value) %>%
    add_row(icu = "ICU44", which = "rq vs idr", p.value = p.value.icu44.rqvsidr$p.value) %>%
    add_row(icu = "ICU44", which = "rq vs cox", p.value = p.value.icu44.rqvscox$p.value) %>%

    add_row(icu = "ICU65", which = "cox vx idr", p.value = p.value.icu65.coxvsidr$p.value) %>%
    add_row(icu = "ICU65", which = "cox vs rq", p.value = p.value.icu65.coxvsrq$p.value) %>%
    add_row(icu = "ICU65", which = "idr vs rq", p.value = p.value.icu65.idrvsrq$p.value) %>%
    add_row(icu = "ICU65", which = "idr vs cox", p.value = p.value.icu65.idrvscox$p.value) %>%
    add_row(icu = "ICU65", which = "rq vs idr", p.value = p.value.icu65.rqvsidr$p.value) %>%
    add_row(icu = "ICU65", which = "rq vs cox", p.value = p.value.icu65.rqvscox$p.value) %>%

    add_row(icu = "ICU76", which = "cox vx idr", p.value = p.value.icu76.coxvsidr$p.value) %>%
    add_row(icu = "ICU76", which = "cox vs rq", p.value = p.value.icu76.coxvsrq$p.value) %>%
    add_row(icu = "ICU76", which = "idr vs rq", p.value = p.value.icu76.idrvsrq$p.value) %>%
    add_row(icu = "ICU76", which = "idr vs cox", p.value = p.value.icu76.idrvscox$p.value) %>%
    add_row(icu = "ICU76", which = "rq vs idr", p.value = p.value.icu76.rqvsidr$p.value) %>%
    add_row(icu = "ICU76", which = "rq vs cox", p.value = p.value.icu76.rqvscox$p.value) %>%

    add_row(icu = "ICU77", which = "cox vx idr", p.value = p.value.icu77.coxvsidr$p.value) %>%
    add_row(icu = "ICU77", which = "cox vs rq", p.value = p.value.icu77.coxvsrq$p.value) %>%
    add_row(icu = "ICU77", which = "idr vs rq", p.value = p.value.icu77.idrvsrq$p.value) %>%
    add_row(icu = "ICU77", which = "idr vs cox", p.value = p.value.icu77.idrvscox$p.value) %>%
    add_row(icu = "ICU77", which = "rq vs idr", p.value = p.value.icu77.rqvsidr$p.value) %>%
    add_row(icu = "ICU77", which = "rq vs cox", p.value = p.value.icu77.rqvscox$p.value)
p.value.dt <- p.value.total %>%
  mutate(p.value = round(p.value, digits = 3)) %>%
  tidyr::pivot_wider(names_from = which, values_from = p.value) %>%
  select(icu, 'cox vs idr' = "cox vx idr", 'cox vs rq', 'idr vs cox', 'idr vs rq', 'rq vs cox', 'rq vs idr')

dt <- DT::datatable(p.value.dt, escape = FALSE, rownames = FALSE, options = list(dom = 'Bfrtip', pageLength = 15, bFilter = 0, bInfo = 0, bPaginate = 0, columnDefs = list(list(className = 'dt-left', targets = 0:6))))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)


### plot LoS over time for each ICU
dt.los <- data.date %>% select(date, icu, los)

g <- ggplot2::ggplot(dt.los, ggplot2::aes(x = date, y = los)) +
  ggplot2::geom_line(linewidth = 0.5) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::coord_cartesian(ylim = c(0, 100)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, color = "red"), axis.text = ggplot2::element_text(size = 12),
                 strip.text.x = ggplot2::element_text(size = 12), strip.text.y = ggplot2::element_text(size = 12),
                 axis.text.x = ggplot2::element_text(angle = 70, vjust = 0.9, hjust = 1)) +
  ggplot2::ylab("LoS") +
  ggplot2::facet_wrap(. ~ icu, ncol = 3, scales = "free_x") +
  ggplot2::xlab("")

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_data_example_los_", format(Sys.time(), format = "%d-%m"), ".png"), height = 500, width = 900)
print(g)
dev.off()


## minimal observations needed to reject the null hypothesis
dt <- dt.data.ex.icu10 %>%
  group_by(which) %>%
  mutate(n = 1:n()) %>%
  filter(1 / e.value.alt.cons.prod <= 0.05) %>%
  slice(which.min(date)) %>%
  select(icu, which, n) %>%
  ungroup() %>%
  add_row(dt.data.ex.icu44 %>%
            group_by(which) %>%
            mutate(n = 1:n()) %>%
            filter(1 / e.value.alt.cons.prod <= 0.05) %>%
            slice(which.min(date)) %>%
            select(icu, which, n)) %>%
  add_row(dt.data.ex.icu65 %>%
            group_by(which) %>%
            mutate(n = 1:n()) %>%
            filter(1 / e.value.alt.cons.prod <= 0.05) %>%
            slice(which.min(date)) %>%
            select(icu, which, n)) %>%
  add_row(dt.data.ex.icu76 %>%
            group_by(which) %>%
            mutate(n = 1:n()) %>%
            filter(1 / e.value.alt.cons.prod <= 0.05) %>%
            slice(which.min(date)) %>%
            select(icu, which, n)) %>%
  add_row(dt.data.ex.icu77 %>%
            group_by(which) %>%
            mutate(n = 1:n()) %>%
            filter(1 / e.value.alt.cons.prod <= 0.05) %>%
            slice(which.min(date)) %>%
            select(icu, which, n))
dt <- dt %>%
  tidyr::pivot_wider(names_from = which, values_from = n) %>%
  mutate('idr vs cox' = as.integer(NA), 'idr vs rq' = as.integer(NA), 'cox vs rq' = as.integer(NA)) %>%
  select(icu, 'cox vs idr', 'cox vs rq', 'idr vs cox', 'idr vs rq', 'rq vs cox', 'rq vs idr')

data.dt <- DT::datatable(dt, escape = FALSE, rownames = FALSE, options = list(dom = 'Bfrtip', pageLength = 15, bFilter = 0, bInfo = 0, bPaginate = 0, columnDefs = list(list(className = 'dt-left', targets = 0:6))))
htmlwidgets::saveWidget(data.dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)