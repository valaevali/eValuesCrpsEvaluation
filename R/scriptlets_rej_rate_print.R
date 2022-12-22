#Load eValuesCrps
# devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")

n.it <- 1000

dt.u.f.10 <- sim_e_values(n.obs = 10, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.u.f.25 <- sim_e_values(n.obs = 25, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.u.f.50 <- sim_e_values(n.obs = 50, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.u.f.100 <- sim_e_values(n.obs = 100, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.u.f.300 <- sim_e_values(n.obs = 300, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)

# f.dt.u.f.300 <- getFile("/target/run-300-1000-2022-12-22T16-19-34.rds")
# f.dt.u.f.100 <- getFile("/target/run-100-1000-2022-12-22T15-58-31.rds")
# f.dt.u.f.50 <- getFile("/target/run-50-1000-2022-12-22T15-54-30.rds")
# f.dt.u.f.25 <- getFile("/target/run-25-1000-2022-12-22T18-56-51.rds")
# f.dt.u.f.10 <- getFile("/target/run-10-1000-2022-12-22T18-56-12.rds")
f.dt.u.f.300 <- dt.u.f.300
f.dt.u.f.100 <- dt.u.f.100
f.dt.u.f.50 <- dt.u.f.50
f.dt.u.f.25 <- dt.u.f.25
f.dt.u.f.10 <- dt.u.f.10

print_further_forecasts <- function(dt.f, f, g) {
  per.clim <- as.data.frame(t(dt.f %>%
                                filter(names.F == f & names.G == g) %>%
                                select(contains("value"))))
  to.print <- per.clim %>%
    mutate(which = rownames(per.clim),
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

print_for_each_k_lambdas <- function(dt.f, n.it) {
  cl.pe <- print_further_forecasts(dt.f, 'climatological', 'perfect')
  cl.sr <- print_further_forecasts(dt.f, 'climatological', 'sign-reversed')
  cl.un <- print_further_forecasts(dt.f, 'climatological', 'unfocused')

  pe.cl <- print_further_forecasts(dt.f, 'perfect', 'climatological')
  pe.sr <- print_further_forecasts(dt.f, 'perfect', 'sign-reversed')
  pe.un <- print_further_forecasts(dt.f, 'perfect', 'unfocused')

  sr.cl <- print_further_forecasts(dt.f, 'sign-reversed', 'climatological')
  sr.pe <- print_further_forecasts(dt.f, 'sign-reversed', 'perfect')
  sr.un <- print_further_forecasts(dt.f, 'sign-reversed', 'unfocused')

  un.cl <- print_further_forecasts(dt.f, 'unfocused', 'climatological')
  un.pe <- print_further_forecasts(dt.f, 'unfocused', 'perfect')
  un.sr <- print_further_forecasts(dt.f, 'unfocused', 'sign-reversed')

  png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_rej_rate_it-", n.it, "_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 900)
  g <- ggpubr::ggarrange(pe.cl, pe.sr, pe.un,
                    cl.pe, cl.sr, cl.un,
                    sr.pe, sr.cl, sr.un,
                    un.pe, un.cl, un.sr,
                    ncol = 3, nrow = 4, common.legend = TRUE, legend = "bottom")
  print(g)
  dev.off()
}

dt.u.f.300 <- f.dt.u.f.300$evaluated %>%
  mutate(it = 300) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".300")))
dt.u.f.100 <- f.dt.u.f.100$evaluated %>%
  mutate(it = 100) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".100")))
dt.u.f.50 <- f.dt.u.f.50$evaluated %>%
  mutate(it = 50) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".50")))
dt.u.f.25 <- f.dt.u.f.25$evaluated %>%
  mutate(it = 25) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".25")))
dt.u.f.10 <- f.dt.u.f.10$evaluated %>%
  mutate(it = 10) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".10")))
dt.f <- merge(dt.u.f.300, dt.u.f.100, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.f <- merge(dt.f, dt.u.f.50, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.f <- merge(dt.f, dt.u.f.25, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.f <- merge(dt.f, dt.u.f.10, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.f <- dt.f %>%
  select(names.F, names.G, p.value.H0.rej.10, p.value.H0.rej.25, p.value.H0.rej.50, p.value.H0.rej.100, p.value.H0.rej.300,
         e.value.lambda.prod.H0.rej.10, e.value.lambda.prod.H0.rej.25, e.value.lambda.prod.H0.rej.50, e.value.lambda.prod.H0.rej.100, e.value.lambda.prod.H0.rej.300,
         e.value.grapa.prod.H0.rej.10, e.value.grapa.prod.H0.rej.25, e.value.grapa.prod.H0.rej.50, e.value.grapa.prod.H0.rej.100, e.value.grapa.prod.H0.rej.300,
         e.value.alt.conf.prod.H0.rej.10, e.value.alt.conf.prod.H0.rej.25, e.value.alt.conf.prod.H0.rej.50, e.value.alt.conf.prod.H0.rej.100, e.value.alt.conf.prod.H0.rej.300,
         e.value.alt.cons.prod.H0.rej.10, e.value.alt.cons.prod.H0.rej.25, e.value.alt.cons.prod.H0.rej.50, e.value.alt.cons.prod.H0.rej.100, e.value.alt.cons.prod.H0.rej.300,
         e.value.alt.more.cons.prod.H0.rej.10, e.value.alt.more.cons.prod.H0.rej.25, e.value.alt.more.cons.prod.H0.rej.50, e.value.alt.more.cons.prod.H0.rej.100, e.value.alt.more.cons.prod.H0.rej.300
  )

print_for_each_k_lambdas(dt.f, n.it)


########################## loosing power

n.it <- 1000

dt.l.p.f.10 <- sim_e_values(n.obs = 10, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.l.p.f.25 <- sim_e_values(n.obs = 25, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.l.p.f.50 <- sim_e_values(n.obs = 50, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.l.p.f.100 <- sim_e_values(n.obs = 100, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.l.p.f.300 <- sim_e_values(n.obs = 300, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
# dt.l.p.f.10 <- getFile("/target/run-10-1000-2022-12-22T19-08-39.rds")
# dt.l.p.f.25 <- getFile("/target/run-25-1000-2022-12-22T19-12-02.rds")
# dt.l.p.f.50 <- getFile("/target/run-50-1000-2022-12-22T15-52-54.rds")
# dt.l.p.f.100 <- getFile("/target/run-100-1000-2022-12-22T15-55-24.rds")
# dt.l.p.f.300 <- getFile("/target/run-300-1000-2022-12-22T16-01-45.rds")

f.dt.l.p.o.300 <- dt.l.p.f.300
f.dt.l.p.o.100 <- dt.l.p.f.100
f.dt.l.p.o.50 <- dt.l.p.f.50
f.dt.l.p.o.25 <- dt.l.p.f.25
f.dt.l.p.o.10 <- dt.l.p.f.10

print_rej_presentation <- function(dt, n.obs) {
  to.print <- dt$evaluated %>%
    filter(grepl("perfect", names.F) & names.G == 'perfect') %>%
    mutate(e = stringr::str_extract(names.F, "[.0-9]+"),
           mean.sd = stringr::str_extract(names.F, "perfect-[a-z]")
    ) %>%
    ungroup() %>%
    select(-c(names.F, names.G)) %>%
    tidyr::pivot_longer(!c(e, mean.sd), names_to = "key", values_to = "rej_rate") %>%
    mutate(key = stringr::str_replace_all(key, ".prod.H0.rej", "")) %>%
    mutate(key = stringr::str_replace_all(key, ".H0.rej", "")) %>%
    arrange(key)

  m <- ggplot2::ggplot(to.print %>%
                         filter(grepl("-m", mean.sd)) %>%
                         select(-(mean.sd)), ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
    ggplot2::geom_line(ggplot2::aes(group = key)) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::ylab("Rej. rate") +
    ggplot2::xlab(latex2exp::TeX("$\\epsilon_1$")) +
    ggplot2::ggtitle(latex2exp::TeX(paste0("N($\\mu +\\epsilon_1,\\sigma^2$),k=", n.obs))) +
    ggplot2::theme(text = ggplot2::element_text(size = 18)) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::geom_line(data = filter(to.print %>%
                                       filter(grepl("-m", mean.sd)) %>%
                                       select(-(mean.sd)), key == "p.value"), size = 2)
  s <- ggplot2::ggplot(to.print %>%
                         filter(grepl("-s", mean.sd)) %>%
                         select(-(mean.sd)), ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
    ggplot2::geom_line(ggplot2::aes(group = key)) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::ylab("Rej. rate") +
    ggplot2::xlab(latex2exp::TeX("$\\epsilon_2$")) +
    ggplot2::ggtitle(latex2exp::TeX(paste0("$N(\\mu ,\\sigma^2 +\\epsilon_2)$,k=", n.obs))) +
    ggplot2::theme(text = ggplot2::element_text(size = 18)) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::geom_line(data = filter(to.print %>%
                                       filter(grepl("-s", mean.sd)) %>%
                                       select(-(mean.sd)), key == "p.value"), size = 2)
  return(list("m" = m, "s" = s))
}

p.10 <- print_rej_presentation(f.dt.l.p.o.10, 10)
p.25 <- print_rej_presentation(f.dt.l.p.o.25, 25)
p.50 <- print_rej_presentation(f.dt.l.p.o.50, 50)
p.100 <- print_rej_presentation(f.dt.l.p.o.100, 100)
p.300 <- print_rej_presentation(f.dt.l.p.o.300, 300)

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/rej-bias-it-", n.it, "_", format(Sys.time(), format = "%d-%m"), ".png"), height = 1150, width = 600)
ggpubr::ggarrange(p.10$m, p.10$s, p.25$m, p.25$s, p.50$m, p.50$s, p.100$m, p.100$s, p.300$m, p.300$s,
                  ncol = 2, nrow = 5, common.legend = TRUE, legend = "bottom")
dev.off()