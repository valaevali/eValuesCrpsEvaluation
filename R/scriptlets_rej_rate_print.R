#Load eValuesCrps
# devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")

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
    ggplot2::scale_x_continuous(limits = c(50, 300), breaks = c(50, 100, 300)) +
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

print_for_each_k_lambdas <- function(dt.f, n.obs) {
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

  png(paste0("C:/Users/valer/Documents/UNI/Masterarbeit/evalues/ma/pictures/print_further_sim_rej_rate_nobs-", n.obs, "_05-12.png"), height = 950, width = 900)
  g <- ggpubr::ggarrange(pe.cl, pe.sr, pe.un,
                    cl.pe, cl.sr, cl.un,
                    sr.pe, sr.cl, sr.un,
                    un.pe, un.cl, un.sr,
                    ncol = 3, nrow = 4, common.legend = TRUE, legend = "bottom")
  print(g)
  dev.off()
}

f.dt.u.f.300 <- eValuesCrps::getFile("/target/run-300-1000-2022-12-02T19-15-08.rds")
f.dt.u.f.100 <- eValuesCrps::getFile("/target/run-100-1000-2022-12-02T16-50-30.rds")
f.dt.u.f.50 <- eValuesCrps::getFile("/target/run-50-1000-2022-12-02T16-36-44.rds")

dt.u.f.300 <- f.dt.u.f.300$evaluated %>%
  mutate(it = 300) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".300")))
dt.u.f.100 <- f.dt.u.f.100$evaluated %>%
  mutate(it = 100) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".100")))
dt.u.f.50 <- f.dt.u.f.50$evaluated %>%
  mutate(it = 50) %>%
  rename_at(vars(contains("value")), list(~paste0(., ".50")))
dt.f <- merge(dt.u.f.300, dt.u.f.100, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.f <- merge(dt.f, dt.u.f.50, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.f <- dt.f %>%
  select(names.F, names.G, p.value.H0.rej.50, p.value.H0.rej.100, p.value.H0.rej.300,
         e.value.lambda.prod.H0.rej.50, e.value.lambda.prod.H0.rej.100, e.value.lambda.prod.H0.rej.300,
         e.value.grapa.prod.H0.rej.50, e.value.grapa.prod.H0.rej.100, e.value.grapa.prod.H0.rej.300,
         e.value.alt.conf.prod.H0.rej.50, e.value.alt.conf.prod.H0.rej.100, e.value.alt.conf.prod.H0.rej.300,
         e.value.alt.cons.prod.H0.rej.50, e.value.alt.cons.prod.H0.rej.100, e.value.alt.cons.prod.H0.rej.300,
         e.value.alt.more.cons.prod.H0.rej.50, e.value.alt.more.cons.prod.H0.rej.100, e.value.alt.more.cons.prod.H0.rej.300
  )

print_for_each_k_lambdas(dt.f, 300)


########################## loosing power



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

p.50 <- print_rej_presentation(f.dt.u.f.50, 50)
p.100 <- print_rej_presentation(f.dt.u.f.100, 100)
p.300 <- print_rej_presentation(f.dt.u.f.300, 300)

png("C:/Users/valer/Documents/UNI/Masterarbeit/evalues/ma/pictures/rej-50-100-300-it-1000_05-12.png", height = 550, width = 900)
ggpubr::ggarrange(p.50$m, p.100$m, p.300$m, p.50$s, p.100$s, p.300$s,
                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()