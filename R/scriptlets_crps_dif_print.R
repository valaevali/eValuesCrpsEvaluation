#Load eValuesCrps
devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")
library("dplyr")

print_further_forecasts <- function(dt.f, f, g) {
  to.print <- dt.f %>%
    filter(names.F == f & names.G == g) %>%
    select(-c(names.F, names.G)) %>% group_by(it) %>%
    mutate(n = 1:n())

  p <- ggplot2::ggplot(to.print, ggplot2::aes(x = n, y = diff, color = it)) +
    ggplot2::geom_line(ggplot2::aes(group = it)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(paste(f, "vs", g)) +
    ggplot2::ylim(c(-8,8)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("number of obs") +
    ggplot2::ylab("difference crps")
  return(p)
}

print_for_each_k_dif <- function(dt.f, n.obs) {
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

  png(paste0("C:/Users/valer/Documents/UNI/Masterarbeit/evalues/ma/pictures/print_further_sim_diff_crps_nobs-", n.obs, "_05-12.png"), height = 950, width = 900)
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

dt.u.f.300 <- f.dt.u.f.300$uncompacted %>%
  filter(it <= 50) %>%
  select(names.F, names.G, crps.F, crps.G, it) %>%
  tidyr::unnest(c(crps.F,crps.G)) %>%
  mutate(diff = crps.F - crps.G) %>%
  arrange(it) %>%
  select(-c(crps.F, crps.G))
dt.u.f.100 <- f.dt.u.f.100$uncompacted %>%
  filter(it <= 50) %>%
  select(names.F, names.G, crps.F, crps.G, it) %>%
  tidyr::unnest(c(crps.F,crps.G)) %>%
  mutate(diff = crps.F - crps.G) %>%
  arrange(it) %>%
  select(-c(crps.F, crps.G))
dt.u.f.50 <- f.dt.u.f.50$uncompacted %>%
  filter(it <= 50) %>%
  select(names.F, names.G, crps.F, crps.G, it) %>%
  tidyr::unnest(c(crps.F,crps.G)) %>%
  mutate(diff = crps.F - crps.G) %>%
  select(-c(crps.F, crps.G)) %>%
  arrange(it)

print_for_each_k_dif(dt.u.f.50, 50)
print_for_each_k_dif(dt.u.f.100, 100)
print_for_each_k_dif(dt.u.f.300, 300)