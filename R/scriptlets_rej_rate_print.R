print_further_forecasts_rej_rate <- function(dt.f) {
  p <- ggplot2::ggplot(dt.f, ggplot2::aes(x = n.obs, y = rej_rate, color = key)) +
    ggplot2::geom_line(ggplot2::aes(group = key), size = 0.5) +
    ggplot2::scale_x_continuous(limits = c(10, 300), breaks = c(10, 25, 50, 100, 300)) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::geom_line(data = filter(dt.f, key == "p.value"), size = 1) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("k") +
    ggplot2::ylab("Rej. rate") +
    ggplot2::facet_wrap(~ name, ncol = 3)
  return(p)
}

dt.rr.u.f.300 <- dt.u.f.300$evaluated %>%
  mutate(it = 300)
dt.rr.u.f.100 <- dt.u.f.100$evaluated %>%
  mutate(it = 100)
dt.rr.u.f.50 <- dt.u.f.50$evaluated %>%
  mutate(it = 50)
dt.rr.u.f.25 <- dt.u.f.25$evaluated %>%
  mutate(it = 25)
dt.rr.u.f.10 <- dt.u.f.10$evaluated %>%
  mutate(it = 10)
dt.rr.f <- dt.rr.u.f.300 %>% ungroup() %>%
  add_row(dt.rr.u.f.100) %>%
  add_row(dt.rr.u.f.50) %>%
  add_row(dt.rr.u.f.25) %>%
  add_row(dt.rr.u.f.10) %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  tidyr::pivot_longer(!c(name, it), names_to = "key", values_to = "rej_rate") %>%
  mutate(key = stringr::str_replace(key, "(.prod|).H0.rej", "")) %>%
  select(n.obs = it, name, key, rej_rate)

g <- print_further_forecasts_rej_rate(dt.rr.f)
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_rej_rate_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 900)
print(g)
dev.off()


########################## loosing power

print_rej_loosing_power <- function(to.print) {
  m <- ggplot2::ggplot(to.print, ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
    ggplot2::geom_line(ggplot2::aes(group = key), size = 0.5) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    ggplot2::ylim(c(0,100)) +
    ggplot2::ylab("Rej. rate") +
    ggplot2::xlab("e") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(text = ggplot2::element_text(size = 18)) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::geom_line(data = filter(to.print, key == "p.value"), size = 1) +
    ggplot2::facet_grid(n.obs ~ m.s)
  return(m)
}

dt.rr.l.p.10 <-  dt.l.p.f.10$evaluated %>%
  mutate(it = 10)
dt.rr.l.p.25 <-  dt.l.p.f.25$evaluated %>%
  mutate(it = 25)
dt.rr.l.p.50 <-  dt.l.p.f.50$evaluated %>%
  mutate(it = 50)
dt.rr.l.p.100 <- dt.l.p.f.100$evaluated %>%
  mutate(it = 100)
dt.rr.l.p.300 <- dt.l.p.f.300$evaluated %>%
  mutate(it = 300)
dt.rr.l.p <- dt.rr.l.p.300 %>% ungroup() %>%
  add_row(dt.rr.l.p.100) %>%
  add_row(dt.rr.l.p.50) %>%
  add_row(dt.rr.l.p.25) %>%
  add_row(dt.rr.l.p.10) %>%
  filter(grepl("perfect", names.F) & names.G == 'perfect') %>%
  mutate(e = stringr::str_extract(names.F,"[.0-9]+"),
         m.s = stringr::str_extract(names.F, "perfect-[a-z]"),
         m.s = stringr::str_extract(m.s, "[a-z]$"),
         m.s = ifelse(m.s == 'm', "N(mu + e, sd^s)", "N(mu, sd^2 + e)")
         ) %>%
  select(-c(names.F, names.G)) %>%
  tidyr::pivot_longer(!c(m.s, it, e), names_to = "key", values_to = "rej_rate") %>%
  mutate(key = stringr::str_replace(key, "(.prod|).H0.rej", "")) %>%
  select(n.obs = it, m.s, e, key, rej_rate)

g <- print_rej_loosing_power(dt.rr.l.p)
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_rej_bias_", format(Sys.time(), format = "%d-%m"), ".png"), height = 1150, width = 600)
print(g)
dev.off()