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
dt.rr.f <- dt.rr.u.f.300 %>%
  ungroup() %>%
  add_row(dt.rr.u.f.100) %>%
  add_row(dt.rr.u.f.50) %>%
  add_row(dt.rr.u.f.25) %>%
  add_row(dt.rr.u.f.10) %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  tidyr::pivot_longer(!c(name, it), names_to = "key", values_to = "rej_rate") %>%
  mutate(key = stringr::str_replace(key, "(.prod|).H0.rej", "")) %>%
  select(n.obs = it, name, key, rej_rate)

g <- ggplot2::ggplot(dt.rr.f, ggplot2::aes(x = n.obs, y = rej_rate, group = key)) +
  ggplot2::geom_line(ggplot2::aes(color = key), linewidth = 0.5) +
  ggplot2::scale_x_continuous(limits = c(10, 300), breaks = c(10, 25, 50, 100, 300)) +
  ggplot2::scale_y_continuous(limits = c(0, 100)) +
  ggplot2::geom_line(data = filter(dt.rr.f, key == "p.value"), linewidth = 1, color = "red") +
  ggplot2::scale_colour_manual(values = c("blue", "maroon", "cyan", "darkgreen", "darkorange", "red")) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::xlab("k") +
  ggplot2::ylab("Rej. rate") +
  ggplot2::facet_wrap(~name, ncol = 3)

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_rej_rate_", format(Sys.time(), format = "%d-%m"), ".png"), height = 950, width = 900)
print(g)
dev.off()


########################## loosing power
dt.rr.l.p.10 <- dt.l.p.f.10$evaluated %>%
  mutate(it = 10)
dt.rr.l.p.25 <- dt.l.p.f.25$evaluated %>%
  mutate(it = 25)
dt.rr.l.p.50 <- dt.l.p.f.50$evaluated %>%
  mutate(it = 50)
dt.rr.l.p.100 <- dt.l.p.f.100$evaluated %>%
  mutate(it = 100)
dt.rr.l.p.300 <- dt.l.p.f.300$evaluated %>%
  mutate(it = 300)
dt.rr.l.p <- dt.rr.l.p.300 %>%
  ungroup() %>%
  add_row(dt.rr.l.p.100) %>%
  add_row(dt.rr.l.p.50) %>%
  add_row(dt.rr.l.p.25) %>%
  add_row(dt.rr.l.p.10) %>%
  filter(grepl("perfect", names.F) & names.G == 'perfect') %>%
  mutate(e = stringr::str_extract(names.F, "[.0-9]+"),
         m.s = stringr::str_extract(names.F, "perfect-[a-z]"),
         m.s = stringr::str_extract(m.s, "[a-z]$"),
         m.s = ifelse(m.s == 'm', "N(mu + e, sd^s)", "N(mu, sd^2 + e)")
  ) %>%
  select(-c(names.F, names.G)) %>%
  tidyr::pivot_longer(!c(m.s, it, e), names_to = "key", values_to = "rej_rate") %>%
  mutate(key = stringr::str_replace(key, "(.prod|).H0.rej", "")) %>%
  select(n.obs = it, m.s, e, key, rej_rate)

g <- ggplot2::ggplot(dt.rr.l.p, ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
  ggplot2::geom_line(ggplot2::aes(group = key), linewidth = 0.5) +
  ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::ylab("Rej. rate") +
  ggplot2::xlab("e") +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(text = ggplot2::element_text(size = 18)) +
  ggplot2::scale_colour_manual(values = c("blue", "maroon", "cyan", "darkgreen", "darkorange", "red")) +
  ggplot2::geom_line(data = filter(dt.rr.l.p, key == "p.value"), size = 1) +
  ggplot2::facet_grid(n.obs ~ m.s)

png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_rej_bias_", format(Sys.time(), format = "%d-%m"), ".png"), height = 1150, width = 600)
print(g)
dev.off()

########## print DT for usual forecasts.
dt.rr.u.f.300.dt <- dt.u.f.300$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".300")))
dt.rr.u.f.100.dt <- dt.u.f.100$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".100")))
dt.rr.u.f.50.dt <- dt.u.f.50$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".50")))
dt.rr.u.f.25.dt <- dt.u.f.25$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".25")))
dt.rr.u.f.10.dt <- dt.u.f.10$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".10")))
dt.rr.f.dt <- merge(dt.rr.u.f.300.dt, dt.rr.u.f.100.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.f.dt <- merge(dt.rr.f.dt, dt.rr.u.f.50.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.f.dt <- merge(dt.rr.f.dt, dt.rr.u.f.25.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.f.dt <- merge(dt.rr.f.dt, dt.rr.u.f.10.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.f.dt <- dt.rr.f.dt %>%
  select(names.F, names.G, p.value.H0.rej.10, p.value.H0.rej.25, p.value.H0.rej.50, p.value.H0.rej.100, p.value.H0.rej.300,
         e.value.lambda.prod.H0.rej.10, e.value.lambda.prod.H0.rej.25, e.value.lambda.prod.H0.rej.50, e.value.lambda.prod.H0.rej.100, e.value.lambda.prod.H0.rej.300,
         e.value.grapa.prod.H0.rej.10, e.value.grapa.prod.H0.rej.25, e.value.grapa.prod.H0.rej.50, e.value.grapa.prod.H0.rej.100, e.value.grapa.prod.H0.rej.300,
         e.value.alt.conf.prod.H0.rej.10, e.value.alt.conf.prod.H0.rej.25, e.value.alt.conf.prod.H0.rej.50, e.value.alt.conf.prod.H0.rej.100, e.value.alt.conf.prod.H0.rej.300,
         e.value.alt.cons.prod.H0.rej.10, e.value.alt.cons.prod.H0.rej.25, e.value.alt.cons.prod.H0.rej.50, e.value.alt.cons.prod.H0.rej.100, e.value.alt.cons.prod.H0.rej.300,
         e.value.alt.more.cons.prod.H0.rej.10, e.value.alt.more.cons.prod.H0.rej.25, e.value.alt.more.cons.prod.H0.rej.50, e.value.alt.more.cons.prod.H0.rej.100, e.value.alt.more.cons.prod.H0.rej.300
  )

sketch <- htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'F'),
      th(rowspan = 2, 'G'),
      th(colspan = 5, 'p-value'),
      th(colspan = 5, 'lambda = 0.5'),
      th(colspan = 5, 'GRAPA'),
      th(colspan = 5, 'alt., conf.'),
      th(colspan = 5, 'alt., cons.'),
      th(colspan = 5, 'alt., more cons.')
    ),
    tr(
      lapply(rep(c(10, 25, 50, 100, 300), 6), th)
    )
  )
))

dt <- DT::datatable(dt.rr.f.dt, escape = FALSE, container = sketch, rownames = FALSE, options = list(dom = 'Bfrtip', pageLength = 15, bFilter = 0, bInfo = 0, bPaginate = 0))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)


########## DT for loosing power
dt.rr.l.p.10.dt <- dt.l.p.f.10$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".10")))
dt.rr.l.p.25.dt <- dt.l.p.f.25$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".25")))
dt.rr.l.p.50.dt <- dt.l.p.f.50$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".50")))
dt.rr.l.p.100.dt <- dt.l.p.f.100$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".100")))
dt.rr.l.p.300.dt <- dt.l.p.f.300$evaluated %>%
  rename_at(vars(contains("value")), list(~paste0(., ".300")))
dt.rr.l.p.dt <- merge(dt.rr.l.p.300.dt, dt.rr.l.p.100.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.l.p.dt <- merge(dt.rr.l.p.dt, dt.rr.l.p.50.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.l.p.dt <- merge(dt.rr.l.p.dt, dt.rr.l.p.25.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.l.p.dt <- merge(dt.rr.l.p.dt, dt.rr.l.p.10.dt, by = c('names.F' = 'names.F', 'names.G' = 'names.G'))
dt.rr.l.p.dt <- dt.rr.l.p.dt %>%
  select(names.F, names.G, p.value.H0.rej.10, p.value.H0.rej.25, p.value.H0.rej.50, p.value.H0.rej.100, p.value.H0.rej.300,
         e.value.lambda.prod.H0.rej.10, e.value.lambda.prod.H0.rej.25, e.value.lambda.prod.H0.rej.50, e.value.lambda.prod.H0.rej.100, e.value.lambda.prod.H0.rej.300,
         e.value.grapa.prod.H0.rej.10, e.value.grapa.prod.H0.rej.25, e.value.grapa.prod.H0.rej.50, e.value.grapa.prod.H0.rej.100, e.value.grapa.prod.H0.rej.300,
         e.value.alt.conf.prod.H0.rej.10, e.value.alt.conf.prod.H0.rej.25, e.value.alt.conf.prod.H0.rej.50, e.value.alt.conf.prod.H0.rej.100, e.value.alt.conf.prod.H0.rej.300,
         e.value.alt.cons.prod.H0.rej.10, e.value.alt.cons.prod.H0.rej.25, e.value.alt.cons.prod.H0.rej.50, e.value.alt.cons.prod.H0.rej.100, e.value.alt.cons.prod.H0.rej.300,
         e.value.alt.more.cons.prod.H0.rej.10, e.value.alt.more.cons.prod.H0.rej.25, e.value.alt.more.cons.prod.H0.rej.50, e.value.alt.more.cons.prod.H0.rej.100, e.value.alt.more.cons.prod.H0.rej.300
  )
dt <- DT::datatable(dt.rr.l.p.dt, escape = FALSE, container = sketch, rownames = FALSE, options = list(dom = 'Bfrtip', pageLength = 15, bFilter = 0, bInfo = 0, bPaginate = 0))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)