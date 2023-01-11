print_further_forecasts_crps_diff <- function(to.print) {
  p <- ggplot2::ggplot(to.print, ggplot2::aes(x = n, y = diff, color = it)) +
    ggplot2::geom_line(ggplot2::aes(group = it)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_cartesian(ylim = c(-8,8)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("number of obs") +
    ggplot2::ylab("difference crps") +
    ggplot2::facet_wrap(~name, ncol = 3)
  return(p)
}

dt.crps.diff.u.f.300 <- dt.u.f.300$uncompacted %>%
  filter(it <= 50) %>%
  select(names.F, names.G, crps.F, crps.G, it) %>%
  tidyr::unnest(c(crps.F,crps.G)) %>%
  mutate(diff = crps.F - crps.G,
         name = paste(names.F, "vs", names.G)) %>%
  arrange(it) %>%
  select(-c(crps.F, crps.G)) %>%
  group_by(it, name) %>%
  mutate(n = 1:n())

g <- print_further_forecasts_crps_diff(dt.crps.diff.u.f.300)
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_diff_crps_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
print(g)
dev.off()