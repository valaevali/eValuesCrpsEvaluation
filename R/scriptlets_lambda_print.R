print_further_forecasts_lambda <- function(to.print) {
  p <- ggplot2::ggplot(to.print, ggplot2::aes(x = n, y = lambda.grapa, color = it)) +
    ggplot2::geom_line(ggplot2::aes(group = it)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("number of obs") +
    ggplot2::ylab("lambda GRAPA") +
    ggplot2::facet_wrap(~ name, ncol = 3)
  return(p)
}

dt.lambda.u.f.300 <- dt.u.f.300$uncompacted %>%
  select(names.F, names.G, lambda.grapa, it) %>%
  tidyr::unnest(lambda.grapa) %>%
  arrange(it)
dt.lambda.u.f <- dt.lambda.u.f.300 %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  group_by(it, name) %>%
  mutate(n = 1:n())

g <- print_further_forecasts_lambda(dt.lambda.u.f)
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_lambda_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
print(g)
dev.off()