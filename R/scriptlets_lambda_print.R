print_further_forecasts_lambda <- function(dt.f, f, g) {
  to.print <- dt.f %>%
    filter(names.F == f & names.G == g) %>%
    select(-c(names.F, names.G)) %>% group_by(it) %>%
    mutate(n = 1:n())

  p <- ggplot2::ggplot(to.print, ggplot2::aes(x = n, y = lambda.grapa, color = it)) +
    ggplot2::geom_line(ggplot2::aes(group = it)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(paste(f, "vs", g)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("number of obs") +
    ggplot2::ylab("lambda GRAPA")
  return(p)
}

print_for_each_k_lambdas <- function(dt.f, n.obs) {
  cl.pe <- print_further_forecasts_lambda(dt.f, 'climatological', 'perfect')
  cl.sr <- print_further_forecasts_lambda(dt.f, 'climatological', 'sign-reversed')
  cl.un <- print_further_forecasts_lambda(dt.f, 'climatological', 'unfocused')

  pe.cl <- print_further_forecasts_lambda(dt.f, 'perfect', 'climatological')
  pe.sr <- print_further_forecasts_lambda(dt.f, 'perfect', 'sign-reversed')
  pe.un <- print_further_forecasts_lambda(dt.f, 'perfect', 'unfocused')

  sr.cl <- print_further_forecasts_lambda(dt.f, 'sign-reversed', 'climatological')
  sr.pe <- print_further_forecasts_lambda(dt.f, 'sign-reversed', 'perfect')
  sr.un <- print_further_forecasts_lambda(dt.f, 'sign-reversed', 'unfocused')

  un.cl <- print_further_forecasts_lambda(dt.f, 'unfocused', 'climatological')
  un.pe <- print_further_forecasts_lambda(dt.f, 'unfocused', 'perfect')
  un.sr <- print_further_forecasts_lambda(dt.f, 'unfocused', 'sign-reversed')

  png(paste0("C:/Users/valer/Documents/UNI/Masterarbeit/evalues/ma/pictures/print_further_sim_lambda-nobs-", n.obs, "_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
  g <- ggpubr::ggarrange(pe.cl, pe.sr, pe.un,
                    cl.pe, cl.sr, cl.un,
                    sr.pe, sr.cl, sr.un,
                    un.pe, un.cl, un.sr,
                    ncol = 3, nrow = 4, common.legend = TRUE, legend = "bottom")
  print(g)
  dev.off()
}

dt.lambda.u.f.300 <- dt.u.f.300$uncompacted %>%
  select(names.F, names.G, lambda.grapa, it) %>%
  tidyr::unnest(lambda.grapa) %>%
  arrange(it)
dt.lambda.u.f.100 <- dt.u.f.100$uncompacted %>%
  select(names.F, names.G, lambda.grapa, it) %>%
  tidyr::unnest(lambda.grapa) %>%
  arrange(it)
dt.lambda.u.f.50 <- dt.u.f.50$uncompacted %>%
  select(names.F, names.G, lambda.grapa, it) %>%
  tidyr::unnest(lambda.grapa) %>%
  arrange(it)
dt.lambda.u.f.25 <- dt.u.f.25$uncompacted %>%
  select(names.F, names.G, lambda.grapa, it) %>%
  tidyr::unnest(lambda.grapa) %>%
  arrange(it)
dt.lambda.u.f.10 <- dt.u.f.10$uncompacted %>%
  select(names.F, names.G, lambda.grapa, it) %>%
  tidyr::unnest(lambda.grapa) %>%
  arrange(it)

print_for_each_k_lambdas(dt.lambda.u.f.10, 10)
print_for_each_k_lambdas(dt.lambda.u.f.25, 25)
print_for_each_k_lambdas(dt.lambda.u.f.50, 50)
print_for_each_k_lambdas(dt.lambda.u.f.100, 100)
print_for_each_k_lambdas(dt.lambda.u.f.300, 300)