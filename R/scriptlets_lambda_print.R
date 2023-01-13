print_further_forecasts_lambda <- function(to.print, lambda.name) {
  p <- ggplot2::ggplot(to.print, ggplot2::aes(x = n, y = lambda, color = it)) +
    ggplot2::geom_line(ggplot2::aes(group = it)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18), axis.text = ggplot2::element_text(size = 12), axis.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("number of obs") +
    ggplot2::ylab(lambda.name) +
    ggplot2::facet_wrap(~ name, ncol = 3)
  return(p)
}

# lambda grapa
dt.lambda.grapa.u.f.300 <- dt.u.f.300$uncompacted %>%
  select(names.F, names.G, lambda = lambda.grapa, it) %>%
  tidyr::unnest(lambda) %>%
  arrange(it)
dt.lambda.grapa.u.f <- dt.lambda.grapa.u.f.300 %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  group_by(it, name) %>%
  mutate(n = 1:n())

g <- print_further_forecasts_lambda(dt.lambda.grapa.u.f, "lambda GRAPA")
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_lambda_grapa_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
print(g)
dev.off()

### lambda conservative
dt.lambda.cons.u.f.300 <- dt.u.f.300$uncompacted %>%
  select(names.F, names.G, lambda = lambda.alt.cons, it) %>%
  tidyr::unnest(lambda) %>%
  arrange(it)
dt.lambda.cons.u.f <- dt.lambda.cons.u.f.300 %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  group_by(it, name) %>%
  mutate(n = 1:n())

g <- print_further_forecasts_lambda(dt.lambda.cons.u.f, "lambda cons")
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_lambda_cons_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
print(g)
dev.off()

### lambda more conservative
dt.lambda.more.cons.u.f.300 <- dt.u.f.300$uncompacted %>%
  select(names.F, names.G, lambda = lambda.alt.more.cons, it) %>%
  tidyr::unnest(lambda) %>%
  arrange(it)
dt.lambda.more.cons.u.f <- dt.lambda.more.cons.u.f.300 %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  group_by(it, name) %>%
  mutate(n = 1:n())

g <- print_further_forecasts_lambda(dt.lambda.more.cons.u.f, "lambda more cons")
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_lambda_more_cons_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
print(g)
dev.off()

### lambda conf
dt.lambda.conf.u.f.300 <- dt.u.f.300$uncompacted %>%
  select(names.F, names.G, lambda = lambda.alt.conf, it) %>%
  tidyr::unnest(lambda) %>%
  arrange(it)
dt.lambda.conf.u.f <- dt.lambda.conf.u.f.300 %>%
  mutate(name = paste(names.F, "vs", names.G)) %>%
  select(-c(names.F, names.G)) %>%
  group_by(it, name) %>%
  mutate(n = 1:n())

g <- print_further_forecasts_lambda(dt.lambda.conf.u.f, "lambda conf")
png(paste0("C:/Users/valer/Documents/UNI/MA/evalues/ma/pictures/print_further_sim_lambda_conf_", format(Sys.time(), format = "%m-%d"), ".png"), height = 950, width = 900)
print(g)
dev.off()