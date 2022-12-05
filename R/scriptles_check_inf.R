devtools::load_all()

dt.l.p.f <- eValuesCrps::sim_e_values(n.obs = 50, n.it = 50, loosing.power.only = TRUE, usual.forecasts = FALSE)

# dt.l.p.f <- getFile("/target/run-50-50-2022-10-15T12-26-21.rds")

dt.check.inf <- f.dt.u.f.300$uncompacted %>%
  filter(grepl("unfocused", names.F) | names.G == 'unfocused') %>%
  select(names.F, names.G, crps.F, crps.G, inf.crps) %>%
  tidyr::unnest(contains("crps.F")) %>%
  filter(names(crps.F.fun) == "fun") %>%
  tidyr::unnest(contains("crps.G")) %>%
  filter(names(crps.G.fun) == "fun") %>%
  tidyr::unnest(inf.crps) %>%
  mutate(inf.crps = as.numeric(format(inf.crps, nsmall = 4))) %>%
  arrange(names.F) %>%
  distinct()

print_inf <- function(line, from = -10, to = 5) {
  print(line)
  x <- seq(from, to, by = 0.01)
  fun <- \(y) { sapply(y, \(i) {dt.check.inf[line, 1:5]$crps.F.fun$fun(i)}) - sapply(y, \(i) {dt.check.inf[line, 1:5]$crps.G.fun$fun(i) })}
  plot(x, sort(fun(x)), type = "l", col = "red")
  print(dt.check.inf[line, 1:5]$inf.crps)
  abline(h = dt.check.inf[line, 1:5]$inf.crps)
  abline(h = -dt.check.inf[line, 1:5]$inf.crps)
}

print_inf(55)
print_inf(55, -5,10)
print_inf(703) # TODO: here it is false => sd_1 != sd_2
print_inf(703, -10,10) # TODO: here it is false => sd_1 != sd_2
print_inf(961) # TODO: here it is false => sd_1 != sd_2
print_inf(sample(1:dim(dt.check.inf)[1],1))


##########################
devtools::load_all()

mu <- rnorm(50)
y <- rnorm(50, mu)
crps.F.para <- list("mu" = mu, "sd" = 1, "main" = TRUE)
crps.G.para <- list("mu" = mu, "sd" = 1.02)
evalue <- e_value(y, crps.F.para, crps.G.para)

crps.F.para <- base::append(crps.F.para, rlang::exec(create_crps_fun, length(y), !!!crps.F.para))
crps.G.para <- base::append(crps.G.para, rlang::exec(create_crps_fun, length(y), !!!crps.G.para))

which <- 10
inf.2 <- abs(optim_inf_value(\(x) { crps.F.para$inf.fun(x, which) - crps.G.para$inf.fun(x, which) },
                    min.value = -10, max.value = 10))
null.value <- (sqrt(2 * crps.F.para$sd) * crps.G.para$mu - sqrt(2 * crps.G.para$sd) * crps.F.para$mu) /
  (sqrt(2 * crps.F.para$sd) - sqrt(2 * crps.G.para$sd))
null.value.min <- min(null.value)
x <- seq(-10,10, by=0.01)
f <- \(y) {crps.F.para$inf.fun(y,which) - crps.G.para$inf.fun(y,which)}

plot(x, sort(f(x)), type="l", col = "red", ylim = c(-0.05, 0.05))
# abline(h = evalue$inf.crps, col = "green")
# abline(h = -evalue$inf.crps, col = "darkgreen")
abline(h = (crps.F.para$inf.fun(min(null.value), which) - crps.G.para$inf.fun(min(null.value),which)))
abline(h = inf.2, col = "yellow")
abline(h = -inf.2, col = "orange")

abline(h = inf.1, col = "green")
abline(h = -inf.1, col = "green")
abline(h = inf.2, col = "blue")
abline(h = -inf.2, col = "blue")

plot(x, sort(crps.G.para$fun(x)), col = "red", type = "l")
abline(x, sort(crps.G.para$fun(x)), col = "green")