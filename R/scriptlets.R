#Load eValuesCrps
devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")

############ used for computational optimization
mu <- rnorm(100)
y <- rnorm(100, mu)
crps.fun.F <- \(x, j) { scoringRules::crps_norm(x, mu[j], 1) }
fun.F <- \(x, j) { pnorm(x, mu[j], 1) }
crps.fun.G <- \(x, j) { scoringRules::crps_norm(x, -mu[j], 1) }
fun.G <- \(x, j) { pnorm(x, mu[j] + 0.3, 1) }

start.time.opt <- Sys.time()
inf.crps <- sapply(seq_along(y),
                   \(i) { abs(optim_inf_value(\(x) { crps.fun.F(x, i) - crps.fun.G(x, i) }, min.value = -10, max.value = 10)) })
end.time.opt <- Sys.time()
print(end.time.opt - start.time.opt)

ggplot2::ggplot(tibble(x = seq(-10, 10)), ggplot2::aes(x = x, y = x)) +
  ggplot2::stat_function(fun = \(x) { fun.F(x, x + 10) - fun.G(x, x + 10) })

start.time.dif <- Sys.time()
opt.y <- sapply(seq_along(y), \(i) { uniroot(\(x) { 2 * (fun.F(x, i) - fun.G(x, i)) }, interval = c(-10, mu[i]), extendInt = "yes")$root })
inf.crps.dif <- abs(crps.fun.F(opt.y, seq_along(opt.y)) - crps.fun.G(opt.y, seq_along(opt.y)))
end.time.dif <- Sys.time()
print(end.time.dif - start.time.dif)

print(paste0("Difference: ", (end.time.opt - start.time.opt) - (end.time.dif - start.time.dif)))
# Conclusion: with uniroot not faster

############# accuracy for optim_inf with 3 or 2 start.points
mu <- rnorm(100)
y <- rnorm(100, mu)
crps.fun.F <- \(x, j) { scoringRules::crps_norm(x, mu[j], 1) }
crps.fun.G <- \(x, j) { scoringRules::crps_norm(x, -mu[j], 1) }

start.time.opt <- Sys.time()
inf.crps <- sapply(seq_along(y),
                   \(i) { abs(optim_inf_value(\(x) { crps.fun.F(x, i) - crps.fun.G(x, i) }, min.value = -10, max.value = 10, start.points = 5)) })
end.time.opt <- Sys.time()
print(end.time.opt - start.time.opt)

start.time.dif <- Sys.time()
inf.crps.dif <- sapply(seq_along(y),
                       \(i) { abs(optim_inf_value(\(x) { crps.fun.F(x, i) - crps.fun.G(x, i) }, min.value = -10, max.value = 10, start.points = 2)) })
end.time.dif <- Sys.time()
print(end.time.dif - start.time.dif)
print(paste0("Accuracy: ", sum(inf.crps == inf.crps.dif)))

print(paste0("Difference: ", (end.time.opt - start.time.opt) - (end.time.dif - start.time.dif),
             ", %-faster: ", 100 / as.numeric(end.time.opt - start.time.opt) * as.numeric((end.time.opt - start.time.opt) - (end.time.dif - start.time.dif))))
# Conclusion: between 98 - 100 % equality but up to 80 % faster => use only two intervals

######### load t.crps
t.crps <- getFile("/target/full-200-uncompacted-2022-07-14T13-13-26.rds")
t.result <- t.crps %>%
  group_by(names.F, names.G) %>%
  mutate(across(contains(c("p.value", "value.prod")), ~100 / 200 * sum(.x <= 0.05), .names = "{.col}.H0.rej")) %>%
  select(contains(c("names", "H0"))) %>%
  distinct() %>%
  arrange(names.F, names.G)

######### print rej rate for perfect-s/m
print_rej_rate_perfect_loosing_power(t.result)

######### print crps difference for perfect-s/m compared to perfect
print_crps_diff_histogram_loosing_power(t.crps) # where t.crps is the savedRDS "uncompacted"

# These do not give valuable informations
######### print crps difference for usual forecasts
print_crps_diff_histogram_usual_forecasts(t.crps)

pdf(paste0(getwd(), "/target/plot-it-1000.pdf"))
print_rej_rate_perfect_loosing_power(t.more)
dev.off()


## find the problem
n.obs <- 100
n.it <- 100
i <- 2
loosing.power.only <- TRUE
usual.forecasts <- FALSE
loosing.power.forecasts <- loosing.power.only
forecasts.input <- NA
lambda <- 0.5
method <- list("GRAPA", "lambda", "alternative")
p.value.method <- "t"

# print all the plots
devtools::load_all()
dt.l.p.f <- getFile("/target/run-300-1000-2022-09-23T19-48-00.rds")
printPlot("plot-300-1000-l-p-o-rej-rate-more-alternatives-2", print_rej_rate_perfect_loosing_power, dt.l.p.f)
printPlot("plot-300-1000-l-p-o-e-values-hist-more-alternatives", print_e_values_histogram_loosing_power, dt.l.p.f.2)
printPlot("plot-300-1000-l-p-o-crps-dif-hist-more-alternatives", print_crps_diff_histogram_loosing_power, dt.l.p.f.2)

rm(list = ls())
dt.u.f <- getFile("/target/run-300-1000-2022-09-02T14-40-30.rds")
printPlot("plot-300-1000-u-f-e-values-hist-more-alternatives", print_e_values_histogram_usual_forecasts, dt.u.f.2)
print_crps_diff_histogram_usual_forecasts(dt.u.f.2, "/target/plot-300-1000-u-f-crps-dif-hist-more-alternatives")


to.print <- dt.l.p.f$evaluated %>%
  filter(grepl("perfect", names.F) & names.G == 'perfect') %>%
  mutate(e = stringr::str_extract(names.F, "[.0-9]+"),
         mean.sd = stringr::str_extract(names.F, "perfect-[a-z]")
  ) %>%
  ungroup() %>%
  select(-c(names.F, names.G)) %>%
  tidyr::pivot_longer(!c(e, mean.sd), names_to = "key", values_to = "rej_rate") %>%
  mutate(key = paste0(mean.sd, "R_scripts", key)) %>%
  arrange(key) %>%
  select(-mean.sd)

pdf(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-300-1000-lambda-fix_2.pdf"), paper = "a4r")
g.m <- ggplot2::ggplot(to.print.lambda.fix %>% filter(grepl("-m", key)), ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
  ggplot2::geom_line(ggplot2::aes(group = key)) +
  ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::labs(x = "epsilon") +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
g.s <- ggplot2::ggplot(to.print.lambda.fix %>% filter(grepl("-s", key)), ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
  ggplot2::geom_line(ggplot2::aes(group = key)) +
  ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::labs(x = "epsilon") +
  ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
Rmisc::multiplot(g.m, g.s, cols = 2)
dev.off()

pdf(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-300-1000-lambda-fix_1.pdf"), paper = "a4r")
ggplot2::ggplot(to.print %>% filter(grepl("-m", key)), ggplot2::aes(x = as.numeric(e), y = rej_rate, color = key)) +
  ggplot2::geom_line(ggplot2::aes(group = key)) +
  ggplot2::geom_hline(yintercept = 5, linetype = "dotted") +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggplot2::labs(x = "epsilon")
dev.off()


df <- dt.u.f$evaluated
dt <- DT::datatable(df, escape = FALSE, rownames = FALSE, colnames = c("F", "G", "p-value", "lambda = 0.5", "GRAPA", "alt., conf.", "alt., cons.", "alt., more cons.", "mean cons. & conf.", "mean cons., conf., more conf."),
                    extensions = 'Buttons', options = list(dom = 't', pageLength = 15,
                                                           columnDefs = list(list(visible = FALSE, targets = 4:(length(dt) + 1))), bFilter = 0, bInfo = 0, bPaginate = 0))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)

# with clickable buttons all displayed not drop down menu
dt <- DT::datatable(df, escape = FALSE, rownames = FALSE, colnames = c("F", "G", "p-value", "lambda = 0.5", "GRAPA", "alt., conf.", "alt., cons.", "alt., more cons.", "mean cons. & conf.", "mean cons., conf., more conf."),
                    extensions = 'Buttons', options = list(dom = 'Bfrtip', pageLength = 15,
                                                           buttons = list(list(extend = 'colvisGroup', text = "ALL", show = 0:(length(dt) + 1)),
                                                                          list(extend = 'colvisGroup', text = "lambda = 0.5", show = 0:3, hide = 4:(length(dt) + 1)),
                                                                          list(extend = 'colvisGroup', text = "GRAPA", show = 0:4, hide = 5:(length(dt) + 1)),
                                                                          list(extend = 'colvisGroup', text = "alternative", show = c(0:2, 5:7), hide = c(3:4, 8:(length(dt) + 1))),
                                                                          list(extend = 'colvisGroup', text = "mean of resulting e-values", show = c(0:2, 8:(length(dt) + 1)), hide = 3:7)),
                                                           columnDefs = list(list(visible = FALSE, targets = 4:(length(dt) + 1))), bFilter = 0, bInfo = 0, bPaginate = 0))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)

dt <- df %>%
  DT::datatable(escape = FALSE, rownames = FALSE, colnames = c("F", "G", "p-value", "lambda = 0.5", "GRAPA", "alt., conf.", "alt., cons.", "alt., more cons.", "mean cons. & conf.", "mean cons., conf., more conf."),
                options = list(dom = 't', pageLength = 15,
                               columnDefs = list(list(visible = FALSE, targets = c(3:7))),
                               bFilter = 0, bInfo = 0, bPaginate = 0, initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'font-size': '120%'});",
                    "}"))) %>%
  DT::formatStyle(columns = 0:(length(dt) + 1), fontSize = '120%')

html <- "target/usual-forecasts-mean.html"
saveWidget(dt, html, selfcontained = FALSE)
webshot::webshot(html, "target/usual-forecasts-mean.png")

it <- 1
lambda <- 0.5
p.value.method <- "t"
method <- list("GRAPA", "lambda", "alternative")
n.obs <- 300
mu <- rnorm(n.obs)
y <- rnorm(n.obs, mu)
tau <- sample(c(-1, 1), n.obs, replace = TRUE)
crps.F.para <- list("mu" = mu, "sd" = 1, "main" = TRUE)
crps.G.para <- list("mu" = mu, "sd" = 1 + 0.01)
crps.G.para <- list("mu" = cbind(mu, mu + tau), "sd" = matrix(nrow = n.obs, ncol = 2, 1), "w" = matrix(nrow = n.obs, ncol = 2, 1 / 2))

forecasts.input <- NA
loosing.power.only <- TRUE
loosing.power.forecasts <- TRUE
usual.forecasts <- FALSE


#######################
f.dt.u.f.300 <- eValuesCrps::getFile("/target/run-300-1000-2022-10-21T14-58-17.rds")
f.dt.u.f.100 <- eValuesCrps::getFile("/target/run-100-1000-2022-10-21T16-13-09.rds")
f.dt.u.f.50 <- eValuesCrps::getFile("/target/run-50-1000-2022-10-21T17-55-41.rds")

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
#
# sketch <- htmltools::withTags(table(
#   class = 'display',
#   thead(
#     tr(
#       th(rowspan = 2, 'F'),
#       th(rowspan = 2, 'G'),
#       th(colspan = 3, 'p-value'),
#       th(colspan = 3, 'lambda = 0.5'),
#       th(colspan = 3, 'GRAPA'),
#       th(colspan = 3, 'alt., conf.'),
#       th(colspan = 3, 'alt., cons.'),
#       th(colspan = 3, 'alt., more cons.')
#     ),
#     tr(
#       lapply(rep(c(50, 100, 300), 6), th)
#     )
#   )
# ))
# print(sketch)
#
# q <- rbind(c("F", "G", "p-value", NA, NA, "lambda = 0.5", NA, NA, "GRAPA", NA, NA, "alt., conf.", NA, NA, "alt., cons.", NA, NA, "alt., more cons.", NA, NA),
#            c(NA, NA, rep(c(50, 100, 300), 6)))
# dt <- DT::datatable(dt.f, escape = FALSE, container = sketch, rownames = FALSE, options = list(dom = 'Bfrtip', pageLength = 15, bFilter = 0, bInfo = 0, bPaginate = 0))
# htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
# shell.exec(tf)
# html <- paste0("target/usual-forecasts-all-50-100-300.html")
# saveWidget(dt, html, selfcontained = FALSE)
# webshot::webshot(html, paste0("ma/ma_presentation/UFGTeX-Presentation-master/pictures/usual-forecasts-all-50-100-300.png"))

######################## print further forecasts for presentation

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

png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/print_further_sim_presentation_1.png"), height = 550, width = 900)
ggpubr::ggarrange(pe.cl, pe.sr, pe.un,
                  cl.pe, cl.sr, cl.un,
                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()
png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/print_further_sim_presentation_2.png"), height = 550, width = 900)
ggpubr::ggarrange(sr.pe, sr.cl, sr.un,
                  un.pe, un.cl, un.sr,
                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()
png(paste0(getwd(), "/ma/pictures/print_further_sim_presentation.png"), height = 950, width = 900)
ggpubr::ggarrange(pe.cl, pe.sr, pe.un,
                  cl.pe, cl.sr, cl.un,
                  sr.pe, sr.cl, sr.un,
                  un.pe, un.cl, un.sr,
                  ncol = 3, nrow = 4, common.legend = TRUE, legend = "bottom")
dev.off()


##### mutliplot for rejection rates
png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-50-100-300-it-1000_m.png"), height = 550, width = 900)
ggpubr::ggarrange(g.50, g.100, g.300,
                  ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-50-100-300-it-1000_s.png"), height = 550, width = 900)
ggpubr::ggarrange(s.50, s.100, s.300,
                  ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()


### Problem with unfocused
## find the problem
n.obs <- 100
n.it <- 100
i <- 2
mu <- rnorm(n.obs)
y <- rnorm(n.obs, mu)
tau <- sample(c(-1, 1), n.obs, replace = TRUE)
loosing.power.only <- TRUE
usual.forecasts <- FALSE
loosing.power.forecasts <- loosing.power.only
forecasts.input <- list(
  "perfect" = list("mu" = mu, "sd" = 1, "main" = TRUE),
  "climatological" = list("mu" = 0, "sd" = 2),
  "unfocused" = list("mu" = cbind(mu, mu + tau), "sd" = matrix(nrow = n.obs, ncol = 2, 1), "w" = matrix(nrow = n.obs, ncol = 2, 1 / 2))
)
lambda <- 0.5
method <- list("GRAPA", "lambda")
p.value.method <- "t"

e.p.u <- eValuesCrps::e_value(y, forecasts.input$perfect, forecasts.input$unfocused, it = i, method = method, lambda = lambda, p.value.method = p.value.method)