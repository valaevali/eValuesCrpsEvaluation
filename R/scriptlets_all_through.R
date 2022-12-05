devtools::install_github("valaevali/eValuesCrps")
library("eValuesCrps")
library("dplyr")
library("scoringRules")
library("DT")
library("doParallel")

n.obs <- 300
n.it <- 1000
n.it <- 50

dt.l.p.f.50 <- sim_e_values(n.obs = 50, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.u.f.50 <- sim_e_values(n.obs = 50, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.l.p.f.100 <- sim_e_values(n.obs = 100, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.u.f.100 <- sim_e_values(n.obs = 100, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)
dt.l.p.f.300 <- sim_e_values(n.obs = 300, n.it = n.it, loosing.power.only = TRUE, usual.forecasts = FALSE)
dt.u.f.300 <- sim_e_values(n.obs = 300, n.it = n.it, loosing.power.only = FALSE, usual.forecasts = TRUE)

f.dt.l.p.o.300 <- getFile("/target/run-300-1000-2022-10-21T13-22-52.rds")
f.dt.l.p.o.100 <- getFile("/target/run-100-1000-2022-10-21T15-52-49.rds")
f.dt.l.p.o.50 <- getFile("/target/run-50-1000-2022-10-21T17-38-15.rds")

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
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
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
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::scale_colour_manual(values = c("blue", "cornflowerblue", "cyan", "darkgreen", "darkolivegreen3", "red"), name = NULL) +
    ggplot2::geom_line(data = filter(to.print %>%
                                       filter(grepl("-s", mean.sd)) %>%
                                       select(-(mean.sd)), key == "p.value"), size = 2)
  return(list("m" = m, "s" = s))
}

p.50 <- print_rej_presentation(f.dt.l.p.o.50, 50)
p.100 <- print_rej_presentation(f.dt.l.p.o.100, 100)
p.300 <- print_rej_presentation(f.dt.l.p.o.300, 300)

png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-50-100-300-it-1000_m.png"), height = 550, width = 900)
ggpubr::ggarrange(p.50$m, p.100$m, p.300$m,
                  ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-50-100-300-it-1000_s.png"), height = 550, width = 900)
ggpubr::ggarrange(p.50$s, p.100$s, p.300$s,
                  ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

png(paste0(getwd(), "/ma/ma_presentation/UFGTeX-Presentation-master/pictures/rej-50-100-300-it-1000_b.png"), height = 550, width = 900)
ggpubr::ggarrange(p.50$m, p.100$m, p.300$m, p.50$s, p.100$s, p.300$s,
                  ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()

printPlot(paste0("plot-", n.obs, "-", n.it, "-l-p-o-rej-rate"), print_rej_rate_perfect_loosing_power, dt.l.p.f)
printPlot(paste0("plot-", n.obs, "-", n.it, "-l-p-o-e-values-hist"), print_e_values_histogram_loosing_power, dt.l.p.f)
printPlot(paste0("plot-", n.obs, "-", n.it, "-l-p-o-crps-dif-hist"), print_crps_diff_histogram_loosing_power, dt.l.p.f)

# usual forecasts
rm(list = ls())
n.obs <- 50
n.it <- 1000
dt.u.f <- sim_e_values(n.obs = n.obs, n.it = n.it)
printPlot(paste0("plot-", n.obs, "-", n.it, "-u-f-e-values-hist"), print_e_values_histogram_usual_forecasts, dt.u.f)
print_crps_diff_histogram_usual_forecasts(dt.u.f, paste0("/target/plot-", n.obs, "-", n.it, "-u-f-crps-dif-hist"))

df <- dt.u.f$evaluated
dt <- DT::datatable(df, escape = FALSE, rownames = FALSE, colnames = c("F", "G", "p-value", "lambda = 0.5", "GRAPA", "alt., conf.", "alt., cons.", "alt., more cons."),
                    extensions = 'Buttons', options = list(dom = 'Bfrtip', pageLength = 15,
                                                           buttons = list(list(extend = 'colvisGroup', text = "ALL", show = 0:(length(dt) + 1)),
                                                                          list(extend = 'colvisGroup', text = "lambda = 0.5", show = 0:3, hide = 4:(length(dt) + 1)),
                                                                          list(extend = 'colvisGroup', text = "GRAPA", show = 0:4, hide = 5:(length(dt) + 1)),
                                                                          list(extend = 'colvisGroup', text = "alternative", show = c(0:2, 5:7), hide = c(3:4, 8:(length(dt) + 1)))),
                                                           columnDefs = list(list(visible = FALSE, targets = 4:(length(dt) + 1))), bFilter = 0, bInfo = 0, bPaginate = 0))
htmlwidgets::saveWidget(dt, tf <- tempfile(fileext = ".html"), selfcontained = FALSE)
shell.exec(tf)

dt <- DT::datatable(df, escape = FALSE, rownames = FALSE, colnames = c("F", "G", "p-value", "lambda = 0.5", "GRAPA", "alt., conf.", "alt., cons.", "alt., more cons."),
                    options = list(dom = 't', pageLength = 15,
                                   columnDefs = list(list(visible = TRUE, targets = 0:(length(dt) + 1))),
                                   bFilter = 0, bInfo = 0, bPaginate = 0))
html <- paste0("target/usual-forecasts-all-", n.obs, "-", n.it, ".html")
saveWidget(dt, html, selfcontained = FALSE)
webshot::webshot(html, paste0("ma/ma_presentation/UFGTeX-Presentation-master/pictures/usual-forecasts-all-", n.obs, "-", n.it, ".png"))