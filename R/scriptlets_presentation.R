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

p.50 <- print_rej_presentation(dt.l.p.o.50, 50)
p.100 <- print_rej_presentation(dt.l.p.o.100, 100)
p.300 <- print_rej_presentation(dt.l.p.o.300, 300)

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
