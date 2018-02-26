save_mt_map <- function(x, name){
  x %>%
    ggplot2::ggsave(name,
                    plot = .,
                    device = "pdf",
                    path = "./figures/",
                    width = 4.45,
                    height = 2.54)
}
