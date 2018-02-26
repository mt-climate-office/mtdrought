mdt_theme_map <- function(base_size = 6.5, base_family = ""){
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = 'transparent'),
                   panel.grid.minor = ggplot2::element_line(colour = 'transparent'),
                   panel.spacing = ggplot2::unit(0,"lines"),
                   legend.justification = c(0,0),
                   legend.position = c(-0.02,0),
                   legend.background = ggplot2::element_blank(),
                   legend.key.width=unit(0.15,"in"),
                   legend.key.height=unit(0.15,"in"),
                   legend.margin = margin(t = 0, r = -0.2, b = 0, l = 0, unit = "in"),
                   # legend.text = element_text(margin = margin(r = -0.1, unit = "in")),
                   plot.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(0,0,0,10)
    )
}
