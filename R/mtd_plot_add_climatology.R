mtd_plot_add_climatology <- function(x,
                                     col = "black",
                                     mean = FALSE){
  list(
    if("normal.000" %in% names(x))
      ggplot2::geom_ribbon(ggplot2::aes(x = day,
                                        ymin = normal.000,
                                        ymax = normal.100),
                           data = x,
                           fill = col,
                           alpha = 0.2),
    # ggplot2::geom_ribbon(ggplot2::aes(x = YDAY,
    #                                   ymin = `2.5%`,
    #                                   ymax = `97.5%`),
    #                      data = x,
    #                      fill = col,
    #                      alpha = 0.3),
    # if("25%" %in% names(x))
    #   ggplot2::geom_ribbon(ggplot2::aes(x = YDAY,
    #                                     ymin = `25%`,
    #                                     ymax = `75%`),
    #                        data = x,
    #                        fill = col,
    #                        alpha = 0.4),
    # if("50%" %in% names(x))
    #   ggplot2::geom_line(ggplot2::aes(x = YDAY,
    #                                   y = `50%`),
    #                      data = x,
    #                      color = col,
    #                      alpha = 1,
    #                      linetype = 2),
    if(mean && "normal.mean" %in% names(x))
      ggplot2::geom_line(ggplot2::aes(x = day,
                                      y = normal.mean),
                         data = x,
                         color = col,
                         linetype = 2,
                         alpha = 1)
  )
}
