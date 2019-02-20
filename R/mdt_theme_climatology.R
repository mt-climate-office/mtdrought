mtd_theme_climatology <- function(ybreaks,
                                  title = NULL,
                                  polar = TRUE){
  month_lengths = c(Jan = 31,
                    Feb = 28,
                    Mar = 31,
                    Apr = 30,
                    May = 31,
                    Jun = 30,
                    Jul = 31,
                    Aug = 31,
                    Sep = 30,
                    Oct = 31,
                    Nov = 30,
                    Dec = 31)
  
  list(  
    if(polar) 
      ggplot2::coord_polar(),
    ggplot2::geom_hline(yintercept = ybreaks, 
                        color = "grey60", 
                        size = 0.3,
                        linetype = 2,
                        na.rm = TRUE),
    ggplot2::geom_vline(xintercept = c(1, 
                                       month_lengths %>%
                                         cumsum() %>%
                                         head(11),
                                       365), 
                        color = "grey60", 
                        size = 0.3,
                        linetype = 2,
                        na.rm = TRUE),
    if(polar)
      ggplot2::geom_hline(yintercept = tail(ybreaks,1), 
                          color = "grey60", 
                          size = 0.5,
                          linetype = 1,
                          na.rm = TRUE),
    if(polar) 
      ggplot2::annotate(geom = "text", 
                        y = c(ybreaks[-1],
                              ybreaks[-1],
                              ybreaks[-1],
                              ybreaks[-1],
                              ybreaks[1]),
                        x = c(rep(1,length(ybreaks[-1])),
                              rep(182,length(ybreaks[-1])),
                              rep(90,length(ybreaks[-1])),
                              rep(273,length(ybreaks[-1])),
                              1),
                        label = c(ybreaks[-1],
                                  ybreaks[-1],
                                  ybreaks[-1],
                                  ybreaks[-1],
                                  ybreaks[1]),
                        hjust = c(rep(1.1,length(ybreaks[-1])),
                                  rep(-0.1,length(ybreaks[-1])),
                                  rep(-0.1,length(ybreaks[-1])),
                                  rep(1.1,length(ybreaks[-1])),
                                  0.5),
                        vjust = c(rep(-0.25,length(ybreaks[-1])),
                                  rep(1.25,length(ybreaks[-1])),
                                  rep(-0.25,length(ybreaks[-1])),
                                  rep(1.25,length(ybreaks[-1])),
                                  0.5),
                        size = 2.5),
    # else
    #   ggplot2::annotate(geom = "text", 
    #                     y = ybreaks,
    #                     x = rep(1,length(ybreaks)),
    #                     label = ybreaks,
    #                     hjust = 1.25,
    #                     vjust = 0.5,
    #                     size = 3.5)
    ggplot2::scale_x_continuous(
      limits = c(1,365),
      minor_breaks = c(1, month_lengths %>%
                         cumsum() %>%
                         head(11)),
      breaks = c(1, month_lengths %>%
                   cumsum() %>%
                   head(11)) + (month_lengths/2),
      labels = names(month_lengths),
      expand = c(0,0)
    ),
    ggplot2::scale_y_continuous(limits = c(head(ybreaks, 1), tail(ybreaks, 1)),
                                expand = c(0,0)),
    ggplot2::theme_minimal(),
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = element_blank(),
                   axis.text.x = ggplot2::element_text(size = 8),
                   panel.grid = element_blank(),
                   plot.title = ggplot2::element_text(size = 10)),
    if(!polar)
      ggplot2::theme(plot.margin = ggplot2::margin(t = 20,
                                                   r = 20,
                                                   b = 20,
                                                   l = 20),
                     axis.text.x = ggplot2::element_text(size = 8),
                     axis.text.y = ggplot2::element_text(size = 8,
                                                         hjust = 1)),
    if(!polar)
      ggplot2::geom_ribbon(aes(x = 1:365,
                               ymin = head(ybreaks,1),
                               ymax = tail(ybreaks,1)),
                           fill = NA,
                           color = "grey60", 
                           size = 0.5,
                           na.rm = TRUE),
    ggtitle(title)
  )
}
