#' A [ggplot2] theme for Isabella Benabaye's personal use
#'
#' \code{theme_Tim} is a [ggplot2] theme in Tim Monko's style - courtesy of inspiration from Isabella Benabaye's Ib_theme and Ib package. The theme was specifically designed to be used for plots in manuscripts. Defaulting to a size suitable for resizing without having to worry about relative sizing and font sizing.
#'
#' By default (`plots_pane = FALSE`), the theme adjusts the text sizes for
#' printing.`plots_pane = TRUE` is meant to be used when viewing plots in the
#' plots pane and text sizes are not adjusted.  There is also an option (`md =
#' TRUE`) to use markdown theme elements from `ggtext` instead of
#' `element_text()`.
#'
#' @md
#' @param title_family title elements font family
#' @param text_family text elements font family
#' @param base_size base font size
#' @param text_color color of the text & title elements
#' @param bg_color plot background color
#' @param line_color color of line elements
#' @param plot_margin plot margin specifications
#' @param plots_pane indicate whether plots will be viewed in plots pane or not
#' @param md indicate whether to use markdown elements for text
#' @export
 
theme_Tim <- function(title_family = "Source Sans Pro",
                      text_family = "Source Sans Pro",
                      base_size = 7, base_size_pane = 13, text_color = "black",
                      bg_color = "#ffffff", line_color = "black",
                      plot_margin = margin(5,5,5,5),
                      legend_margin = margin(-15,-10,-15,-15),
                      plots_pane = FALSE,
                      md = FALSE) {
  
  if (plots_pane == FALSE & md == FALSE) {
    ggplot2::theme_classic() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = element_text(family = title_family,
                             color = text_color),
        line = element_line(color = line_color),
        
        plot.title = element_text(face = "bold",
                                  size = base_size * 1.8,
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = base_size * 1.5,
                                     lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.title = element_text(size = base_size * 1.32,
                                  hjust = 0.5),
        axis.line = element_line(color = line_color),
        
        legend.title = element_text(size = base_size * 1.32),
        legend.text = element_text(size = base_size * 1.1),
        legend.box.margin = legend_margin,
        
        # panel.grid.minor = element_line(color = 'gray95'),
        # panel.grid.major = element_line(color = 'gray95'),
        
        strip.text = element_text(size = base_size * 1.32)
      ) 
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_classic() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = ggtext::element_markdown(family = title_family,
                                         color = text_color),
        line = element_line(color = line_color),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.7,
                                                 lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              hjust = 1),
        axis.line = element_line(color = line_color),
        
        legend.title = ggtext::element_markdown(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1),
        
        panel.grid.minor = element_line(color = 'gray95'),
        panel.grid.major = element_line(color = 'gray95')
      )
  } else if (plots_pane == TRUE & md == TRUE) {
    ggplot2::theme_classic(base_size = base_size_pane) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.title = ggtext::element_markdown(hjust = 1),
        axis.line = element_line(color = line_color),
        
        panel.grid.minor = element_line(color = 'gray95'),
        panel.grid.major = element_line(color = 'gray95')
      )
  } else {
    ggplot2::theme_classic(base_size = base_size_pane) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.title = element_text(hjust = 1),
        axis.line = element_line(color = line_color),
        
        panel.grid.minor = element_line(color = 'gray95'),
        panel.grid.major = element_line(color = 'gray95')
      )
  }
}
