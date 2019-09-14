theme_timeless <- function(base_size = 8,
                           base_font_family = "Rubik",
                           titles_font_family = "Libre Franklin",
                           grid_lines = FALSE,
                           axis_lines = TRUE,
                           axis_ticks = TRUE,
                           x_axis_text_align = 'center',
                           y_axis_text_align = 'center',
                           markdown_elements = 'none') {

  # Set appropriate ggplot2 element function to use depending on whether an element is markdown
    if ("ggtext" %in% .packages(all.available = TRUE)) {
      if (is.null(markdown_elements) || markdown_elements == 'none') {
        title_text_element <- ggplot2::element_text
        subtitle_text_element <- ggplot2::element_text
        caption_text_element <- ggplot2::element_text
        axis_title_text_element <- ggplot2::element_text
        axis_label_text_element <- ggplot2::element_text
        strip_text_element <- ggplot2::element_text
      } else {
        if ('title' %in% markdown_elements) {
          title_text_element <- ggtext::element_markdown
        } else {title_text_element <- ggplot2::element_text}
        if ('subtitle' %in% markdown_elements) {
          subtitle_text_element <- ggtext::element_markdown
        } else {subtitle_text_element <- ggplot2::element_text}
        if ('caption' %in% markdown_elements) {
          caption_text_element <- ggtext::element_markdown
        } else {caption_text_element <- ggplot2::element_text}
        if ('axis title' %in% markdown_elements) {
          axis_title_text_element <- ggtext::element_markdown
        } else {axis_title_text_element <- ggplot2::element_text}
        if ('axis label' %in% markdown_elements) {
          axis_label_text_element <- ggtext::element_markdown
        } else {axis_label_text_element <- ggplot2::element_text}
        if ('strip' %in% markdown_elements) {
          strip_text_element <- ggtext::element_markdown
        } else {strip_text_element <- ggplot2::element_text}
      }
    } else {
      stop("The `ggtext` package must be installed in order to use markdown elements.")
    }


  # Error check and message for numeric arguments
    if (!is.numeric(base_size)) {
      stop("The `base_size` argument must be specified as a single number.")
    }


  # Check if the specified font family can be loaded, potentially by relying on the `extrafont` R package
    available_fonts <- names(windowsFonts())

    if (!base_font_family %in% available_fonts) {
      if ('extrafont' %in% .packages(all.available = TRUE)) {

        if (base_font_family %in% extrafont::fonts()) {

          library('extrafont')
          extrafont::loadfonts(device = 'win', quiet = TRUE)

          warning(paste0('Fonts have been loaded via the `extrafont` package in order to use ', base_font_family))

        } else {

          stop(paste("The family", base_font_family, "is not available.",
                     "Make sure the font is installed on this machine. If it's not one of R's default fonts, try registering and loading it with the `extrafont` package."))
        }
      }
    }

    # Handling of axis lines
      if (!is.null(axis_lines) && !(is.logical(axis_lines) || tolower(axis_lines) %in% c('x', 'y', 'xy', 'yx'))) {
        stop("If `axis_lines` is specified, it must be either `TRUE`, `FALSE`, 'x', 'y', 'xy', or 'yx'.")
      }

      if (is.null(axis_lines) || (is.logical(axis_lines) && !axis_lines)) {

        use_x_axis_lines <- FALSE
        use_y_axis_lines <- FALSE

      } else {

        axis_lines_element <- ggplot2::element_line(colour = 'lightgrey')

        if (is.character(axis_lines)) {
          use_x_axis_lines <- grepl(pattern = 'x', x = axis_lines, ignore.case = TRUE)
          use_y_axis_lines <- grepl(pattern = 'y', x = axis_lines, ignore.case = TRUE)
        }

        if (is.logical(axis_lines) && axis_lines) {
          use_x_axis_lines <- TRUE
          use_y_axis_lines <- TRUE
        }
      }

      if (use_x_axis_lines) {
        axis_lines_x <- axis_lines_element
      } else {
        axis_lines_x <- element_blank()
      }

      if (use_y_axis_lines) {
        axis_lines_y <- axis_lines_element
      } else {
        axis_lines_y <- element_blank()
      }

    # Handling of axis ticks
      if (!is.null(axis_ticks) && !(is.logical(axis_ticks) || tolower(axis_ticks) %in% c('x', 'y', 'xy', 'yx'))) {
        stop("If `axis_ticks` is specified, it must be either `TRUE`, `FALSE`, 'x', 'y', 'xy', or 'yx'.")
      }

      if (is.null(axis_ticks) || (is.logical(axis_ticks) && !axis_ticks)) {

        use_x_axis_ticks <- FALSE
        use_y_axis_ticks <- FALSE

      } else {

        axis_ticks_element <- ggplot2::element_line(colour = 'lightgrey')

        if (is.character(axis_ticks)) {
          use_x_axis_ticks <- grepl(pattern = 'x', x = axis_ticks, ignore.case = TRUE)
          use_y_axis_ticks <- grepl(pattern = 'y', x = axis_ticks, ignore.case = TRUE)
        }

        if (is.logical(axis_ticks) && axis_ticks) {
          use_x_axis_ticks <- TRUE
          use_y_axis_ticks <- TRUE
        }
      }

      if (use_x_axis_ticks) {
        axis_ticks_x <- axis_ticks_element
      } else {
        axis_ticks_x <- element_blank()
      }

      if (use_y_axis_ticks) {
        axis_ticks_y <- axis_ticks_element
      } else {
        axis_ticks_y <- element_blank()
      }

  # Handling of grid lines
    if (!is.null(grid_lines) && !(is.logical(grid_lines) || tolower(grid_lines) %in% c('x', 'y', 'xy', 'yx'))) {
      stop("If `gridlines` is specified, it must be either `TRUE`, `FALSE`, 'x', 'y', 'xy', or 'yx'.")
    }

    if (is.null(grid_lines) || (is.logical(grid_lines) && !grid_lines)) {

      use_x_gridlines <- FALSE
      use_y_gridlines <- FALSE

    } else {

        major_gridlines <- element_line(colour = 'lightgrey')
        minor_gridlines <- element_blank()

        if (is.character(grid_lines)) {
          use_x_gridlines <- grepl(pattern = 'x', x = grid_lines, ignore.case = TRUE)
          use_y_gridlines <- grepl(pattern = 'y', x = grid_lines, ignore.case = TRUE)
        }

        if (is.logical(grid_lines) && grid_lines) {
          use_x_gridlines <- TRUE
          use_y_gridlines <- TRUE
        }
    }

    if (use_x_gridlines) {
      major_gridlines_x <- major_gridlines
      minor_gridlines_x <- minor_gridlines
    } else {
      major_gridlines_x <- element_blank()
      minor_gridlines_x <- element_blank()
    }

    if (use_y_gridlines) {
      major_gridlines_y <- major_gridlines
      minor_gridlines_y <- minor_gridlines
    } else {
      major_gridlines_y <- element_blank()
      minor_gridlines_y <- element_blank()
    }

  # Handling of text alignment for axes

    if (!is.null(x_axis_text_align)) {

      if (!x_axis_text_align %in% c("left", 'center', 'right')) {
        stop("`x_axis_text_align` must be one of: 'left', 'center', or 'right'")
      }
      x_axis_text_hjust <- switch (x_axis_text_align,
                                   'left' = 0,
                                   'right' = 1,
                                   'center' = 0.5
      )
    } else {
      x_axis_text_hjust <- 0.5
    }

    if (!is.null(y_axis_text_align)) {

      if (!y_axis_text_align %in% c("left", 'center', 'right')) {
        stop("`y_axis_text_align` must be one of: 'left', 'center', or 'right'")
      }
      y_axis_text_hjust <- switch (y_axis_text_align,
                                   'left' = 0,
                                   'right' = 1,
                                   'center' = 0.5
      )
    } else {
      y_axis_text_hjust <- 0.5
    }

  # Uniform line size based on the specified value of `base_size`
    scaled_line_size <- (base_size/10)*(grid::get.gpar('lineheight')$lineheight)

  # Combine all the specified custom elements into a list
  # which will be built into a theme
    custom_theme_elements <- structure(list(# Set text preference for plot titles, subtitles, and captions
                                            text = ggplot2::element_text(family = base_font_family,
                                                                         size = base_size, face = "plain", color = "#545454",
                                                                         hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1,
                                                                         margin = grid::unit(c(0, 0, 0, 0), 'pt'),
                                                                         debug = FALSE, inherit.blank = TRUE),
                                            plot.title = title_text_element(size = (1.4)*base_size, face = 'bold',
                                                                               color = "#545454",
                                                                               hjust = 0, vjust = 1, family = titles_font_family,
                                                                               margin = ggplot2::margin(b = base_size,
                                                                                                        unit = "pt")),
                                            plot.subtitle = subtitle_text_element(size = base_size,
                                                                                  face = "italic", family = titles_font_family,
                                                                                  hjust = 0, vjust = 2,
                                                                                  color= "#545454",
                                                                                  margin = ggplot2::margin(b = base_size,
                                                                                                           unit = "pt")),
                                            # Position title/subtitle relative to entire plot (not to main panel)
                                            plot.title.position = 'plot', plot.subtitle.position = 'plot',
                                            # Plot background and grids
                                            panel.background = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
                                            panel.grid.major.x = major_gridlines_x,
                                            panel.grid.minor.x = minor_gridlines_x,
                                            panel.grid.major.y = major_gridlines_y,
                                            panel.grid.minor.y = minor_gridlines_y,
                                            # Plot legend
                                            legend.position = "top",
                                            legend.justification = c(0, 0),
                                            legend.text = ggplot2::element_text(size = (0.8)*base_size, colour = "#545454",
                                                                                family = base_font_family),
                                            legend.background = ggplot2::element_blank(),
                                            legend.key = ggplot2::element_rect(size = 0.5, colour = NA),
                                            # Axis lines and ticks
                                            axis.line.x = axis_lines_x,
                                            axis.line.y = axis_lines_y,
                                            axis.ticks.x = axis_ticks_x,
                                            axis.ticks.y = axis_ticks_y,
                                            # Axis text
                                            axis.text.x = axis_label_text_element(size = base_size, family = base_font_family,
                                                                                colour = "#545454", vjust = 0.5, hjust = x_axis_text_hjust),
                                            axis.text.y = axis_label_text_element(size = base_size, colour = "#545454",
                                                                                family = base_font_family, hjust = y_axis_text_hjust),
                                            axis.title.x = axis_title_text_element(size = base_size*(1.1), colour = "#545454",
                                                                                 face = "plain", family = base_font_family, angle = 0,
                                                                                 margin = ggplot2::margin(t = (.8)*base_size, b = (.8)*base_size,
                                                                                                          l = 0, r = 0,
                                                                                                          unit = "pt")),
                                            axis.title.y = axis_title_text_element(size = base_size*(1.1), colour = "#545454",
                                                                                 face = "plain", family = base_font_family, angle = 0,
                                                                                 vjust = 0.5,
                                                                                 margin = ggplot2::margin(t = (.8)*base_size, b = (.8)*base_size,
                                                                                                          l = 0, r = (.8)*base_size,
                                                                                                          unit = "pt")),
                                            # Strips used in facet labels
                                            strip.placement = "outside",
                                            strip.text.x = strip_text_element(size = base_size*(1.1), colour = "#545454",
                                                                                 face = "bold", family = base_font_family,
                                                                                 margin = ggplot2::margin(t = 0, b = (.8)*base_size,
                                                                                                          l = 0, r = 0,
                                                                                                          unit = "pt")),
                                            strip.text.y = strip_text_element(size = base_size*(1.1), colour = "#545454",
                                                                                 face = "bold", family = base_font_family,
                                                                                 margin = ggplot2::margin(t = 0, b = (.8)*base_size,
                                                                                                          l = 0, r = 0,
                                                                                                          unit = "pt"),
                                                                                 angle = 180),
                                            strip.background = ggplot2::element_blank(),
                                            # Spacing between panels in faceted plots
                                            panel.spacing = grid::unit(scaled_line_size, "lines"),
                                            # Margins surrounding entire plot
                                            plot.margin = grid::unit(c('t' = 0.5, 'r' = 1.5,
                                                                       'b' = 0.5, 'l' = 0.5)*scaled_line_size,
                                                                     units = "lines")),
                                       class = "theme", complete = TRUE)

  # Update the minimalistic ggplot2 theme ("theme_minimal")
  # with the list of custom theme elements
    minimal_theme <- ggplot2::theme_minimal(base_family = base_font_family,
                                            base_size = base_size)

    result_theme <-  ggplot2::`%+replace%`(minimal_theme,
                                           custom_theme_elements)

  # Return the result
    return(result_theme)
}
