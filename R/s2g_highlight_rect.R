s2g_highlight_rect <- function(the_plot = the_plot,
                               the_ggdend = the_ggdend,
                               highlight = highlight,
                               the_colors = the_colors,
                               highlight.nudge,
                               highlight.single,
                               highlight.box, legend) {
  
  if(!is.null(highlight)) {
    
    h <- highlight
    tc <- sort(unique(the_ggdend$labels$class))[h]
    
    the_coords <-
      the_ggdend$labels[the_ggdend$labels$class == tc,"x"]
    the_coords <- sort(the_coords)
    
    if (length(h) > 1) {
      message("Dendrograms can only highlight one class at a time. Use the geom_rect() function from ggplot2 to highlight manually. Alternatively, set the highlight.box argument to a range.")
    }
  }
  
  if (!is.null(highlight.box)) {
    the_coords <- sort(highlight.box)
  }
  
  start <- c(1, which(diff(the_coords) != 1 & diff(the_coords) != 0) + 1)
  end <- c(start - 1, length(the_coords))
  end <- end[end > 0]
  
  if (highlight.single) {
    start <- 1
    end <- length(the_coords)
  }
  
  if (length(start) > 1) {
    the_branch_max <- c()
    the_branch_min <- c()
    for (i in 1:length(start)) {
      # from1 <- the_ggdend$labels$y
      bottom <- the_coords[start[i]]
      top <- the_coords[end[i]]
      from1 <- the_ggdend$segments
      this_tab <- from1$y[from1$x >= bottom &
                            from1$x <= top] %>%
        table()
      
      the_branch_min[i] <-
        this_tab[this_tab == max(this_tab)] %>%
        names() %>%
        as.numeric() %>%
        max()
      
      the_branch_min[i] <-
        from1$y[round(from1$y,5) ==
                  round(the_branch_min[i],5)] %>%
        max(na.rm = TRUE)
      
      from2 <- the_ggdend$segments$y
      the_branch_max[i] <-
        from2[the_ggdend$segments$yend ==
                the_branch_min[i]] %>%
        max()
      
      if (top == bottom) {
        the_branch_max[i] <- the_branch_min[i]*1.1
      }
      
      if (!missing(highlight.nudge)) {
        h.n <- highlight.nudge
      } else {
        h.n <- 0
      }
      the_rect <-
        data.frame(xmin = the_coords[start[i]] - 0.5,
                   xmax = the_coords[end[i]] + 0.5,
                   ymin = -0.1 - h.n,
                   ymax = mean(c(the_branch_min[i],
                                 the_branch_max[i])))
      
      if (!is.null(highlight)) {
        the_rect$class <- tc
        silly_guides <-
          rep(0, the_ggdend$labels$class %>%
                unique() %>%
                length()
          )
        silly_guides[h] <- 2
        
        the_plot <- the_plot +
          geom_rect(data = the_rect,
                    aes(xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        color = class),
                    fill = "white", alpha = 0, linetype = 2,
                    show.legend = legend) +
          guides(
            color = guide_legend(
              override.aes = list(linetype = silly_guides)
            )
          )
      } else {#when using highlight.box
        the_plot <- the_plot +
          geom_rect(data = the_rect,
                    aes(xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax),
                    color = "gray50",
                    fill = "white", alpha = 0, linetype = 2,
                    show.legend = FALSE)
      }
    }
  } else {
    from1 <- the_ggdend$segments
    bottom <- the_coords[start]
    top <- the_coords[end]
    
    this_tab <- from1$y[from1$x >= bottom &
                          from1$x <= top] %>%
      table()
    the_branch_min <-
      # this_tab %>%
      this_tab[this_tab == 4] %>%
      names() %>%
      as.numeric() %>%
      max()
    
    the_branch_min <- from1$y[round(from1$y,5) ==
                                round(the_branch_min,5)]
    
    # }
    
    the_branch_max <-
      from1$y[from1$yend == the_branch_min] %>%
      max()
    
    if (the_coords[start] == the_coords[end]) {
      the_branch_max <- the_branch_min*1.1
    }
    
    label_widths <- the_ggdend$labels$labels %>%
      strwidth("inches")
    
    the_ggdend$labels$label_widths <- label_widths
    
    the_ggdend$labels$label_ymin <-
      the_ggdend$labels$y -
      (the_ggdend$labels$label_widths / 2)
    
    # label_ymin <- min(the_ggdend$labels$label_ymin)
    if (!missing(highlight.nudge)) {
      h.n <- highlight.nudge
    } else {
      h.n <- 0
    }
    
    label_ymin <- -0.1 - h.n
    
    the_rect <- data.frame(xmin = the_coords[start] - 0.5,
                           xmax = the_coords[end] + 0.5,
                           ymin = label_ymin,
                           ymax = mean(c(the_branch_min,
                                         the_branch_max)))
    
    if (!is.null(highlight)) {
      the_rect$class <- tc
      silly_guides <-
        rep(0, the_ggdend$labels$class %>%
              unique() %>%
              length()
        )
      silly_guides[h] <- 2
      
      the_plot <- the_plot +
        geom_rect(data = the_rect,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax,
                      color = class),
                  fill = "white", alpha = 0, linetype = 2,
                  show.legend = legend) +
        guides(
          color = guide_legend(
            override.aes = list(linetype = silly_guides)
          )
        )
    } else {#when using highlight.box
      the_plot <- the_plot +
        geom_rect(data = the_rect,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  color = "gray50",
                  fill = "white", alpha = 0, linetype = 2,
                  show.legend = FALSE)
    }
  }
  return(the_plot)
}
