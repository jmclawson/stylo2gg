s2g_hc <- function(df_z, df, df_a, the_distance,
                   highlight, title, caption, the_caption,
                   labeling, classing, linkage, the_class,
                   highlight.nudge, num_shapes, my_shapes,
                   shapes, legend, horiz, axis.labels,
                   show.zero, highlight.box, count.labels,
                   black, distance.measure, highlight.single
){
  if (missing(labeling)) {
    labeling <- 0
  } else {
    if (is.numeric(labeling)) {
      if (labeling == 0) {
        the_labels <- df_a$table.with.all.freqs %>%
          row.names() %>%
          as.character()
      } else {
        the_labels <- df_a$table.with.all.freqs %>%
          row.names() %>%
          strsplit("_") %>%
          sapply(`[`,labeling)
      }
      if (labeling == 1) {
        if (missing(legend)) {
          legend <- FALSE
        }
      }
      the_labels <- labeling
    } else {
      the_labels <- labeling
    }
  }
  
  if (!distance.measure == "euclidean") {
    df <- df_z
  }
  
  if (labeling != 0 && missing(legend)) {
    legend <- TRUE
  }
  
  the_colors <- gg_color(length(unique(the_class)))
  
  if (!is.null(black)) {
    the_colors[black] <- "#000000"
  }
  
  if (!missing(labeling)) {
    # rownames(df) <- labeling
  }
  
  rownames(df) <- paste0(" ", rownames(df))
  
  the_dend <- df %>%
    as.matrix()
  
  if (distance.measure == "argamon") {
    the_dend <- the_dend %>%
      dist.argamon()
  } else if (distance.measure == "eder") {
    the_dend <- the_dend %>%
      dist.eder()
  } else if (distance.measure == "cosine") {
    the_dend <- the_dend %>%
      dist.cosine()
  } else if (distance.measure == "simple") {
    the_dend <- the_dend %>%
      dist.simple()
  } else if (distance.measure == "delta") {# default to delta
    the_dend <- the_dend %>%
      dist.delta()
  } else {
    the_dend <- the_dend %>%
      dist(method = distance.measure)
  }
  
  the_dend <- the_dend %>%
    as.dist() %>%
    hclust(method = linkage) %>%
    as.dendrogram() %>%
    set("branches_lwd", 0.7) %>%
    # set("branches_k_color", k = 3) %>%
    set("labels_cex", 0.7) %>%
    set("hang_leaves", 0)
  
  the_shapes <- the_class %>%
    as.factor() %>%
    as.numeric() %>%
    .[order.dendrogram(the_dend)]
  
  # the_newshape <- my_shapes
  the_newshape <- rep(c(1, 3:11), length.out = num_shapes)
  
  labels_colors(the_dend) <- the_class %>%
    as.factor() %>%
    .[order.dendrogram(the_dend)] %>%
    the_colors[.]
  
  if (shapes) {
    the_dend <- the_dend %>%
      set("leaves_pch", my_shapes[the_shapes]) %>%
      set("leaves_col", labels_colors(the_dend)) %>%
      set("leaves_cex", 2)
    
    if (missing(legend)) {
      legend <- TRUE
    }
  }
  
  if (missing(legend)) {
    legend <- FALSE
  }
  
  the_ggdend <- the_dend %>%
    as.ggdend()
  
  # the_ggdend$labels$class <- the_ggdend$labels$label %>%
  #   as.character() %>%
  #   strsplit("_") %>%
  #   sapply(`[`,1)
  
  match_df <- df_a$table.with.all.freqs
  
  the_ggdend$labels <-
    the_ggdend$labels[match(rownames(match_df),
                            gsub(" ","",as.character(the_ggdend$labels$label))),]
  
  the_ggdend$labels$class <- the_class
  
  if (!missing(labeling)) {
    if (is.numeric(labeling)) {
      if (labeling == 0) {
        the_ggdend$labels$labels <- the_ggdend$labels$label %>%
          as.character()
      } else {
        the_ggdend$labels$labels <- the_ggdend$labels$label %>%
          as.character() %>%
          strsplit("_") %>%
          sapply(`[`,labeling)
      }
    } else {
      the_ggdend$labels$labels <- labeling
    }
  }
  
  the_gplot <- ggplot() +
    geom_segment(data = the_ggdend$segments,
                 aes(x = x, y = y, xend = xend, yend = yend))
  
  if (!horiz) {
    the_nudge <- -0.09
    point_shift <- -0.05
    the_angle <- 90
    the_hjust <- 1
  } else {
    the_nudge <- 0.08
    point_shift <- -0.05
    the_angle <- 0
    the_hjust <- 0
  }
  
  if (shapes) {
    text_legend <- FALSE
  } else {
    text_legend <- legend
    the_nudge <- 0
  }
  
  if(count.labels){
    the_gplot <- the_gplot +
      geom_text(data = the_ggdend$labels,
                aes(x = x,
                    y = y,
                    label = paste0(x, ". ", labels),
                    color = class),
                angle = the_angle, hjust = the_hjust, nudge_y = the_nudge,
                show.legend = text_legend)
  } else {
    the_gplot <- the_gplot +
      geom_text(data = the_ggdend$labels,
                aes(x = x, y = y, label = labels, color = class),
                angle = the_angle, hjust = the_hjust, nudge_y = the_nudge,
                show.legend = text_legend)
  }
  
  if (shapes) {
    the_gplot <- the_gplot +
      geom_point(data = the_ggdend$labels,
                 aes(x = x,
                     y = y + point_shift,
                     shape = class,
                     color = class)) +
      scale_shape_manual(values = my_shapes)
  }
  
  if (horiz && !axis.labels) {
    the_gplot <- the_gplot +
      coord_flip() +
      scale_y_reverse(breaks = function(n) seq(round_any(min(n),0.5, ceiling),
                                               round_any(max(n),0.5, ceiling),
                                               0.5))
  }
  
  the_plot <- the_gplot +
    theme_dendro() +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
  
  if (axis.labels) {
    
    # the_plot <- the_plot +
    #   theme_dendro() +
    #   theme(legend.key = element_blank()) +
    #   theme(legend.title = element_blank())
    
    # the_plot <- the_dend %>% ggplot(horiz = horiz)
    
    if (!horiz) {
      top_limit <- the_ggdend$segments$y %>% max()
      the_plot <- the_plot +
        labs(y = paste0(the_distance,"\n")) +
        theme(axis.title.y = element_text(color = "black", angle = 90),
              axis.line.y = element_line(color = "black", size = 0.5),
              axis.ticks.y = element_line(color = "black", size = 0.5),
              axis.text.y = element_text(colour = "black")) +
        scale_y_continuous(breaks = function(n) seq(round_any(min(n),0.5, ceiling),
                                                    round_any(max(n),0.5, ceiling),
                                                    0.5))
      # expand_limits(y = c(top_limit,0))
      
      if ("lemon" %in% rownames(installed.packages())) {
        # library(lemon)
        the_plot <- the_plot +
          coord_capped_cart(left = "both")
      }
    } else if (horiz) {
      if (caption && !is.null(the_caption)) {
        the_distance <- paste0(the_distance,
                               "\n",
                               the_caption)
        the_caption <- NULL
      }
      
      top_limit <- the_ggdend$segments$y %>% max()
      the_plot <- the_plot +
        labs(y = the_distance) +
        theme(axis.title.x = element_text(color = "black"),
              axis.line.x = element_line(color = "black", size = 0.5),
              axis.ticks.x = element_line(color = "black", size = 0.5),
              axis.text.x = element_text(colour = "black"))
      
      if ("lemon" %in% rownames(installed.packages())) {
        # library(lemon)
        the_plot <- the_plot +
          coord_capped_flip(bottom="both") +
          scale_y_reverse(breaks = seq(-1,
                                       round_any(top_limit,0.5, ceiling),
                                       0.5))
      } else {
        the_gplot <- the_gplot +
          coord_flip() +
          scale_y_reverse(breaks = function(n) seq(0,
                                                   round_any(max(n),0.5, ceiling),
                                                   0.5))
      }
    }
    if (show.zero) {
      the_plot <- the_plot +
        expand_limits(y = 0)
    }
    
  } else {
    # the_plot <- the_dend %>% ggplot(horiz = horiz)
  }
  
  if (!is.null(black)) {
    the_colors <- gg_color(num_shapes)
    
    the_colors[black] <- "#000000"
      
    the_plot <- the_plot +
      scale_color_manual(values = the_colors)
  }
  
  the_ggdend$segments$kind[the_ggdend$segments$x == the_ggdend$segments$xend] <- "horizontal"
  the_ggdend$segments$kind[the_ggdend$segments$y==the_ggdend$segments$yend] <- "vertical"
  
  if(!is.null(highlight) |!is.null(highlight.box)) {
    the_plot <- s2g_highlight_rect(the_plot = the_plot,
                                   the_ggdend = the_ggdend,
                                   highlight = highlight,
                                   the_colors = the_colors,
                                   highlight.nudge,
                                   highlight.single,
                                   highlight.box, legend)
  }
  
  if (caption) {
    if (!is.null(the_caption)) {
      the_plot <- the_plot +
        labs(caption = the_caption)
    }
  }
  
  return(the_plot)
}
