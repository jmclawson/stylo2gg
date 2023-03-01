s2g_pca <- function(df_z, df_a, the_class, labeling,
                    shapes, legend, highlight,
                    legend_position, num_shapes, my_shapes,
                    title, caption, black, the_caption,
                    scaling, invert.x, invert.y,
                    top.loadings,
                    select.loadings, pc.x, pc.y, 
                    exception,
                    loadings_spacer, 
                    loadings_line_color, 
                    loadings_word_color,
                    loadings_upper,
                    plaintext){
  s2g_export$z <<- df_z
  # don't overwrite the classes that are defined already in the_class - 2022-02-08
  # the_classes <- rownames(df_z) %>%
  #   strsplit("_") %>%
  #   sapply(`[`,1)
  the_classes <- the_class
  
  # Here, begin to add the machinery for "exception"
  # I still need to test it a lot!
  # It occasionally seems to malfunction
  
  if (!missing(exception)) {
    the_exception <- the_classes %in% exception
    # the_class <- the_class[!the_exception]
  } else {
    the_exception <- rep(FALSE, length(the_classes))
  }
  
  # the_exception <- rep(FALSE, length(the_classes))
  
  df_pca <- prcomp(df_z[!the_exception,], scale. = scaling)
  
  if (!missing(exception)) {
    df_exception <-
      df_z[the_exception,] %>%
      as.matrix() %>%
      scale(df_pca$center, df_pca$scale)
    
    df_exception <- df_exception %*% df_pca$rotation
    
    s2g_export$exception <<- df_exception
    
    df_pca$x <- rbind(df_pca$x, df_exception)
    
    df_pca$x <- df_pca$x[rownames(df_z),]
  }
  
  s2g_export$pca <<- df_pca
  s2g_export$pca_details <<- summary(df_pca)
  pca_list <- df_pca
  df_pca_rotation <- df_pca$rotation
  
  pc_variance <- summary(df_pca)$importance[2,1:2]
  
  df_pca <- df_pca$x %>%
    as.data.frame()
  
  if (invert.x) {
    df_pca$PC1 <- df_pca$PC1 * -1
  }
  
  if (invert.y) {
    df_pca$PC2 <- df_pca$PC2 * -1
  }
  
  if (missing(the_class)) {
    df_pca$class <- df_pca %>%
      rownames() %>%
      strsplit("_") %>%
      sapply(`[`, 1)
  } else {
    df_pca$class <- the_class
  }
  
  df_pca$title <- df_pca %>%
    rownames() %>%
    strsplit("_") %>%
    sapply(`[`, 2)
  
  df_pca$shorttitle <- df_pca$title %>%
    gsub(pattern = "[a-z]",
         replacement = "",
         x = .)
  
  the_plot <- df_pca %>%
    ggplot(aes(PC1,
               PC2))
  
  if (missing(top.loadings)) {
    if (missing(select.loadings)){
      the_plot <- the_plot +
        geom_hline(yintercept = 0,
                   color = "gray") +
        geom_vline(xintercept = 0,
                   color = "gray")
    } else {
      the_plot <- s2g_loadings(the_plot,
                               pca_list,
                               top.loadings,
                               select.loadings, 
                               pc.x, 
                               pc.y,
                               invert.x,
                               invert.y,
                               loadings_spacer, 
                               loadings_line_color, 
                               loadings_word_color,
                               loadings_upper)
    }
  } else if (top.loadings > 0) {
    the_plot <- s2g_loadings(the_plot,
                             pca_list,
                             top.loadings,
                             select.loadings, 
                             pc.x, 
                             pc.y,
                             invert.x,
                             invert.y,
                             loadings_spacer, 
                             loadings_line_color, 
                             loadings_word_color,
                             loadings_upper)
  } else {
    message("no go")
    the_plot <- the_plot +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = 0, color = "gray")
  }
  
  if (!missing(labeling)) {
    if (is.numeric(labeling)) {
      if (labeling == 0) {
        labeling <- df_a$table.with.all.freqs %>%
          row.names() %>%
          as.character()
        
        shapes <- FALSE
        if (missing(legend)) {
          legend <- FALSE
        }
      } else {
        labeling <- df_a$table.with.all.freqs %>%
          row.names() %>%
          strsplit("_") %>%
          sapply(`[`,labeling)
      }
    }}
  
  if (!missing(labeling)){
    labeling.numeric <- suppressWarnings(all(!is.na(as.numeric(as.character(labeling)))))
  } else {
    labeling.numeric <- FALSE
  }
  
  # if (!missing(exception)) {
  #   labeling <- labeling[!the_exception]
  # }
  
  if (missing(labeling)) {
    if (missing(legend)) {
      legend <- TRUE
    }
    
    # alpha_values <- rep(1, length(unique(df_pca$class)))
    
    the_plot <- the_plot +
      geom_point(aes(shape = class,
                     color = class,
                     alpha = class,
                     size = class),
                 show.legend = legend) +
      scale_shape_manual(values = my_shapes) +
      scale_alpha_manual(values = rep(1, length(my_shapes))) +
      scale_size_manual(values = rep(3, length(my_shapes)))
    
    # the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)
    
  } else if (shapes) {
    if (missing(legend)) {
      legend <- TRUE
    }
    
    # alpha_values <- rep(1, length(unique(df_pca$class)))
    
    # library(ggrepel)
    the_plot <- the_plot +
      geom_point(aes(shape = class,
                     color = class,
                     alpha = class,
                     size = class),
                 show.legend = legend) +
      scale_alpha_manual(values = rep(1, length(my_shapes))) +
      scale_size_manual(values = rep(3, length(my_shapes)))
    
    
    # the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)
    
    the_plot <- the_plot +
      geom_text_repel(aes(label = labeling,
                          color = class),
                      show.legend = FALSE) +
      scale_shape_manual(values = my_shapes)
  } else {
    if (missing(legend)) {
      legend <- TRUE
    }
    
    # the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)
    
    if(!plaintext) {
      the_plot <- the_plot +
        geom_label(aes(label = labeling,
                       color = class,
                       fill = class,
                       group = class),
                   show.legend = legend)
    } else {
      the_plot <- the_plot +
        geom_text(aes(label = labeling,
                      color = class,
                      group = class),
                  show.legend = legend)
    }
    
    if (labeling.numeric) {
      the_plot <- the_plot +
        guides(color = guide_legend(override.aes = aes(label = "#")))
    }
  }
  # the_plot <- s2g_highlight(the_plot,
  #                           df_pca = df_pca,
  #                           highlight = highlight)
  
  if (!is.null(highlight)) {
    h <- highlight
    
    silly_guides <-
      rep(0, df_pca$class %>%
            unique() %>%
            length()
      )
    
    silly_guides[h] <- 1
    
    the_plot <- the_plot +
      ggalt::geom_encircle(data = df_pca[df_pca$class %in% unique(df_pca$class)[h],],
                           aes(color = class),
                           show.legend = FALSE) +
      geom_rect(data = df_pca[df_pca$class %in% unique(df_pca$class)[h],],
                aes(xmin = 0, xmax = 0,
                    ymin = 0, ymax = 0,
                    color = class),
                show.legend = legend,
                fill = NA) +
      geom_rect(data = df_pca[df_pca$class %in% unique(df_pca$class)[h],],
                aes(xmin = 0, xmax = 0,
                    ymin = 0, ymax = 0),
                color = "gray", fill = NA,
                show.legend = FALSE)
    
    if (labeling.numeric) {
      # message("Numeric labels")
      the_plot <- the_plot +
        guides(color = guide_legend(override.aes =
                                      list(label = "#",
                                           linetype = silly_guides)
        ))
    } else {
      the_plot <- the_plot +
        guides(color = guide_legend(override.aes =
                                      list(#aes(label = "#"),
                                        linetype = silly_guides)
        ))
    }
  }
  
  if (!is.null(black)) {
    the_colors <- gg_color(num_shapes)
    
    the_colors[black] <- "#000000"
      
    the_plot <- the_plot +
      scale_color_manual(values = the_colors)
  }
  
  y_label <- paste0("PC2 (",
                    round(pc_variance[2]*100,1),
                    "%)")
  
  if (!missing(exception)) {
    y_label <- paste0("PC2 (",
                      round(pc_variance[2]*100,1),
                      "%*)")
  }
  
  the_plot <- the_plot +
    theme_bw() +
    theme(legend.title = element_blank()) +
    labs(y = y_label)
  
  if (caption && !is.null(the_caption)) {
    x_label <- paste0("PC1 (",
                      round(pc_variance[1]*100,1),
                      "%)",
                      "\n",
                      the_caption)
    
    if (!missing(exception)) {
      x_label <- paste0("PC1 (",
                        round(pc_variance[1]*100,1),
                        "% except ",
                        paste(exception, collapse = ", "),
                        ")",
                        "\n",
                        the_caption)
    }
    
    the_plot <- the_plot +
      labs(x = x_label)
    
    the_caption <- NULL
  } else {
    x_label <- paste0("PC1 (",
                      round(pc_variance[1]*100,1),
                      "%)")
    
    if (!missing(exception)) {
      x_label <- paste0("PC1 (",
                        round(pc_variance[1]*100,1),
                        "% except ",
                        paste(exception, collapse = ", "),
                        ")")
    }
    
    the_plot <- the_plot +
      labs(x = x_label)
  }
  
  the_plot <- the_plot +
    theme(legend.position = legend_position)
  
  if (caption) {
    if (!is.null(the_caption)) {
      the_plot <- the_plot +
        labs(caption = the_caption)
    }
  }
  return(the_plot)
}
