#' Visualize \code{stylo} data with \code{ggplot2}.
#'
#' @param df An object saved from running \strong{\code{stylo}}
#' on a corpus
#' @param viz A choice of visualizations, either \code{"pca"}
#' for principal components analysis or \code{"hc"} for
#' hierarchical clustering; alternatively, using
#' \code{"PCR"}, \code{"PCV"}, or \code{"CA"}---all
#' inherited from \code{stylo}---will reset a number of
#' defaults.
#' @param num.features The number of features to be used for an
#' analysis. By default, \code{stylo}'s settings are used, but
#' it is easy here limit the number to a smaller set, ordered
#' by frequency
#' @param labeling Defines how to label items: if setting a
#' character vector, define one string for each item in
#' \strong{df}; if setting a numeric vector (e.g, \code{1} or
#' \strong{2}), set it to the desired element (identified via
#' \code{stylo}'s use of underscores in filenames).
#' @param classing The class or category for each item in
#' \strong{df}; if not set, it defaults to the first element
#' before an underscore in the filename of items in the
#' original corpus. \code{Stylo2gg} uses classing to
#' distinguish items by color and shape.
#' @param scaling Toggle the option to scale features before
#' running a principal components analysis. Defaults to
#' \code{FALSE}, except for \strong{\code{viz = "PCR"}}; for
#' all other principal components settings, the default is
#' first to normalize features by z scores, which makes
#' \strong{\code{scaling}} less useful.
#' @param linkage The linkage to be used for cluster analysis.
#' Defaults to \code{"ward.D"}, but \code{"complete"} might
#' also be a reasonable setting. Options include all those
#' built into R.
#' @param distance.measure The formula used for distance in
#' hierarchical clustering. Defaults to \code{"delta"} to use
#' Burrows's formula of Manhattan distance from normalized
#' z-scores, but it might also be reasonable here to call
#' \code{"euclidean"} or some other setting, imported from
#' stylo.
#' @param horiz Set the rotation of the dendrogram in a
#' hierarchical cluster analysis; defaults to \code{TRUE}
#' @param shapes Defaults to \code{FALSE} unless
#' \strong{\code{labeling}} is defined. Change to toggle shapes
#' on the visualization instead of (or in addition to) text
#' labels. This is useful for printing in black and white, but
#' it's also helpful to distinguish among similar colors.
#' @param invert.x Defaults to \code{FALSE}. Change to invert
#' the horizontal orientation in a principal components
#' analysis in order to approximate some ideal visualization.
#' (I don't think this actually changes any understanding of
#' the data.)
#' @param invert.y Defaults to \code{FALSE}. Change to invert
#' the vertical orientation in a principal components analysis
#' in order to approximate some ideal visualization. (I don't
#' think this actually changes any understanding of the data.)
#' @param caption Defaults to \code{FALSE}, except with
#' certain \strong{\code{viz}} settings. Change to toggle
#' metadata at the bottom of a visualization.
#' @param highlight Highlight a category (defined by its
#' number) by drawing around its elements on the visualization.
#' In a principal components analysis, multiple circular
#' highlights are available to contrast sets; on a dendrogram,
#' only one category can be highlighted with a box.
#' @param highlight.nudge On a highlighted dendrogram,
#' optionally define some extra space when a box overlaps the
#' edge of a label.
#' @param black Cast the color of one category (defined by its
#' number) as black. This setting is ideal to contrast a group
#' for printing in black and white.
#' @param title The title that will go on the top of a chart.
#' This value is inherited from \code{stylo} where possible. To
#' remove a title, set it to \code{NULL} or to an empty
#' set \strong{""}.
#' @param axis.labels Defaults to \code{FALSE} except when
#' \strong{\code{viz = "CA"}}. Change to \code{TRUE} to show
#' a distance axis for the dendrogram in a cluster analysis.
#' @param legend Show or hide the legend with \code{TRUE} or \code{FALSE}.
#'
#' @details
#' Because \code{stylo2gg} builds on \code{ggplot2}, almost all
#' commands available to that package should work here as well,
#' using the plus-sign syntax documented by that package.
#'
#' @examples
#' my_data <- stylo()
#' my_data %>% stylo2gg()
#'
#' # Move the legend
#' my_data %>% stylo2gg() +
#'   theme(legend.position = "bottom")
#'
#' @import dendextend ggplot2 dplyr ggrepel lemon
#' @export stylo2gg

stylo2gg <- function(df, viz, num.features,
                     title = NULL, caption = FALSE,
                     legend, black = NULL, highlight = NULL,
                     labeling, classing, shapes = FALSE,
                     invert.x = FALSE, invert.y = FALSE,
                     scaling = FALSE, distance.measure, linkage,
                     horiz = TRUE, axis.labels = FALSE,
                     highlight.nudge) {
  library(dendextend)
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  if ("lemon" %in% rownames(installed.packages())) library(lemon)
  df_a <- df
  if (missing(num.features)) {
    num.features <- length(df$features.actually.used)
  }

  if ("call" %in% names(df)) {
    my_call <- df$call
    my_call[1] <- substitute(data.frame())
    my_call <- eval(my_call)
  } else {
    my_call <- c()
  }

  if (missing(viz)) {
      if ("analysis.type" %in% names(my_call)) {
        viz <- my_call$analysis.type
      } else {
        viz <- "CA"
      }
  }

  legend_position <- "right"

  if(viz == "PCR" | viz == "PCV"){
      if(missing(caption)){
        caption <- TRUE
      }
      legend_position <- "top"
  } else if (viz == "CA"){
    if(missing(caption)){
      caption <- TRUE
    }
  }

    if (missing(linkage)) {
      if ("linkage" %in% names(my_call)) {
        linkage <- my_call$linkage
      }
    }

    if (missing(distance.measure)) {
      if ("distance.measure" %in% names(my_call)) {
        distance.measure <- my_call$distance.measure
      }
    }

    if (is.null(title)) {
      if ("custom.graph.title" %in% names(my_call)) {
        title <- my_call$custom.graph.title
      }
    }

    the_caption <- NULL
    the_viz <- NULL

    if (viz == "PCV") {
      the_viz <- "Covariance Matrix"
    } else if (viz == "PCR") {
      the_viz <- "Correlation Matrix"
    } else if (viz == "CA") {
      if (missing(axis.labels)) {
        axis.labels <- TRUE
      }
      if (missing(distance.measure)) {
        the_dist <- as.character(stylo.default.settings()$distance.measure)
      } else {
        the_dist <- as.character(distance.measure)
      }

      if (the_dist == "delta") {
        the_dist <- "Classic Delta"
      }
      the_dist <- paste0(toupper(substr(the_dist, 1, 1)),
                        substr(the_dist, 2, nchar(the_dist)))

      if (missing(linkage)) {
        the_linkage <- stylo.default.settings()$linkage
      } else {
        the_linkage <- linkage
      }

      the_viz <- paste0(the_dist, " distance (",
                       the_linkage, " linkage)")
    }

    if ("analyzed.features" %in% names(my_call)) {
      if (my_call$analyzed.features == "c") {
        the_features <- "MFC"
      } else if (my_call$analyzed.features == "w") {
          the_features <-  "MFW"
      }
    } else {
      the_features <-  "features"
    }

    the_features <- paste(num.features, the_features)

    if (is.null(the_caption)) {
      the_caption <- the_features
    } else {
      the_caption <- paste(the_caption, the_features, sep = " | ")
    }

    if ("ngram.size" %in% names(my_call)) {
      if (my_call$ngram.size > 1) {
        the_ngrams <- paste0(my_call$ngram.size, "-grams")
        if (is.null(the_caption)) {
          the_caption <- the_ngrams
        } else {
          the_caption <- paste(the_caption, the_ngrams, sep = " | ")
        }
      }
    }

    if ("culling.max" %in% names(my_call)) {
      the_culling <- paste0("Culled @ ",
                            my_call$culling.max,
                            "%")
      if (is.null(the_caption)) {
        the_caption <- the_culling
      } else {
        the_caption <- paste(the_caption, the_culling, sep = " | ")
      }
    }

    if (axis.labels) {
      if (viz == "CA") {
        the_distance <- the_viz
        the_viz <- NULL
      }
    }

    if (!is.null(the_viz)) {
      the_caption <- paste(the_caption, the_viz, sep = "\n")
    }

  if (missing(viz)) {
    viz <- "pca"
  }

  if (missing(linkage)) {
    linkage <- "ward.D"
  }

  if (missing(distance.measure)) {
    distance.measure <- "delta"
  }

  if (viz == "PCR") {
    viz <- "pca"
    scaling <- TRUE
  }

  df <- df$table.with.all.freqs %>%
    .[,df$features.actually.used[1:num.features]] %>%
    as.data.frame()

  df_means <- colMeans(df)
  df_sd <- apply(df, 2, sd)

  ## create table of z scores
  corpus_zscores <- list()
  for (row_i in rownames(df)) {
    thisrow <- (df[row_i, ] - df_means) / df_sd
    corpus_zscores[[row_i]] <- thisrow
  }

  df_z <- data.frame(matrix(unlist(corpus_zscores),
                            nrow = length(corpus_zscores),
                            byrow = T))

  rownames(df_z) <- names(corpus_zscores)
  colnames(df_z) <- colnames(corpus_zscores[[1]])

  if (missing(classing)) {
    the_class <- df %>%
      rownames() %>%
      strsplit("_") %>%
      sapply(`[`, 1)
  } else {
    the_class <- classing
  }

  num_shapes <- the_class %>%
    unique() %>%
    length()

  my_shapes <- rep(c(1, 3:11), length.out = num_shapes)
  if(!is.null(black)) {
    if (length(black) == 1){
      my_shapes[black] <- 19
    }
  }

  if (viz == "PCV") {
    viz <- "pca"
    df_z <- df
  }

  if (viz == "pca" || viz == "PCA" || viz == "PCR") {
    the_plot <- s2g_pca(df_z, df_a, the_class, labeling,
                        shapes, legend, highlight,
                        legend_position, num_shapes, my_shapes,
                        title, caption, black, the_caption,
                        scaling, invert.x, invert.y)
  } else if (viz == "hc" || viz == "ca" || viz == "CA" || viz == "HC") {
    the_plot <- s2g_hc(df_z, df, df_a, the_distance,
                       highlight, title, caption, the_caption,
                       labeling, classing, linkage, the_class,
                       highlight.nudge, num_shapes, my_shapes,
                       shapes, legend, horiz, axis.labels,
                       black, distance.measure)
  }
  suppressWarnings(print(the_plot))
  }

s2g_pca <- function(df_z, df_a, the_class, labeling,
                    shapes, legend, highlight,
                    legend_position, num_shapes, my_shapes,
                    title, caption, black, the_caption,
                    scaling, invert.x, invert.y){
  df_pca <- prcomp(df_z, scale. = scaling)

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
               PC2)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray")

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

  if (missing(labeling)) {
    if (missing(legend)) {
      legend <- TRUE
    }

    the_plot <- the_plot +
      geom_point(aes(shape = class,
                     color = class),
                 show.legend = legend) +
      scale_shape_manual(values = my_shapes)

    the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)

  } else if (shapes) {
    if (missing(legend)) {
      legend <- TRUE
    }

    # library(ggrepel)
    the_plot <- the_plot +
      geom_point(aes(shape = class,
                     color = class),
                 show.legend = legend)

    the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)

    the_plot <- the_plot +
      geom_text_repel(aes(label = labeling,
                          color = class),
                      show.legend = FALSE) +
      scale_shape_manual(values = my_shapes)
  } else {
    if (missing(legend)) {
      legend <- TRUE
    }

    the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)

    the_plot <- the_plot +
      geom_text(aes(label = labeling,
                    color = class,
                    group = class),
                show.legend = legend)
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

    the_plot <- the_plot +
      labs(x = x_label)

    the_caption <- NULL
  } else {
    x_label <- paste0("PC1 (",
                      round(pc_variance[1]*100,1),
                      "%)")

    the_plot <- the_plot +
      labs(x = x_label)
  }

  the_plot <- the_plot +
      theme(legend.position = legend_position)

  return(the_plot)
}

s2g_hc <- function(df_z, df, df_a, the_distance,
                   highlight, title, caption, the_caption,
                   labeling, classing, linkage, the_class,
                   highlight.nudge, num_shapes, my_shapes,
                   shapes, legend, horiz, axis.labels,
                   black, distance.measure){
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

  if (labeling != 0 && missing(legend)){
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

  the_ggdend$labels$class <- the_ggdend$labels$label %>%
    as.character() %>%
    strsplit("_") %>%
    sapply(`[`,1)

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
  }

  the_gplot <- the_gplot +
    geom_text(data = the_ggdend$labels,
              aes(x = x, y = y, label = labels, color = class),
              angle = the_angle, hjust = the_hjust, nudge_y = the_nudge,
              show.legend = text_legend)

  if (shapes) {

    the_gplot <- the_gplot +
      geom_point(data = the_ggdend$labels,
                 aes(x = x, y = y + point_shift, shape = class, color = class)) +
      scale_shape_manual(values = my_shapes)
  }

  if (horiz && !axis.labels) {
    the_gplot <- the_gplot +
      coord_flip() +
      scale_y_reverse(breaks = function(n) seq(0,
                                               round_any(max(n),0.5),
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
      the_plot <- the_plot +
        labs(y = paste0(the_distance,"\n")) +
        theme(axis.title.y = element_text(color = "black", angle = 90),
              axis.line.y = element_line(color = "black", size = 0.5),
              axis.ticks.y = element_line(color = "black", size = 0.5),
              axis.text.y = element_text(colour = "black")) +
        scale_y_continuous(breaks = function(n) seq(0,
                                                    round_any(max(n),0.5),
                                                    0.5))
      if ("lemon" %in% rownames(installed.packages())) {
        # library(lemon)
        the_plot <- the_plot +
          coord_capped_cart(left = "both") +
          expand_limits(y = 0)
      }
    } else if (horiz) {
      if (caption && !is.null(the_caption)) {
        the_distance <- paste0(the_distance,
                          "\n",
                          the_caption)
        the_caption <- NULL
      }

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
          scale_y_reverse(breaks = function(n) seq(0,
                                                   round_any(max(n),0.5),
                                                   0.5)) +
          expand_limits(y = 0)
      } else {
        the_gplot <- the_gplot +
          coord_flip() +
          scale_y_reverse(breaks = function(n) seq(0,
                                                   round_any(max(n),0.5),
                                                   0.5))
      }
    }

  } else {
    # the_plot <- the_dend %>% ggplot(horiz = horiz)
  }

  # if (legend && horiz) {
  #   the_max_x <<- ggplot_build(the_plot)$layout$panel_scales_x[[1]]$range$range[2]
  #   the_min_y <<- -1 * ggplot_build(the_plot)$layout$panel_scales_y[[1]]$range$range[1]
  #   the_max_y <<- -1 * ggplot_build(the_plot)$layout$panel_scales_y[[1]]$range$range[2]
  #
  #   legend_rows <- ceiling(length(unique(the_class)) / legend_cols)
  #
  #   legend_max_y <<- the_min_y
  #   legend_min_y <<- the_min_y*.05
  #
  #   legend_range_y <<- c()
  #   for (i in 1:legend_cols) {
  #     this_col <- i * (mean(legend_max_y - legend_min_y) / legend_cols)
  #     legend_range_y <<- c(legend_range_y, rep(this_col, legend_rows))
  #   }
  #
  #   x_range_max <- the_max_x/2 + length(unique(the_class))/2 #17
  #   x_range_min <- the_max_x/2 - length(unique(the_class))/2
  #
  #   fake_legend <<- data.frame(class = unique(the_class),
  #                             colors = the_colors,
  #                             shapes = the_newshape,
  #                             x = rep_len(rev(seq(the_max_x + 1,
  #                                                 the_max_x + legend_rows,
  #                                                 1)),
  #                                         length(unique(the_class))),
  #                             y = c(rep_len(rev(legend_range_y),
  #                                           length(unique(the_class)))))
  #
  #   the_plot <- the_plot +
  #     geom_point(data = fake_legend,
  #                aes(x = x,
  #                    y = y,
  #                    color = colors,
  #                    shape = the_newshape),
  #                show.legend = FALSE) +
  #     geom_text(data = fake_legend,
  #                aes(x = x,
  #                    y = y - 0.1,
  #                    label = class),
  #               hjust = 0)
  #
  # } else if (legend && !horiz) {
  #   vert_unit <- 0.2
  #
  #   the_min_x <- ggplot_build(the_plot)$layout$panel_scales_x[[1]]$range$range[1]
  #   the_max_x <- ggplot_build(the_plot)$layout$panel_scales_x[[1]]$range$range[2]
  #   the_min_y <- ggplot_build(the_plot)$layout$panel_scales_y[[1]]$range$range[1]
  #   the_max_y <- ggplot_build(the_plot)$layout$panel_scales_y[[1]]$range$range[2]
  #
  #   legend_rows <- ceiling(length(unique(the_class))/legend_cols)
  #
  #   legend_max_x <- the_max_x*.9
  #   legend_min_x <- the_max_x*.1
  #
  #   legend_range_x <- c()
  #   for (i in 1:legend_cols) {
  #     this_col <- i * (mean(legend_max_x - legend_min_x) / legend_cols)
  #     legend_range_x <- c(legend_range_x, rep(this_col, legend_rows))
  #   }
  #
  #   # y_range_max <- the_max_y/2 + length(unique(the_class))/2 #17
  #   # y_range_min <- the_max_y/2 - length(unique(the_class))/2
  #
  #   fake_legend <- data.frame(class = unique(the_class),
  #                              colors = the_colors,
  #                              shapes = the_newshape,
  #                              y = rep_len(rev(seq(the_max_y + vert_unit,
  #                                                  the_max_y + vert_unit * legend_rows,
  #                                                  vert_unit)),
  #                                          length(unique(the_class))),
  #                              x = c(rep_len(legend_range_x,
  #                                            length(unique(the_class)))))
  #
  #   the_plot <- the_plot +
  #     geom_point(data = fake_legend,
  #                aes(x = x,
  #                    y = y,
  #                    color = colors,
  #                    shape = the_newshape),
  #                show.legend = FALSE) +
  #     geom_text(data = fake_legend,
  #               aes(x = x + 0.1,
  #                   y = y,
  #                   label = class),
  #               hjust = 0)
  #
  # }

  # if (shapes) {
  #   the_plot <- the_plot +
  #     scale_shape_manual(values = rep(c(1, 3:11), length.out = num_shapes))
  # }

  if (!is.null(black)) {
    the_colors <- gg_color(num_shapes)

    the_colors[black] <- "#000000"

    the_plot <- the_plot +
      scale_color_manual(values = the_colors)
  }

  the_ggdend$segments$kind[the_ggdend$segments$x == the_ggdend$segments$xend] <- "horizontal"
  the_ggdend$segments$kind[the_ggdend$segments$y==the_ggdend$segments$yend] <- "vertical"

  the_plot <- s2g_highlight_rect(the_plot = the_plot,
                                 the_ggdend = the_ggdend,
                                 highlight = highlight,
                                 the_colors = the_colors,
                                 highlight.nudge)

  if (!missing(title)) {
    if (title == "") {
      title = NULL
    }
  }

  if (caption) {
    if (!is.null(the_caption)) {
      the_plot <- the_plot +
        labs(caption = the_caption)
    }
  }

  if (!is.null(title)) {
    the_plot <- the_plot +
      ggtitle(title)
  }
  return(the_plot)
}



s2g_highlight <- function(the_plot, df_pca, highlight) {
  if (!is.null(highlight)) {

    # add highlight for single-item classes
    for (h in 1:length(highlight)) {
    if (nrow(df_pca[df_pca$class == unique(df_pca$class)[highlight[h]],])==1){
      the_plot <- the_plot +
        geom_point(data = df_pca[df_pca$class == unique(df_pca$class)[highlight[h]],],
                   aes(color = class), size = 4, pch=21, stroke=1,
                   show.legend = FALSE)
    } }

    for (h in 1:length(highlight)) {
      the_plot <- the_plot +
        stat_density_2d(data = df_pca[df_pca$class == unique(df_pca$class)[highlight[h]],],
                        na.rm = FALSE,
                        aes(#linetype = class,
                          color = class,
                          alpha = 1/stat(nlevel),
                          group = class),
                        bins = 4,
                        show.legend = FALSE)
    }
    the_plot <- the_plot +
      # scale_linetype_manual(values = the_lines[order(highlight)]) +
      guides(alpha = FALSE, linetype = FALSE) +
      scale_alpha(range = c(1, 1), limits = c(3.1, 4), na.value = 0)

  }
  return(the_plot)
}

s2g_highlight_rect <- function(the_plot,
                               the_ggdend,
                               highlight,
                               the_colors,
                               highlight.nudge) {
  if (!is.null(highlight)) {
    h <- highlight
    tc <- sort(unique(the_ggdend$labels$class))[h]

    if (length(h) > 1) {
      message("Dendrograms can only highlight one class at a time. Use the geom_rect() function from ggplot2 to highlight manually.")
    } else {

      the_coords <- the_ggdend$labels[the_ggdend$labels$class == tc,"x"]

      start <- c(1, which(diff(the_coords) != 1 & diff(the_coords) != 0) + 1)
      end <- c(start - 1, length(the_coords))
      end <- end[end > 0]

      print(start)

      if (length(start) > 1) {
        the_branch_max <- c()
        the_branch_min <- c()
        for (i in 1:length(start)) {
          from1 <- the_ggdend$labels$y
          here <- the_coords[start[i]]
          there <- the_coords[end[i]]
          the_branch_min[i] <-
            from1[the_ggdend$labels$x %in% here:there] %>%
            max()

          from2 <- the_ggdend$segments$y
          the_branch_max[i] <-
            from2[the_ggdend$segments$yend ==
                the_branch_min[i]] %>%
            max()

          if (here == there) {
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
                                     the_branch_max[i])),
                       class = tc)
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
                      show.legend = TRUE) +
            guides(
              color = guide_legend(
                override.aes = list(linetype = silly_guides)
                )
              )
          # scale_linetype_manual(values = 2)
        }
      } else {
        the_branch_min <-
          the_ggdend$labels$y %>%
          .[the_ggdend$labels$x %in%
              the_coords[start]:the_coords[end]] %>%
          max()
        the_branch_max <-
          the_ggdend$segments$y %>%
          .[the_ggdend$segments$yend == the_branch_min] %>%
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
                                             the_branch_max)),
                               class = tc)

        silly_guides <- rep(0, the_ggdend$labels$class %>%
                              unique() %>%
                              length())

        silly_guides[h] <- 2

        the_plot <- the_plot +
          geom_rect(data = the_rect,
                    aes(xmin = xmin,
                        xmax = xmax,
                        ymin = ymin,
                        ymax = ymax,
                        color = class),
                    fill = "white", alpha = 0, linetype = 2) +
          guides(
            color = guide_legend(
              override.aes = list(
                linetype = silly_guides)
              )
            )
      }
    }
  }
  return(the_plot)
  }

gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
  }
