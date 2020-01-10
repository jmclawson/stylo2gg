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
#' @param num.loadings The number of features to show as
#' vectors in a principal components analysis. By default,
#' loadings are not shown unless \code{stylo}'s setting for
#' \code{pca.visual.flavour} is set to \code{"loadings"}; at
#' this time, it defaults to the full number of features. It's
#' probably most revealing to choose a smaller number, so that
#' only the most significant features are plotted.
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
#' @param highlight.box On a dendrogram, highlight items indicated by their item numbers (from the bottom on a horizontal dendrogram, from the left on a vertical dendrogram); it might be helpful to toggle the \strong{\code{count.labels}} parameter to
#' \code{TRUE} to avoid having to count large data sets.
#' @param highlight.nudge On a highlighted dendrogram,
#' optionally define some extra space when a box overlaps the
#' edge of a label.
#' @param highlight.single Toggle (TRUE/FALSE) to determine whether a dendrogram's highlight should draw a single box for all of the items or individual boxes for each cluster. When using \strong{\code{highlight}}, this setting will default to \code{TRUE}; when using \strong{\code{highlight.box}}, it will default too \code{FALSE}.
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
#' @param show.zero Toggle (TRUE / FALSE) for leaving space below the lowest distance to indicate zero
#' @param count.labels Toggle (TRUE / FALSE) to show or hide counting numbers at the beginning of labels on a dendrogram. Useful for manually setting a \strong{\code{highlight.box}} when constructing a plot, but probably not ideal for the final version of a dendrogram. Defaults to FALSE
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

stylo2gg <- function(df, viz, num.features, num.loadings,
                     title = NULL, caption = FALSE,
                     count.labels = FALSE,
                     legend, black = NULL, highlight = NULL,
                     labeling, classing, shapes = FALSE,
                     invert.x = FALSE, invert.y = FALSE,
                     scaling = FALSE, distance.measure, linkage,
                     horiz = TRUE, axis.labels = FALSE,
                     highlight.nudge, highlight.single,
                     show.zero, highlight.box = NULL) {
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

  if (missing(num.loadings)) {
    if ("pca.visual.flavour" %in% names(my_call)) {
      if (my_call$pca.visual.flavour == "loadings") {
        num.loadings <- num.features
      }
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
      if(missing(show.zero)) {
        if (!missing(axis.labels) && axis.labels) {
          show.zero <- TRUE
        } else {
          show.zero <- FALSE
        }
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
                        scaling, invert.x, invert.y,
                        num.loadings)
  } else if (viz == "hc" || viz == "ca" || viz == "CA" || viz == "HC") {
    if (missing(highlight.single) && !is.null(highlight)){
      highlight.single <- TRUE
    }
    if (missing(highlight.single) && !is.null(highlight.box)){
      highlight.single <- FALSE
    }
    the_plot <- s2g_hc(df_z, df, df_a, the_distance,
                       highlight, title, caption, the_caption,
                       labeling, classing, linkage, the_class,
                       highlight.nudge, num_shapes, my_shapes,
                       shapes, legend, horiz, axis.labels,
                       show.zero, highlight.box, count.labels,
                       black, distance.measure, highlight.single
    )
  }

  if (!missing(title)) {
    if (!is.expression(title)) {
      if (title == "") {
        title = NULL
      }
    }
  }

  if (!is.null(title)) {
    the_plot <- the_plot +
      ggtitle(title)
  }

  return(the_plot)
  }

s2g_pca <- function(df_z, df_a, the_class, labeling,
                    shapes, legend, highlight,
                    legend_position, num_shapes, my_shapes,
                    title, caption, black, the_caption,
                    scaling, invert.x, invert.y, num.loadings){
  df_pca <- prcomp(df_z, scale. = scaling)
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

  if (missing(num.loadings)) {
    the_plot <- the_plot +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = 0, color = "gray")
  } else if (num.loadings > 0) {
    the_plot <- s2g_loadings(the_plot,
                             pca_list,
                             num.loadings)
  } else {
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

  if (missing(labeling)) {
    if (missing(legend)) {
      legend <- TRUE
    }

    the_plot <- the_plot +
      geom_point(aes(shape = class,
                     color = class),
                 show.legend = legend) +
      scale_shape_manual(values = my_shapes)

    # the_plot <- s2g_highlight(the_plot, df_pca = df_pca, highlight = highlight)

  } else if (shapes) {
    if (missing(legend)) {
      legend <- TRUE
    }

    # library(ggrepel)
    the_plot <- the_plot +
      geom_point(aes(shape = class,
                     color = class),
                 show.legend = legend)

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

    the_plot <- the_plot +
      geom_text(aes(label = labeling,
                    color = class,
                    group = class),
                show.legend = legend)
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
                show.legend = FALSE) +
      guides(
        color = guide_legend(
          override.aes = list(linetype = silly_guides)
        )
      )
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

  if (caption) {
    if (!is.null(the_caption)) {
      the_plot <- the_plot +
        labs(caption = the_caption)
    }
  }
  return(the_plot)
}

s2g_loadings <- function(the_plot,
                         pca_list,
                         # df_pca,
                         # df_pca_rotation,
                         num.loadings) {
  df_pca <- as.data.frame(pca_list$x)
  df_pca_rotation <- as.data.frame(pca_list$rotation)

  max_x <- max(df_pca$PC1)
  min_x <- min(df_pca$PC1)
  max_y <- max(df_pca$PC2)
  min_y <- min(df_pca$PC2)

  df_rotation <- as.data.frame(df_pca_rotation)
  df_rotation_abs <-
    data.frame(PC1 = df_rotation$PC1 %>%
                 as.numeric() %>%
                 abs(),
               PC2 = df_rotation$PC2 %>%
                 as.numeric() %>%
                 abs(),
               word = rownames(df_rotation),
               stringsAsFactors = FALSE)

  pc1_words <-
    df_rotation_abs$word[order(df_rotation_abs$PC1,
                                decreasing = TRUE)]

  pc2_words <-
    df_rotation_abs$word[order(df_rotation_abs$PC2,
                                decreasing = TRUE)]

  loading_words <- c(pc1_words[1:num.loadings],
                     pc2_words[1:num.loadings]) %>%
    unique()

  loadings_df <-
    df_rotation[rownames(df_rotation) %in% loading_words,1:2]

  # Figure out which vectors are longest
  loadings_df[,3] <-
    (loadings_df[,1])^2 + (loadings_df[,2])^2

  # order the rows by length
  loadings_df <- loadings_df[order(loadings_df[,3],
                                   decreasing = TRUE),]

  # limit number of rows to num.loadings
  if (length(loading_words) > num.loadings) {
    loadings_df <- loadings_df[1:num.loadings,]
  }

  max_pc1 <- max(df_rotation$PC1)
  min_pc1 <- min(df_rotation$PC1)
  max_pc2 <- max(df_rotation$PC2)
  min_pc2 <- min(df_rotation$PC2)

  loadings_df_scaled <- loadings_df[,1:2]
  loadings_df_scaled[,1] <- loadings_df_scaled[,1] *
    (max_x - min_x)/(max_pc1 - min_pc1)
  loadings_df_scaled[,2] <- loadings_df_scaled[,2] *
    (max_y - min_y)/(max_pc2 - min_pc2)

  the_plot <- the_plot +
    geom_segment(data = loadings_df_scaled,
                 aes(x = 0,
                     y = 0,
                     xend = PC1 * 0.72,
                     yend = PC2 * 0.72),
                 arrow = arrow(length = unit(0.2,"cm")),
                 color = "gray") +
    geom_text(data = loadings_df_scaled,
              aes(x = PC1*0.75,
                  y = PC2*0.75,
                  label = rownames(loadings_df)),
              size = 5,
              color = "darkgray")

  return(the_plot)
}

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
  # the_ggdend_a <<- the_ggdend
  # the_ggdend_a$df_a <<- match_df

  the_ggdend$labels <-
    the_ggdend$labels[match(rownames(match_df),
        gsub(" ","",as.character(the_ggdend$labels$label))),]

  # the_ggdend_b <<- the_ggdend

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



s2g_highlight <- function(the_plot, df_pca, highlight) {
  if (!is.null(highlight)) {

    for (h in 1:length(highlight)) {
      # add highlight for single-item classes
      if (nrow(df_pca[df_pca$class == unique(df_pca$class)[highlight[h]],])==1){
        the_plot <- the_plot +
          geom_point(data = df_pca[df_pca$class == unique(df_pca$class)[highlight[h]],],
                     aes(color = class), size = 4, pch=21, stroke=1,
                     show.legend = FALSE)
      }
      # add highlights for all other classes
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

s2g_highlight_rect <- function(the_plot = the_plot,
                               the_ggdend = the_ggdend,
                               highlight = highlight,
                               the_colors = the_colors,
                               highlight.nudge,
                               highlight.single,
                               highlight.box, legend) {
  # the_ggdend <<- the_ggdend

  if(!is.null(highlight)) {

    h <- highlight
    tc <- sort(unique(the_ggdend$labels$class))[h]

    the_coords <-
      the_ggdend$labels[the_ggdend$labels$class == tc,"x"]
    the_coords <- sort(the_coords)

    if (length(h) > 1) {
      message("Dendrograms can only highlight one class at a time. Use the geom_rect() function from ggplot2 to highlight manually.")
      }
    }

  if (!is.null(highlight.box)) {
    the_coords <- sort(highlight.box)
    }

  start <- c(1, which(diff(the_coords) != 1 & diff(the_coords) != 0) + 1)
  end <- c(start - 1, length(the_coords))
  end <- end[end > 0]

  # the_coords <<- the_coords
  # start <<- start
  # end <<- end

  if (highlight.single) {
    start <- 1
    end <- length(the_coords)
    }

  if (length(start) > 1) {
    the_branch_max <- c()
    # the_branch_max <<- c()
    the_branch_min <- c()
    # the_branch_min <<- c()
    for (i in 1:length(start)) {
      # from1 <- the_ggdend$labels$y
      bottom <- the_coords[start[i]]
      top <- the_coords[end[i]]
      from1 <- the_ggdend$segments
      this_tab <- from1$y[from1$x >= bottom &
                          from1$x <= top] %>%
        table()
      # this_tab <<- this_tab
      the_branch_min[i] <-
        this_tab[this_tab == max(this_tab)] %>%
        names() %>%
        as.numeric() %>%
        max()

      the_branch_min[i] <-
        from1$y[round(from1$y,5) ==
                round(the_branch_min[i],5)] %>%
        max(na.rm = TRUE)

      # the_branch_min[i] <<- the_branch_min[i]

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

    # the_branch_min <<- the_branch_min
    # the_branch_max <<- the_branch_max

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

gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
  }
