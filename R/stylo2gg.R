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
#' @param features A vector containing a selection of features
#' to consider for analysis. This option is useful for
#' replicating a previous analysis.
#' @param num.features The number of features to be used for an
#' analysis. By default, \code{stylo}'s settings are used, but
#' it is easy here limit the number to a smaller set, ordered
#' by frequency
#' @param top.loadings The number of features to show as
#' vectors in a principal components analysis. By default,
#' loadings are not shown unless \code{stylo}'s setting for
#' \code{pca.visual.flavour} is set to \code{"loadings"}; at
#' this time, it defaults to the full number of features. It's
#' probably most revealing to choose a smaller number, so that
#' only the most significant features are plotted.
#' @param select.loadings A list element, with items indicating
#' either the nearest location of a selected feature or the names
#' of these features. The location can be shown in three ways: 1.
#' with coordinates in the PCA space, e.g. \code{c(1,2)}; 2. as
#' the number of a category from which to derive an average
#' location, e.g. \code{4}; 3. as the name of a category or some
#' other element of the original text's filename from which to
#' derive an average location, e.g. \code{"hamilton"}. The name of
#' a feature is the fourth option: 4. using a call with the
#' \code{word} function; for example, to show the word
#' "undershirt," the list item would be
#' \code{call("word", "undershirt")}. Multiple types of items can
#' be combined in one list:
#' \code{select.loadings = list(c(1,2), 4, "hamilton", call("word", "undershirt"))}.
#' @param pc.x Identifies the principal component to be placed 
#' on the X-axis. Defaults to \code{1}.
#' @param pc.y Identifies the principal component to be placed 
#' on the Y-axis. Defaults to \code{2}.
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
#' @param loadings.spacer The spacer used to replace spaces in loading words, used for multiple-word n-grams. Define it as a quoted string. Defaults to \code{"_"}.
#' @param loadings.line.color A string defining the lines leading to loading words. Defaults to \code{lightgray}.
#' @param loadings.word.color A string defining the color used to display loading words. Defaults to \code{darkgray}.
#' @param loadings.upper Toggle (TRUE / FALSE) to convert loadings into uppercase or to leave them alone. The default is FALSE, keeping them unconverted.
#' @param plaintext Toggle (TRUE / FALSE) to show text labels as \code{geom_text()} layers (the default) or as \code{geom_label()} layers (when switched to \code{FALSE}.
#' @param withholding Specify a class or classes of texts to withhold from underlying principal components analysis before these texts are then projected into that space.
#'
#' @details
#' Because \code{stylo2gg} builds on \code{ggplot2}, almost all
#' commands available to that package should work here as well,
#' using the plus-sign syntax documented by that package.
#'
#' @examples
#' \dontrun{
#' my_data <- stylo()
#' my_data %>% stylo2gg()
#'
#' # Move the legend
#' my_data %>% stylo2gg() +
#'   theme(legend.position = "bottom")
#'}
#'
#' @import dendextend ggplot2 dplyr ggrepel lemon
#' @importFrom grDevices hcl
#' @importFrom graphics strwidth
#' @importFrom stats as.dendrogram as.dist dist hclust order.dendrogram prcomp sd
#' @importFrom stylo stylo stylo.default.settings dist.argamon dist.cosine dist.delta dist.eder dist.simple
#' @importFrom utils installed.packages
#' @export stylo2gg

stylo2gg <- function(df, viz, features,
                     num.features, top.loadings,
                     select.loadings, 
                     pc.x = 1, pc.y = 2,
                     title = NULL, caption = FALSE,
                     count.labels = FALSE,
                     legend, black = NULL, highlight = NULL,
                     labeling, classing, shapes = FALSE,
                     invert.x = FALSE, invert.y = FALSE,
                     scaling, distance.measure, linkage,
                     horiz = TRUE, axis.labels = FALSE,
                     highlight.nudge, highlight.single,
                     show.zero, highlight.box = NULL,
                     withholding,
                     loadings.spacer = "_", 
                     loadings.line.color = "lightgray", 
                     loadings.word.color = "darkgray",
                     loadings.upper = FALSE,
                     plaintext = TRUE) {
  s2g_export <<- list()
  df_a <- df
  if (missing(num.features)) {
    num.features <- length(df$features.actually.used)
  }
  
  if (!missing(features)) {
    num.features <- length(features)
  }
  
  if (!missing(scaling)) {
    scaling <- FALSE
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
  
  if (missing(top.loadings)) {
    if ("pca.visual.flavour" %in% names(my_call)) {
      if (my_call$pca.visual.flavour == "loadings") {
        top.loadings <- num.features
      }
    }
  } else if (top.loadings == "all") {
    top.loadings <- num.features
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
  
  if ("pca.visual.flavour" %in% names(my_call)) {
    if (my_call$pca.visual.flavour == "symbols" && missing(shapes)) {
      shapes <- TRUE
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
    
    the_linkage <- paste0(toupper(substr(the_linkage, 1, 1)),
                          substr(the_linkage, 2, nchar(the_linkage)))
    
    the_viz <- paste0(the_dist, " distance (",
                      the_linkage, " linkage)")
  }
  
  if ("analyzed.features" %in% names(my_call)) {
    if (missing(features)) {
      if (my_call$analyzed.features == "c") {
        the_features <- "MFC"
      } else if (my_call$analyzed.features == "w") {
        the_features <-  "MFW"
      }
    } else {
      if (my_call$analyzed.features == "c") {
        the_features <- "C"
      } else if (my_call$analyzed.features == "w") {
        the_features <-  "W"
      }
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
    scaling <- TRUE
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
  
  if (!missing(features)) {
    df <- df$table.with.all.freqs %>%
      .[,features] %>%
      as.data.frame()
    
    num.features <- length(features)
  } else {
    df <- df$table.with.all.freqs %>%
      .[,df$features.actually.used[1:num.features]] %>%
      as.data.frame()
  }
  
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
  if (!is.null(black)) {
    if (length(black) == 1) {
      my_shapes[black] <- 19
    }
  }
  
  if (viz == "PCV") {
    viz <- "pca"
    df_z <- df
  }
  
  if (missing(scaling)) {
    scaling <- FALSE
  }
  
  if (viz == "pca" || viz == "PCA" || viz == "PCR") {
    the_plot <- s2g_pca(df_z, df_a, the_class, labeling,
                        shapes, legend, highlight,
                        legend_position, num_shapes, my_shapes,
                        title, caption, black, the_caption,
                        scaling, invert.x, invert.y,
                        top.loadings,
                        select.loadings, 
                        pc.x, pc.y, 
                        withholding,
                        loadings.spacer, 
                        loadings.line.color, 
                        loadings.word.color,
                        loadings.upper,
                        plaintext)
  } else if (viz == "hc" || viz == "ca" || viz == "CA" || viz == "HC") {
    if (missing(highlight.single) && !is.null(highlight)) {
      highlight.single <- TRUE
    }
    if (missing(highlight.single) && !is.null(highlight.box)) {
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

gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
}

word <- function(x) {x}
