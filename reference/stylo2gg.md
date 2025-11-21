# Visualize `stylo` data with `ggplot2`.

Visualize `stylo` data with `ggplot2`.

## Usage

``` r
stylo2gg(
  df,
  viz,
  features,
  num.features,
  top.loadings,
  select.loadings,
  pc.x = 1,
  pc.y = 2,
  title = NULL,
  caption = FALSE,
  count.labels = FALSE,
  legend,
  black = NULL,
  highlight = NULL,
  labeling,
  classing,
  shapes = FALSE,
  invert.x = FALSE,
  invert.y = FALSE,
  scaling,
  distance.measure,
  linkage,
  horiz = TRUE,
  axis.labels = FALSE,
  highlight.nudge,
  highlight.single,
  show.zero,
  highlight.box = NULL,
  withholding,
  loadings.spacer = "_",
  loadings.line.color = "lightgray",
  loadings.word.color = "darkgray",
  loadings.upper = FALSE,
  plaintext = TRUE
)
```

## Arguments

- df:

  An object saved from running **`stylo`** on a corpus

- viz:

  A choice of visualizations, either `"pca"` for principal components
  analysis or `"hc"` for hierarchical clustering; alternatively, using
  `"PCR"`, `"PCV"`, or `"CA"`—all inherited from `stylo`—will reset a
  number of defaults.

- features:

  A vector containing a selection of features to consider for analysis.
  This option is useful for replicating a previous analysis.

- num.features:

  The number of features to be used for an analysis. By default,
  `stylo`'s settings are used, but it is easy here limit the number to a
  smaller set, ordered by frequency

- top.loadings:

  The number of features to show as vectors in a principal components
  analysis. By default, loadings are not shown unless `stylo`'s setting
  for `pca.visual.flavour` is set to `"loadings"`; at this time, it
  defaults to the full number of features. It's probably most revealing
  to choose a smaller number, so that only the most significant features
  are plotted.

- select.loadings:

  A list element, with items indicating either the nearest location of a
  selected feature or the names of these features. The location can be
  shown in three ways: 1. with coordinates in the PCA space, e.g.
  `c(1,2)`; 2. as the number of a category from which to derive an
  average location, e.g. `4`; 3. as the name of a category or some other
  element of the original text's filename from which to derive an
  average location, e.g. `"hamilton"`. The name of a feature is the
  fourth option: 4. using a call with the `word` function; for example,
  to show the word "undershirt," the list item would be
  `call("word", "undershirt")`. Multiple types of items can be combined
  in one list:
  `select.loadings = list(c(1,2), 4, "hamilton", call("word", "undershirt"))`.

- pc.x:

  Identifies the principal component to be placed on the X-axis.
  Defaults to `1`.

- pc.y:

  Identifies the principal component to be placed on the Y-axis.
  Defaults to `2`.

- title:

  The title that will go on the top of a chart. This value is inherited
  from `stylo` where possible. To remove a title, set it to `NULL` or to
  an empty set **""**.

- caption:

  Defaults to `FALSE`, except with certain **`viz`** settings. Change to
  toggle metadata at the bottom of a visualization.

- count.labels:

  Toggle (TRUE / FALSE) to show or hide counting numbers at the
  beginning of labels on a dendrogram. Useful for manually setting a
  **`highlight.box`** when constructing a plot, but probably not ideal
  for the final version of a dendrogram. Defaults to FALSE

- legend:

  Show or hide the legend with `TRUE` or `FALSE`.

- black:

  Cast the color of one category (defined by its number) as black. This
  setting is ideal to contrast a group for printing in black and white.

- highlight:

  Highlight a category (defined by its number) by drawing around its
  elements on the visualization. In a principal components analysis,
  multiple circular highlights are available to contrast sets; on a
  dendrogram, only one category can be highlighted with a box.

- labeling:

  Defines how to label items: if setting a character vector, define one
  string for each item in **df**; if setting a numeric vector (e.g, `1`
  or **2**), set it to the desired element (identified via `stylo`'s use
  of underscores in filenames).

- classing:

  The class or category for each item in **df**; if not set, it defaults
  to the first element before an underscore in the filename of items in
  the original corpus. `Stylo2gg` uses classing to distinguish items by
  color and shape.

- shapes:

  Defaults to `FALSE` unless **`labeling`** is defined. Change to toggle
  shapes on the visualization instead of (or in addition to) text
  labels. This is useful for printing in black and white, but it's also
  helpful to distinguish among similar colors.

- invert.x:

  Defaults to `FALSE`. Change to invert the horizontal orientation in a
  principal components analysis in order to approximate some ideal
  visualization. (I don't think this actually changes any understanding
  of the data.)

- invert.y:

  Defaults to `FALSE`. Change to invert the vertical orientation in a
  principal components analysis in order to approximate some ideal
  visualization. (I don't think this actually changes any understanding
  of the data.)

- scaling:

  Toggle the option to scale features before running a principal
  components analysis. Defaults to `FALSE`, except for
  **`viz = "PCR"`**; for all other principal components settings, the
  default is first to normalize features by z scores, which makes
  **`scaling`** less useful.

- distance.measure:

  The formula used for distance in hierarchical clustering. Defaults to
  `"delta"` to use Burrows's formula of Manhattan distance from
  normalized z-scores, but it might also be reasonable here to call
  `"euclidean"` or some other setting, imported from stylo.

- linkage:

  The linkage to be used for cluster analysis. Defaults to `"ward.D"`,
  but `"complete"` might also be a reasonable setting. Options include
  all those built into R.

- horiz:

  Set the rotation of the dendrogram in a hierarchical cluster analysis;
  defaults to `TRUE`

- axis.labels:

  Defaults to `FALSE` except when **`viz = "CA"`**. Change to `TRUE` to
  show a distance axis for the dendrogram in a cluster analysis.

- highlight.nudge:

  On a highlighted dendrogram, optionally define some extra space when a
  box overlaps the edge of a label.

- highlight.single:

  Toggle (TRUE/FALSE) to determine whether a dendrogram's highlight
  should draw a single box for all of the items or individual boxes for
  each cluster. When using **`highlight`**, this setting will default to
  `TRUE`; when using **`highlight.box`**, it will default too `FALSE`.

- show.zero:

  Toggle (TRUE / FALSE) for leaving space below the lowest distance to
  indicate zero

- highlight.box:

  On a dendrogram, highlight items indicated by their item numbers (from
  the bottom on a horizontal dendrogram, from the left on a vertical
  dendrogram); it might be helpful to toggle the **`count.labels`**
  parameter to `TRUE` to avoid having to count large data sets.

- withholding:

  Specify a class or classes of texts to withhold from underlying
  principal components analysis before these texts are then projected
  into that space.

- loadings.spacer:

  The spacer used to replace spaces in loading words, used for
  multiple-word n-grams. Define it as a quoted string. Defaults to
  `"_"`.

- loadings.line.color:

  A string defining the lines leading to loading words. Defaults to
  `lightgray`.

- loadings.word.color:

  A string defining the color used to display loading words. Defaults to
  `darkgray`.

- loadings.upper:

  Toggle (TRUE / FALSE) to convert loadings into uppercase or to leave
  them alone. The default is FALSE, keeping them unconverted.

- plaintext:

  Toggle (TRUE / FALSE) to show text labels as `geom_text()` layers (the
  default) or as `geom_label()` layers (when switched to `FALSE`.

## Details

Because `stylo2gg` builds on `ggplot2`, almost all commands available to
that package should work here as well, using the plus-sign syntax
documented by that package.

## Examples

``` r
if (FALSE) { # \dontrun{
my_data <- stylo()
my_data %>% stylo2gg()

# Move the legend
my_data %>% stylo2gg() +
  theme(legend.position = "bottom")
} # }
```
