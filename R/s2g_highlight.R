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
