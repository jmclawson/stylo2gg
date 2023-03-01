get_nearest_loading_words <- function(xy,
                                      pca_list) {
  lo_words <- c()
  df_pca <- pca_list$x
  df_pca_rotation <- pca_list$rotation
  
  # using manhattan distance here
  # (would euclidean be better?)
  # nearest_loading <-
  #   abs(df_pca_rotation[,1] - xy[1]) %>%
  #   as.data.frame()
  #
  # nearest_loading <- nearest_loading +
  #   abs(df_pca_rotation[,2] - xy[2]) %>%
  #   as.data.frame()
  
  # euclidean seems better for angle, though
  # it can overshoot a position after scaling.
  # I think this is worth it.
  nearest_loading <-
    (df_pca_rotation[,1] - xy[1])^2 %>%
    as.data.frame()
  
  nearest_loading <- nearest_loading +
    (df_pca_rotation[,2] - xy[2])^2 %>%
    as.data.frame()
  
  nearest_loading <- nearest_loading %>%
    sqrt() %>%
    as.data.frame()
  
  nearest_loading <-
    nearest_loading[order(nearest_loading[,1]),,
                    drop = FALSE] %>%
    as.data.frame() %>%
    rownames() %>%
    .[1]
  loading_words <- lo_words %>%
    c(nearest_loading) %>%
    unique()
  
  return(loading_words)
}
