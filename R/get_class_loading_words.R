get_class_loading_words <- function(clue,
                                    pca_list) {
  
  df_pca <- pca_list$x
  
  this_sub_x <-
    df_pca[grep(clue,
                rownames(df_pca)),1] %>%
    mean()
  this_sub_y <-
    df_pca[grep(clue,
                rownames(df_pca)),2] %>%
    mean()
  
  loading_words <- this_sub_x %>%
    c(this_sub_y) %>%
    get_nearest_loading_words(pca_list)
  
  return(loading_words)
}
