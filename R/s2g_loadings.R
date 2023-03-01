s2g_loadings <- function(the_plot,
                         pca_list,
                         top.loadings,
                         select.loadings, pc.x, pc.y,
                         invert.x,
                         invert.y,
                         loadings_spacer, 
                         loadings_line_color, 
                         loadings_word_color,
                         loadings_upper) {
  
  df_pca <- as.data.frame(pca_list$x)
  df_pca_rotation <- as.data.frame(pca_list$rotation)
  
  if (!missing(select.loadings)) {
    loading_words <- c()
    if (mode(select.loadings)=="list") {
      loadings_df <-
        data.frame(loading=c(),
                   PC1=numeric(),
                   PC2=numeric(),
                   stringsAsFactors = FALSE)
      for (i in select.loadings) {
        if (is.call(unlist(i))) {
          loading_words <- loading_words %>%
            c(eval(unlist(i)))
        } else if (length(unlist(i)) == 1) {
          t_i <- unlist(i)
          # here's what I do if it's the
          # name or number of a class or
          # a word
          if (mode(t_i) == "character") {
            this_loading_words <-
              t_i %>%
              get_class_loading_words(pca_list)
            loading_words <- loading_words %>%
              c(this_loading_words)
          } else {
            classes <- rownames(df_pca) %>%
              strsplit("_") %>%
              sapply(`[`, 1) %>%
              unique() %>%
              sort()
            this_loading_words <-
              classes[t_i] %>%
              get_class_loading_words(pca_list)
            loading_words <- loading_words %>%
              c(this_loading_words)
          }
        } else if (length(unlist(i)) == 2) {
          t_i <- unlist(i)
          # here's what to do if it's
          # coordinates to aim near
          
          if (invert.x) {
            t_i[1] <- t_i[1]*(-1)
          }
          
          if (invert.y) {
            t_i[2] <- t_i[2]*(-1)
          }
          this_loading_words <-
            t_i %>%
            get_nearest_loading_words(pca_list)
          loading_words <- loading_words %>%
            c(this_loading_words)
          
        } else if (length(unlist(i)) > 2) {
          warning("Each item in a select.loadings list should not exceed length of 2, representing the coordinates of the desired loading.")
        }
      }
    } else {
      # when select.loadings isn't a list
    }
  }
  
  max_x <- max(df_pca$PC1)
  min_x <- min(df_pca$PC1)
  max_y <- max(df_pca$PC2)
  min_y <- min(df_pca$PC2)
  
  s2g_export$pca <<- df_pca
  
  df_rotation <- as.data.frame(df_pca_rotation)
  
  if (missing(select.loadings)) {
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
    
    # wait to limit word choices after distance is known
    loadings_df <- df_rotation %>% 
      mutate(distance = sqrt(PC1^2 + PC2^2)) %>% 
      arrange(-distance)
    
    s2g_export$loadings <<- loadings_df %>% 
      # don't limit what's exported here - 01/18/2022
      # select(PC1, PC2, distance) %>% 
      mutate(PC1 = if(invert.x){-1*PC1} else{PC1},
             PC2 = if(invert.y){-1*PC2} else{PC2},
             angle = atan(PC2/PC1)*(360/(2*pi)), 
             angle = case_when(
               PC1 < 0   ~ angle + 180, 
               angle < 0 ~ angle + 360, 
               TRUE      ~ angle) %>% 
               round(2))
    
    loadings_df <- loadings_df %>% 
      .[1:top.loadings,]
    
  } else {
    loadings_df <-
      df_rotation[rownames(df_rotation) %in% loading_words,1:2]
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
  
  # Standardize spaces in loadings
  feature_spaces <- strsplit(rownames(loadings_df),"[A-Za-z:.]") %>%
    unlist() %>%
    max() %>%
    nchar()
  
  if (feature_spaces >= 2) {
    rownames(loadings_df) <- rownames(loadings_df) %>%
      gsub(pattern = "\\s{2,}",
           replacement = loadings_spacer,
           x = .) %>%
      gsub(pattern = "\\s+",
           replacement = "",
           x = .)
  } else {
    rownames(loadings_df) <- rownames(loadings_df) %>%
      gsub(pattern = "\\s{1,}",
           replacement = loadings_spacer,
           x = .)
  }
  
  if(loadings_upper) {
    rownames(loadings_df) <- 
      rownames(loadings_df) |> 
      toupper()
  }
  
  if (invert.x) {
    loadings_df_scaled$PC1 <- loadings_df_scaled$PC1 * -1
  }
  
  if (invert.y) {
    loadings_df_scaled$PC2 <- loadings_df_scaled$PC2 * -1
  }
  
  the_plot <- the_plot +
    geom_segment(data = loadings_df_scaled,
                 aes(x = 0,
                     y = 0,
                     xend = PC1 * 0.75,
                     yend = PC2 * 0.75),
                 # arrow = arrow(length = unit(0.2,"cm")),
                 color = loadings_line_color) +
    geom_label(data = loadings_df_scaled,
               aes(x = PC1*0.75,
                   y = PC2*0.75,
                   label = rownames(loadings_df)),
               size = 4,
               color = "white",
               fill = alpha(c("white"),0.5)) +
    geom_text(data = loadings_df_scaled,
              aes(x = PC1*0.75,
                  y = PC2*0.75,
                  label = rownames(loadings_df)),
              size = 5,
              color = loadings_word_color)
  
  return(the_plot)
}
