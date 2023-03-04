s2g_loadings <- function(the_plot,
                         pca_list,
                         top.loadings,
                         select.loadings, pc.x, pc.y,
                         invert.x,
                         invert.y,
                         loadings.spacer, 
                         loadings.line.color, 
                         loadings.word.color,
                         loadings.upper) {
  
  df_pca <- as.data.frame(pca_list$x) |> 
    rename("pc_x" = pc.x,
           "pc_y" = pc.y)
  df_pca_rotation <- as.data.frame(pca_list$rotation)
  
  if (!missing(select.loadings)) {
    loading_words <- c()
    if (mode(select.loadings)=="list") {
      loadings_df <-
        data.frame(loading=c(),
                   pc_x = numeric(),
                   pc_y = numeric(),
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
              get_class_loading_words(pca_list, pc.x, pc.y)
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
              get_class_loading_words(pca_list, pc.x, pc.y)
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
  
  max_x <- max(df_pca$pc_x)
  min_x <- min(df_pca$pc_x)
  max_y <- max(df_pca$pc_y)
  min_y <- min(df_pca$pc_y)
  
  s2g_export$pca <<- df_pca
  
  df_rotation <- as.data.frame(df_pca_rotation) |> 
    rename("pc_x" = pc.x,
           "pc_y" = pc.y)
  
  s2g_export$df_rotation <<- df_rotation
  
  if (missing(select.loadings)) {
    df_rotation_abs <-
      data.frame(pc_x = df_rotation$pc_x %>%
                   as.numeric() %>%
                   abs(),
                 pc_y = df_rotation$pc_y %>%
                   as.numeric() %>%
                   abs(),
                 word = rownames(df_rotation),
                 stringsAsFactors = FALSE)
    
    pc_x_words <-
      df_rotation_abs$word[order(df_rotation_abs$pc_x,
                                 decreasing = TRUE)]
    
    pc_y_words <-
      df_rotation_abs$word[order(df_rotation_abs$pc_y,
                                 decreasing = TRUE)]
    
    # wait to limit word choices after distance is known
    loadings_df <- df_rotation %>% 
      mutate(distance = sqrt(pc_x^2 + pc_y^2)) %>% 
      arrange(-distance)
    
    s2g_export$loadings <<- loadings_df %>% 
      mutate(pc_x = if(invert.x){-1*pc_x} else{pc_x},
             pc_y = if(invert.y){-1*pc_y} else{pc_y},
             angle = atan(pc_y/pc_x)*(360/(2*pi)), 
             angle = case_when(
               pc_x < 0   ~ angle + 180, 
               angle < 0 ~ angle + 360, 
               TRUE      ~ angle) %>% 
               round(2))
    
    loadings_df <- loadings_df %>% 
      .[1:top.loadings,]
    
  } else {
    loadings_df <-
      df_rotation[rownames(df_rotation) %in% loading_words, 
                  c(pc.x, pc.y)]
  }
  
  max_pc_x <- max(df_rotation$pc_x)
  min_pc_x <- min(df_rotation$pc_x)
  max_pc_y <- max(df_rotation$pc_y)
  min_pc_y <- min(df_rotation$pc_y)
  
  loadings_df_scaled <- loadings_df[,c("pc_x", "pc_y")]
  loadings_df_scaled[,1] <- loadings_df_scaled[,1] *
    (max_x - min_x)/(max_pc_x - min_pc_x)
  loadings_df_scaled[,2] <- loadings_df_scaled[,2] *
    (max_y - min_y)/(max_pc_y - min_pc_y)
  
  # Standardize spaces in loadings
  feature_spaces <- strsplit(rownames(loadings_df),"[A-Za-z:.]") %>%
    unlist() %>%
    max() %>%
    nchar()
  
  if (feature_spaces >= 2) {
    rownames(loadings_df) <- rownames(loadings_df) %>%
      gsub(pattern = "\\s{2,}",
           replacement = loadings.spacer,
           x = .) %>%
      gsub(pattern = "\\s+",
           replacement = "",
           x = .)
  } else {
    rownames(loadings_df) <- rownames(loadings_df) %>%
      gsub(pattern = "\\s{1,}",
           replacement = loadings.spacer,
           x = .)
  }
  
  if(loadings.upper) {
    rownames(loadings_df) <- 
      rownames(loadings_df) %>% 
      toupper()
  }
  
  if (invert.x) {
    loadings_df_scaled[[1]] <- loadings_df_scaled[[1]] * -1
  }
  
  if (invert.y) {
    loadings_df_scaled[[2]] <- loadings_df_scaled[[2]] * -1
  }
  
  the_plot <- the_plot +
    geom_segment(data = loadings_df_scaled,
                 aes(x = 0,
                     y = 0,
                     # trying to rework for pc.x and pc.y
                     xend = pc_x * 0.75,
                     yend = pc_y * 0.75),
                 # arrow = arrow(length = unit(0.2,"cm")),
                 color = loadings.line.color) +
    geom_label(data = loadings_df_scaled,
               aes(x = pc_x * 0.75,
                   y = pc_y * 0.75,
                   label = rownames(loadings_df)),
               size = 4,
               color = "white",
               fill = alpha(c("white"),0.5)) +
    geom_text(data = loadings_df_scaled,
              aes(x = pc_x * 0.75,
                  y = pc_y * 0.75,
                  label = rownames(loadings_df)),
              size = 5,
              color = loadings.word.color)
  
  return(the_plot)
}
