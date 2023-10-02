create_logo <- function(){
  sticker_plot <- 
    federalist_mfw |> 
    rename_category("NA", "?") |> 
    stylo2gg(viz = "pca",
             shapes = FALSE, 
             labeling = 1,
             highlight = c(3, 4),
             legend = FALSE,
             black = 1) +
    theme_void() +
    theme_transparent() +
    scale_color_manual(
      values = c("orange", "slategray2",
                 "wheat3", "thistle3"))
  
  library(hexSticker)
  
  sticker(sticker_plot, 
          package="stylo2gg", 
          p_size=30, p_y = 1.3,
          s_x=1.05, s_y=.9, 
          s_width=2.1, s_height=2.0,
          p_color = "#000000", p_fontface = "bold",
          white_around_sticker = FALSE,
          h_fill="#fefefe", h_color="#f39c12", 
          filename="man/figures/stylo2gg.png")
  
  library(magick)
  
  image_read("man/figures/stylo2gg.png") |> 
    image_rotate(-30) |> 
    image_crop("597x518-39+42") |> 
    image_rotate(60) |> 
    image_crop("597x517-0-0") |> 
    image_rotate(-30) |> 
    image_crop("517x597+40-40") |> 
    image_fill(color = "transparent", 
               refcolor = "black", 
               fuzz = 65, point = "+1+1") |> 
    image_fill(color = "transparent", 
               refcolor = "black", 
               fuzz = 65, point = "+516+589") |> 
    image_write(path = "man/figures/stylo2gg.png")
  
  if(file.exists("man/figures/logo.png")){
    file.remove("man/figures/logo.png")
  }
  use_logo("man/figures/stylo2gg.png")
  file.remove("man/figures/stylo2gg.png")
}
