library(tidyverse)
library(hexSticker)
library(rando)


bp <- blueprint(
  x = r_norm(),
  y = r_norm(),
  col_red = r_unif(),
  col_green = r_unif(),
  col_blue = r_unif(),
  fill_red = r_unif(),
  fill_green = r_unif(),
  fill_blue = r_unif(),
  size = r_lnorm(),
  alpha = r_unif(),
  shape = r_sample(1:25)
  )

bp_logo <- blueprint(
  label = c("r","a","n","d","o"),
  family = r_sample(c("sans","serif","mono")),
  fontface = r_sample(c("plain", "bold", "italic", "bold.italic")),
  size = 18,
  x = r_norm(seq(-1,1,length.out=n),0.05),
  y = r_norm(-0.05,sd=0.3)
)

my_text <- function(.seed){
  data <- bp_logo(n=5,.seed=.seed)
  text_list <- map(1:ncol(data),
      ~paste0(names(data)[.]," = ",{
              if(is.numeric(data[[.]]))
                data[[.]] else
                  paste0("\"",data[[.]],"\"")})) %>%
    transpose %>%
    map(unlist) %>%
    map(paste0,collapse=", ") %>%
    map(~paste0("annotate(\"text\", ",.,")")) %>%
    map(str2lang) %>%
    map(eval)
}

x_r <- 1.3
y_r <- 0.8
y_r2 <- 0.9

triangles <- tibble(
  x = x_r*c(-2,1,-2, -1,2,2, -2,-2,1, -1,2,2),
  y = y_r2*c(3,3,0, 3,3,0, 0,-3,-3, -3,-3,0)/2,
  grp = rep(c("TL","TR","BL","BR"),each=3)
)

p <- ggplot() +
  geom_point(aes(x=x,y=y,
                 col=rgb(col_red,col_green,col_blue),
                 fill=rgb(fill_red,fill_green,fill_blue),
                 size=size,alpha=alpha,
                 shape = shape),
             data=bp(n=2000,.seed=12345))+ 
  geom_polygon(aes(x=x,y=y,group=grp),inherit.aes=FALSE,
               data=triangles,fill="white") +
  my_text(.seed = 2412) +
  scale_colour_identity() +
  scale_size_identity() +
  scale_alpha_identity() +
  scale_shape_identity() +
  coord_cartesian(xlim=c(-1,1)*x_r,ylim=c(-1,1)*y_r) +
  theme_void() + 
  theme(legend.position = "none")



sticker(p, package="",filename = "rando.svg",
        s_x=1, s_y=1, s_width=2, s_height=2,
        h_fill = "white",h_size=3, h_col="black",
        white_around_sticker=T
        )



