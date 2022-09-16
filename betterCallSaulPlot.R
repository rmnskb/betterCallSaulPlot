#Installing libraries required for plot creation and data manipulation
library(tidyverse)
#This package is for data labelling
library(ggrepel)
#Custom themes package from https://github.com/Mikata-Project/ggthemr
library(ggthemr)
#Image processing library
library(grid)
#Customising the font
library(showtext)
#custom color palettes for plots from https://jaredhuling.org/jcolors/
library(jcolors)

#Setting the plot theme and uploading font from https://www.bestfonts.io/better-call-saul-font/
ggthemr("earth", type = "outer")
font_add(family = "betterCallSaul",
         regular = "bcsFont.ttf")
showtext_auto()

#Uploading the image from https://www.pngwing.com/en/free-png-syvhi
bcsImage <- png::readPNG("bcsLogoTransparent.png")
bcsLogo <- rasterGrob(bcsImage)

#The data was taken from https://www.kaggle.com/datasets/venkataaravindmarni/better-call-saul-episodes-imdb-ratings
bcs <- read_csv("bcs.csv")
bcs$index <- c(1:63)

#ggplot2 will be used for creating the plot with the boundaries from 7.5 to 10
ggplot(bcs, aes(index, rating)) +
  #Adding the logo to the background as the first layer
  annotation_custom(bcsLogo, xmin = 18, xmax = 42,
                    ymin = 8.25, ymax = 9.0) + 
  #Adding points representing rating and coloured by season number as the second layer
  geom_point(size = 5,
             alpha = 0.9,
             aes(colour = factor(season))) + 
  #Labelling the episodes with the highest and the lowest rating
  geom_label_repel(data = filter(bcs,
                                 rating == max(rating) |
                                 rating == min(rating)),
             aes(label = title),
             nudge_x = 1,
             nudge_y = -0.05,
             segment.size = 0.2,
             segment.color = "grey",
             direction = "x") + 
  #Adding linear regression lines for easier trend understanding
  geom_smooth(method = "lm",
              se = FALSE,
              formula = "y ~ x",
              aes(colour = factor(season))) + 
  #Adding the labs 
  labs(x = "Episode Number",
       y = "Rating", 
       title = "Better Call Saul",
       subtitle = "Every episode's rating from IMDB",
       colour = "Season Number:",
       caption = "Source: IMDB.com | Author: @rmnskb") +
  #Adjusting the labs and positioning the legend at the bottom
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "betterCallSaul",
                                  face = "bold",
                                  size = 50),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 15),
        legend.position = "bottom") + 
  #setting the boundaries
  scale_y_continuous(limits = c(7.5, 10.0)) + 
  #Setting the palette
  scale_colour_jcolors(palette = "pal9") + 
  #Setting the legend keys in one row
  guides(colour = guide_legend(nrow = 1))



#ggplot2 will be used for creating the plot with the boundaries from 0 to 10
ggplot(bcs, aes(index, rating)) +
  #Adding the logo to the background as the first layer
  annotation_custom(bcsLogo, xmin = 18, xmax = 42,
                    ymin = 4.0, ymax = 6.0) + 
  #Adding points representing rating and coloured by season number as the second layer
  geom_point(size = 5,
             alpha = 0.9,
             aes(colour = factor(season))) + 
  #Labelling the episodes with the highest and the lowest rating
  geom_label_repel(data = filter(bcs,
                                 rating == max(rating) |
                                   rating == min(rating)),
                   aes(label = title),
                   nudge_x = 2.5,
                   nudge_y = 0.1,
                   segment.size = 0.2,
                   segment.color = "grey",
                   direction = "x") + 
  #Adding linear regression lines for easier trend understanding
  geom_smooth(method = "lm",
              se = FALSE,
              formula = "y ~ x",
              aes(colour = factor(season))) + 
  #Adding the labs 
  labs(x = "Episode Number",
       y = "Rating", 
       title = "Better Call Saul",
       subtitle = "Every episode's rating from IMDB",
       colour = "Season Number:",
       caption = "Source: IMDB.com | Author: @rmnskb") +
  #Adjusting the labs and positioning the legend at the bottom
  theme(plot.title = element_text(hjust = 0.5,
                                  family = "betterCallSaul",
                                  face = "bold",
                                  size = 50),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 15),
        legend.position = "bottom") + 
  #setting the boundaries
  scale_y_continuous(limits = c(0.0, 10.0)) + 
  #Setting the palette
  scale_colour_jcolors(palette = "pal9") + 
  #Setting the legend keys in one row
  guides(colour = guide_legend(nrow = 1))
