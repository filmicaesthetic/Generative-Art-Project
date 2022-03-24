##########
# Generate image
##########

library(dplyr)
library(tidyr)
library(ggplot2)
library(keras)

set.seed(72)

# create single image as start point
random_image <- image_1 %>% mutate(value = c(rep(1, each = (nrow(image_1))))) #rep(0, each = nrow(image_1))
random_image$value[runif(1, 1, length(random_image))] <- 0
# ggplot(random_image, aes(x = x, y = y, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient(low = "white", high = "black", na.value = NA) +
#   scale_y_reverse() +
#   theme_minimal() +
#   theme(panel.grid = element_blank())   +
#   theme(aspect.ratio = 1) +
#   xlab("") +
#   ylab("")

# use img from test set as I can't figure out how to create another object like this
img <- test_images[1, , , drop = FALSE]

# reformat starter image
random_mat <- random_image %>% spread(x, value) %>% select(-y)

# replace img with starter image data
img[1, 1:28, 1:28] <- random_image$value

# predict a single image
current_predictions <- predictions <- model %>% predict(img) 

# set variables
current_predictions
current_image <- random_image / 255 # normalise the values!
it_img <- img
imp <- 0
cat <- 6

# rudimentary optimisation model
for (i in seq(1:2000)) {
  image_it <- current_image
  pix <- runif(1, 1, 20)
  image_it$value[c(runif(pix, 1, length(image_it$value)))] <- c(runif(pix, 0, 255)) / 255
  it_img[1, 1:28, 1:28] <- image_it$value
  
  pred <- model %>% predict(it_img) 
  
  if (abs(pred[cat] - 0.7) <= abs(current_predictions[cat] - 0.7))  {
    imp <- imp + 1
    current_predictions <- pred
    img <- it_img
    current_image <- image_it
  }
  
  image_it <- current_image
  pix <- runif(1, 30, 200)
  image_it$value[c(runif(pix, 1, length(image_it$value)))] <- c(runif(pix, 0, 255)) / 255
  it_img[1, 1:28, 1:28] <- image_it$value

  if (abs(pred[cat] - 0.8) <= abs(current_predictions[cat] - 0.8)) {
    imp <- imp + 1
    current_predictions <- pred
    img <- it_img
    current_image <- image_it
  }
  
  image_it <- current_image
  pix <- 10
  wiggle <- runif(1, 13, 300) 
  image_it$value[c(wiggle, wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29), wiggle + runif(1, 27, 29))] <- c(runif(pix, 200, 255)) / 255
  it_img[1, 1:28, 1:28] <- image_it$value
  
  pred <- model %>% predict(it_img) 
  
  if (abs(pred[cat] - 0.9) <= abs(current_predictions[cat] - 0.9))  {
    imp <- imp + 1
    current_predictions <- pred
    img <- it_img
    current_image <- image_it
  }
}

# view predictions
current_predictions

# View created image
ggplot(current_image, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")


# tested criteria
# to keep the predictions closer to even:
# if (max(pred)-min(pred) <= max(current_predictions) - min(current_predictions) & min(pred) > 0)
# aim for highest probability value in chosen category
# if (pred[3] >= current_predictions[3])
# aim for specific probability value in chosen category:
# if (abs(pred[2] - 0.8) <= abs(current_predictions[2] - 0.8))
# maybe create something that "inverses" every pixel on each loop, to help move it along quicker?