library(keras)

# 1. Lav en liste over dine 12 filnavne
filnavne <- c(
  "1.jpg", "2.jpg", "3.jpg", "4.jpg", "5.jpg", "6.jpg", "7.jpg", "8.jpg", "9.jpg", 
  "10.jpg", "11.jpg")

# 2. Lav dit facit (y_train). Lad os sige ur = 0, og ikke-ur = 1.
# De første 6 er 1 ur (0), de næste 5 er ikke et ur (1).
y_train <- c(1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0)

# Gør det til et rigtigt "array", som Keras forventer
y_train <- as.array(y_train)

# 3. Klargør x_train
# Vi skaber en tom 4D "kasse" til at holde billederne: 12 billeder, 75x100 pixels, 3 farver
x_train <- array(0, dim = c(11, 75, 100, 3))

# 4. Kør et loop, der indlæser hvert billede og putter det i kassen
for (i in 1:length(filnavne)) {
  # Indlæs og krymp til 32x32
  img <- image_load(filnavne[i], target_size = c(75, 100))
  
  # Konvertér til tal og divider med 255 for at skalere farverne
  img_array <- image_to_array(img) / 255
  
  # Sæt billedet ind på den rigtige plads (i) i vores store kasse
  x_train[i, , , ] <- img_array
}


####################################################################################
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", 
                input_shape = c(75, 100, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  
  # RETTELSEN ER HER: Vi har kun 2 klasser nu!
  layer_dense(units = 2, activation = "softmax")

####################################################################################

model %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 15,          # Du kan give den lidt flere runder, nu hvor datasættet er så lille
  batch_size = 4        # Den kigger på 4 billeder ad gangen
)

