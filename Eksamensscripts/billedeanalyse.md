**Billedeanalyse**
================

# Indhold

- API-kald
- Tensorflow

# API-kald fra SMK.dk

**Statuskoder** <br> Kan ses under variabal af GET-request -\>
status_code

- 200: forespørgsel succesfuld
- 404: side ikke fundet
- 500: serverfejl

``` r
library(httr)
library(dplyr)
library(jsonlite)

base_url <- "https://api.smk.dk/api/v1"

#PERSON
person <- GET(paste0(base_url, "/person/search"), query = list(keys = "Erling Eckersberg", rows = 2000))
rescontent <- content(person, as="text")
resretval <- jsonlite::fromJSON(rescontent)

df <- as.data.frame(resretval$items)


#ART FRA ERLING ECKERSBERG
eck <- GET(paste0(base_url, "/art/search"), query = list(keys = "Erling Eckersberg", rows = 2000))
rescontent <- content(eck, as="text")
resretval <- jsonlite::fromJSON(rescontent)

df <- as.data.frame(resretval$items)

df_eck <- df %>% filter(artist == "Erling Eckersberg")
```

# Tensorflow

## Installation og setup

**Fremgangsmåde** <br> Installer “tensorflow” i python med kommandoen:
“pip install tensorflow” mens du er inde i det virtuelle environment
“llms-course”. Husk det kan åbnes fra Anaconda Navigator. Kør så dette i
R:

``` r
install.packages("tensorflow") #slet efter brug
install.packages("keras") #slet efter brug
install.packages("reticulate") #slet efter brug
install.packages("imager") #slet efter brug
install.packages("factoextra") #slet efter brug
library(factoextra)
library(imager)
library(tensorflow)
library(keras)
library(reticulate)
```

Indsæt din egen sti. Bare husk at det skal være pythonversionen som
ligger i det virtuelle miljø “llms-course”. Deruover skal
Python-interpreter også vælges fra indstillinger i R: tools -\> global
options -\> Python -\> select (intepreter) -\> conda environments -\>
llms-course.

``` r
use_python("C:\\Users\\marti\\anaconda3\\envs\\llms-course\\python.exe", required = TRUE)
install_tensorflow(envname = "r-reticulate", force = TRUE)
```

Genstart så R og kør dette med din egen sti:

``` r
library(imager)
library(factoextra)
library(keras)
library(reticulate)
library(tensorflow)

use_python("C:\\Users\\marti\\anaconda3\\envs\\llms-course\\python.exe", required = TRUE)
install_keras()
install_tensorflow()
```

## Algoritme: Ur vs. ikke-ur

Opsætning af data og mapper. Der skal laves to mapper: en mappe med
billeder af ure og en mappe med billeder som ikke er af ure. Størrelsen
og navnene på selve billederne er ligemeget. Det er dog vigtigt at
mapperne hedder det samme som vektorelementerne i variablen
“klasse_navne”

``` r
library(keras)
library(tensorflow)

klasse_navne <- c("ur", "ikke-ur")
hovedmappe_sti <- "C:/Users/marti/Documents/Git/EK/2. sem/Deep Learning og NLP/DL_og_NLP_git/Eksamensscripts/billeder/"
```

**Generatoren**

Gør overordnet tre ting:

- Skalerer alle værdier individuelt ved at dividere med 255 som er
  maksværdien i RGB-farveskalen. Dette gør modellen hurtigere da
  computeren foretrækker små tal mellem 0 og 1.

- Laver justeringer til den relativt lille datamængde af billeder vi
  har. Derfor den justeringer billederne: roterer, spejlvender, zoomer,
  flytter sidevers.

- Tager 20% af billederne fra som bruges til validering.

``` r
datagen <- image_data_generator(
  rescale = 1/255,          
  rotation_range = 20,
  width_shift_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  validation_split = 0.2
)
```

**PUMPE 1: Træningsdata (80% af billederne)**

- Her “pumpes” billederne ind fra den specificerede sti/directory fra
  PC.
- generatoren som blev defineret ovenfor behandler i pumpen alle
  billeder.
- Størrelsen på billederne bliver komprimeret til 75 x 100.
- Farvede billeder gøres sorthvide, for at minimere kompleksitet,
  behandlingstid og for at skærpe modellens præcision
- Der defineres at der er to klasser som der skelnes imellem og at det
  tager udgangspunkt i variablen “klasse_navne”
- Sparse betyder at resultatet præsenteres som 0 eller 1, selvom vi godt
  ved at det er ur vs. ikke-ur

``` r
traenings_pumpe <- flow_images_from_directory(
  directory = hovedmappe_sti,
  generator = datagen,
  target_size = c(75, 100), 
  color_mode = "grayscale", 
  classes = klasse_navne,   
  class_mode = "sparse",    
  batch_size = 2,
  subset = "training"
)
```

**PUMPE 2: Valideringsdata (De resterende 20% af billederne)** -
Valideringsbillederne kommer igennem samme type pumpe som
træningsbillederne

``` r
validerings_pumpe <- flow_images_from_directory(
  directory = hovedmappe_sti,
  generator = datagen,
  target_size = c(75, 100), 
  color_mode = "grayscale", 
  classes = klasse_navne,   
  class_mode = "sparse",    
  batch_size = 2,
  subset = "validation"
)
```

**Modellen bygges**

- Modellen betår her af to Convolutional lag.

- I det første lag kigger modellen på 32 mønstre af 3 x 3 pixels.

- I max_pooling “halveres” billedet ved kun at beholde de vigtiste
  karakteristiska <br> videre til næste lag.

- I andet lag kigger modellen nu på 64 mønstre. Da datamængden er
  skrumpet har vi nu overskud <br> til at kunne kigge på flere møsntre.

- Billedet krympes igen

- Flatten skaber en lang streng af tal istedet for en matrix.

- Dropout rate 50% slukker for halvdelen af modellens hjerneceller, så
  er tvunget til at tænke <br> hårdere så den ikke overfitter/lærer
  pixels udenad, men istedet generelle mønstre.

- layer_dense kobler først mønstrene sammen, hvor 64 er antallet af
  neuroner som arbejder. <br> layer_dense er så til sidst tvunget til at
  tage en beslutning om det er et ur eller ej, hvor <br> det bliver
  præsenteret som en sandsynlighed for begge udfald.

``` r
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(75, 100, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")
```

**Kompiler og træn**

- Stop hvis validerings-accuracy (“val_accuracy”) ikke bliver bedre i 10
  runder og sænker farten hvis den sidder fast
- (Vi lader R udregne steps_per_epoch automatisk for at undgå fejl)

``` r
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.0001),
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)
```

- Validering. Hvis accuracy ikke er blevet bedre gennem 10 itterationer,
  så stopper træningen.

``` r
mine_callbacks <- list(
  callback_early_stopping(monitor = "val_accuracy", patience = 10, restore_best_weights = TRUE),
  callback_reduce_lr_on_plateau(monitor = "val_accuracy", factor = 0.5, patience = 4)
)
```

- kører 50 gange hvis den ikke bliver stoppet.
- Vi fodrer den med validerings_pumpen, så den kan dobbelttjekke sit
  arbejde
- plotter

``` r
history <- model %>% fit(
  traenings_pumpe,
  steps_per_epoch = traenings_pumpe$n / traenings_pumpe$batch_size,   
  epochs = 50,
  validation_data = validerings_pumpe,
  validation_steps = validerings_pumpe$n / validerings_pumpe$batch_size,
  callbacks = mine_callbacks
)

plot(history)
```

**Test på nyt/eget billede:**

``` r
test_billede <- "test2" 

img_test <- image_load(paste0(hovedmappe_sti, test_billede, ".jpg"), 
                       target_size = c(75, 100), 
                       color_mode = "grayscale")
img_test_array <- image_to_array(img_test) / 255
billede_til_model <- array_reshape(img_test_array, c(1, 75, 100, 1))

forudsigelse <- model %>% predict(billede_til_model)

par(mfrow=c(1,2)) 
plot(as.raster(img_test_array[,,1]), main = paste0("Billede: ", test_billede, ".jpg"))

barplot(
  as.numeric(forudsigelse), 
  names.arg = klasse_navne, 
  las = 1,                 
  col = "lightgreen", 
  main = "Modellens gæt",
  ylab = "Sandsynlighed",
  ylim = c(0, 1)
)

par(mfrow=c(1,1))
```

## Algoritme: Kat vs. hund

``` r
library(reticulate)
use_python("C:\\Users\\marti\\anaconda3\\envs\\llms-course\\python.exe", required = TRUE)

library(tensorflow)
library(keras)

hovedmappe_sti <- "C:\\Users\\marti\\Documents\\Git\\EK\\2. sem\\Deep Learning og NLP\\DL_og_NLP_git\\Eksamensscripts\\billeder\\"

klasse_navne <- c("kat", "hund")

# -------------------------------------------------------------------
# 1. HENT OG FILTRÉR DATA
# -------------------------------------------------------------------
cifar <- dataset_cifar10()

# Find alle de steder, hvor billedet enten er en kat (3) eller hund (5)
train_idx <- which(cifar$train$y == 3 | cifar$train$y == 5)
test_idx <- which(cifar$test$y == 3 | cifar$test$y == 5)

# Træk kun de billeder og facitter ud (og divider pixels med 255)
x_train <- cifar$train$x[train_idx, , , ] / 255
y_train_cifar <- cifar$train$y[train_idx]

x_test <- cifar$test$x[test_idx, , , ] / 255
y_test_cifar <- cifar$test$y[test_idx]

# Lav vores eget facit: Hvis det var en 3'er (kat), så giv den 0. Ellers giv den 1 (hund).
y_train <- ifelse(y_train_cifar == 3, 0, 1)
y_test <- ifelse(y_test_cifar == 3, 0, 1)


# -------------------------------------------------------------------
# 2. BYG MODELLEN
# -------------------------------------------------------------------
model <- keras_model_sequential() %>%
  
  # CIFAR-10 billeder er altid 32x32 pixels i farver (3)
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", 
                input_shape = c(32, 32, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>% 
  
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax")


# -------------------------------------------------------------------
# 3. KOMPILÉR OG TRÆN
# -------------------------------------------------------------------
model %>% compile(
  optimizer = "adam", # Vi kan bruge standard Adam nu, fordi vi har masser af data!
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

# Træn direkte på vores x_train og brug x_test som validering!
history <- model %>% fit(
  x_train, y_train,
  epochs = 15,          
  batch_size = 64,      # Den kigger nu på 64 billeder ad gangen (stor forbedring!)
  validation_data = list(x_test, y_test)
)

# Se de endelig (og rolige) grafer!
plot(history)


# -------------------------------------------------------------------
# 4. TEST PÅ ET TILFÆLDIGT BILLEDE FRA TEST-SÆTTET
# -------------------------------------------------------------------
# Vælg et tilfældigt billede (fx nummer 42)
test_index <- 70
test_billede <- x_test[test_index, , , ]
sandt_facit <- y_test[test_index] 

billede_til_model <- array_reshape(test_billede, c(1, 32, 32, 3))
img_test_array <- test_billede #kun for at plot virker. ikke statistisk korrekt!

############### EGET BILLEDE ############3
test_billede <- "kat" #test med eget billede

img_test <- image_load(paste0(hovedmappe_sti, test_billede, ".jpg"), 
                       target_size = c(32, 32))

img_test_array <- image_to_array(img_test) / 255
billede_til_model <- array_reshape(img_test_array, c(1, 32, 32, 3))
###############



# Gæt!
forudsigelse <- model %>% predict(billede_til_model)

# Vis plot
par(mfrow = c(1, 2), mar = c(4, 4, 4, 2))
# as.raster virker perfekt her, fordi x_test er formateret rigtigt!
plot(as.raster(img_test_array), main = paste("Sandt facit:", klasse_navne[sandt_facit + 1]))

barplot(
  as.numeric(forudsigelse), 
  names.arg = klasse_navne, 
  las = 1,                 
  col = "lightblue", 
  main = "Modellens gæt",
  ylab = "Sandsynlighed",
  ylim = c(0, 1)
)
par(mfrow=c(1,1))
```

# Pixeludvælgelse

Her kan vi vælge et billede og lave pixelanalyse samt lave clustering på
gråtoneversionen af billedet (gøre RGB til en værdi og derefter clustre)

``` r
library(imager)
library(factoextra)
library(keras)
library(reticulate)
library(tensorflow)


hovedmappe_sti <- "C:/Users/marti/Documents/Git/EK/2. sem/Deep Learning og NLP/DL_og_NLP_git/Eksamensscripts/billeder/"

billeder_fra_folder <- list.files(paste0(hovedmappe_sti, "ikke-ur"), 
                    pattern = "\\.(jpg|png)$", 
                    full.names = TRUE)
```

**Hvilket billede vi vil kigge på**

``` r
print(billeder_fra_folder)
nr <- 10
```

**Billedet**

``` r
billede <- load.image(billeder_fra_folder[nr])

dim(billede)
plot(billede)
```

**Loop**

Udregn gråtoner for hele billedet på én gang. Billedet bliver derefter
fladet ud til en vektor, som kmeans kræver i næste step.

``` r
grayscale_matrix <- (billede[,,1,1]*255 + 
                         billede[,,1,2]*255 + 
                         billede[,,1,3]*255) / 3

pixel_vektor <- as.vector(grayscale_matrix)
```

**Clustering**

K=antal klynger. Efter K-means “samles” billedet igen, og af en eller
anden grund er der sket noget med billedet så det skal spejlvendes.
Dette gøres i loopet. Hvis billedets dimensioner ser forkerte ud, så
prøv at åbne det op i en anden fane og just højde/bredde.

``` r
k <- 2

set.seed(123) 
klynger <- kmeans(pixel_vektor, centers = k)

klynge_matrix <- matrix(klynger$cluster, 
                        nrow = nrow(billede), 
                        ncol = ncol(billede))

clustered_matrix <- matrix(nrow = nrow(klynge_matrix), ncol = 0)
for(i in ncol(klynge_matrix):1){
  clustered_matrix <- cbind(clustered_matrix, klynge_matrix[,i])
}


image(clustered_matrix, col = rainbow(k), axes = FALSE, main = "Clustered Billede")
```
