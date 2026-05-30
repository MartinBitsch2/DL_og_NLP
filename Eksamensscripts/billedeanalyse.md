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
ligger i det virtuelle miljø “llms-course”:

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
hovedmappe_sti <- "C:/Users/marti/Documents/Git/EK/2. sem/Deep Learning og NLP/Billedegenkendelse/billeder/"
```

**Generatoren gør overordnet tre ting:**

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

``` r
traenings_pumpe <- flow_images_from_directory(
  directory = hovedmappe_sti,
  generator = datagen,
  target_size = c(75, 100), 
  color_mode = "grayscale", 
  classes = klasse_navne,   
  class_mode = "sparse",    
  batch_size = 2,
  subset = "training"       # NYT: Bed den om kun at tage trænings-delen
)
```

``` r
# PUMPE 2: Valideringsdata (De resterende 20% af billederne)
# (Bemærk: Modellen må IKKE øve sig på disse, kun testes på dem)
validerings_pumpe <- flow_images_from_directory(
  directory = hovedmappe_sti,
  generator = datagen,
  target_size = c(75, 100), 
  color_mode = "grayscale", 
  classes = klasse_navne,   
  class_mode = "sparse",    
  batch_size = 2,
  subset = "validation"     # NYT: Bed den om kun at tage validerings-delen
)
```

## Algoritme: Kat vs. hund
