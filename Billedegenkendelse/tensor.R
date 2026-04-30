library(keras)

# 1. Hent CIFAR-10 datasættet
cifar <- dataset_cifar10()


# Split det op i træning og test præcis som sidst
c(c(x_train, y_train), c(x_test, y_test)) %<-% cifar

# Skalér pixels til mellem 0 og 1
x_train <- x_train / 255
x_test <- x_test / 255

# 2. Byg CNN Modellen
model <- keras_model_sequential() %>%
  
  # -- FILTER LAG 1 --
  # Input_shape er nu c(32, 32, 3), fordi billedet er 32x32 pixels med 3 farver.
  # Vi beder den lære 32 forskellige mønstre (f.eks. vandrette og lodrette streger).
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", 
                input_shape = c(32, 32, 3)) %>%
  # Pooling 'krymper' billedet, så modellen fokuserer på de vigtigste træk og kører hurtigere
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  # -- FILTER LAG 2 --
  # Beder modellen sammensætte de simple streger til hjørner og former (64 nye mønstre)
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  # -- DEN KLASSISKE HJERNE --
  # Flader de fundne mønstre ud til en lang række...
  layer_flatten() %>%
  # ... og forsøger at træffe en beslutning ud fra dem
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax") # Softmax fordi vi har 10 kategorier


##################################################################################
#KATEGORIER
##################################################################################
# Definer navnene på de 10 klasser i CIFAR-10 (de ligger kun som tal 0-9 i datasættet)
#klasse_navne <- c('fly', 'bil', 'fugl', 'kat', 'hjort', 'hund', 'frø', 'hest', 'skib', 'lastbil')
klasse_navne <- c("ur", "ikke-ur")



##################################################################################
#KODE TIL TRÆNING
##################################################################################
# 1. Kompilér modellen
# Vi fortæller den, hvordan den skal lære (Adam), og at den skal måle succes i Nøjagtighed (Accuracy)
model %>% compile(
  loss = "sparse_categorical_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

# 2. Start træningen!
print("Begynder træning... Dette kan tage lidt tid!")

history <- model %>% fit(
  x_train, y_train,
  epochs = 10,             # Vi lader den kigge på datasættet 10 gange
  batch_size = 64,         # Den kigger på 64 billeder ad gangen, før den opdaterer sig selv
  validation_split = 0.2   # Vi gemmer 20% af træningsdataen til at dobbelttjekke undervejs
)

# 3. Tegn grafen, når den (endelig) er færdig
plot(history)

#################################################################################
#TEST
#################################################################################
# Vælg et billede fra test-sættet (prøv at ændre tallet mellem 1 og 10.000)
test_index <- 1
test_billede <- x_test[test_index,,, ]
sand_label <- y_test[test_index] + 1 # +1 fordi R tæller fra 1 (1-10 i stedet for 0-9)

# 1. Forbered billedet til modellen (batch på 1)
billede_til_model <- array_reshape(test_billede, c(1, 32, 32, 3))

# 2. Bed modellen om at udregne sandsynligheder
forudsigelse <- model %>% predict(billede_til_model)

# --- VISUALISERING ---
# Del RStudios plot-vindue op i to (1 række, 2 kolonner)
par(mfrow=c(1,2))

# Tegn billedet til venstre
plot(as.raster(test_billede))
title(paste("Sandt facit:", klasse_navne[sand_label]))

# Tegn søjlediagram over modellens forudsigelser til højre
barplot(
  as.numeric(forudsigelse), 
  names.arg = klasse_navne, 
  las = 2,                  # Vender teksten lodret, så klassenavnene kan læses
  col = "lightblue", 
  main = "Modellens Gæt",
  ylab = "Sandsynlighed (0 til 1)",
  ylim = c(0, 1)            # Låser aksen til 100%
)

# Nulstil plot-vinduet bagefter, så fremtidige grafer fylder det hele igen
par(mfrow=c(1,1))
#################################################################################
#MODELLENS ACCURACY
#################################################################################
# Kør den store eksamen
eksamens_resultat <- model %>% evaluate(x_test, y_test)

# Se karakteren (Accuracy vil være et tal som f.eks. 0.65, hvilket betyder 65% rigtige)
print(eksamens_resultat)

###
# 1. Få modellen til at udregne sandsynligheder for alle 10.000 billeder
alle_forudsigelser <- model %>% predict(x_test)

# 2. Find modellens ENDELIGE gæt for hvert billede (det tal med højest sandsynlighed)
# apply-funktionen kigger på hver række og finder positionen med det højeste tal
modellens_gaet <- apply(alle_forudsigelser, 1, which.max) - 1

# 3. y_test er en matrix. Vi gør den til en flad liste af tal, så vi kan sammenligne
sande_facitter <- as.vector(y_test)

# 4. Lav den magiske tabel!
tabel <- table(Sandt_Billede = sande_facitter, Modellens_Gaet = modellens_gaet)

# Skift tallene (0-9) ud med vores rigtige navne for at gøre det læseligt
rownames(tabel) <- klasse_navne
colnames(tabel) <- klasse_navne

print(tabel)

#################################################################################
#EGEN TEST MED BIL,           BIL+NICOLAI
#################################################################################

# 1. Angiv navnet på dit billede
mit_billede_sti <- "C://Users//marti//Documents//Git//EK//2. sem//Deep Learning og NLP//billeder//1.jpg"  # Ændr denne til "bil2.jpg" for at teste det andet

# 2. Indlæs billedet og tving det til at være 32x32 pixels (Keras klarer det svære for os!)
img <- image_load(mit_billede_sti, target_size = c(75, 100))

# 3. Lav billedet om til et format (en matrix af tal), som modellen forstår
img_array <- image_to_array(img)

# 4. Skalér farverne (ligesom vi gjorde med træningsdata, dividerer vi med 255)
img_array <- img_array / 255

# 5. Tilføj "batch" dimensionen, så formatet bliver c(1, 32, 32, 3)
billede_til_model <- array_reshape(img_array, c(1, 75, 100, 3))

# 6. Lad modellen gætte!
forudsigelse <- model %>% predict(billede_til_model)


# --- VIS RESULTATET (Billede og Graf) ---
par(mfrow=c(1,2)) # Del skærmen i to (venstre/højre)

# For at R kan tegne billedet pænt igen, fjerner vi den første dimension (1'tallet)
billede_til_plot <- array_reshape(img_array, c(75, 100, 3))
plot(as.raster(billede_til_plot))
title("Din fil (Krympet til x højde x y bredde)")

# Tegn søjlediagrammet med forudsigelserne
barplot(
  as.numeric(forudsigelse), 
  names.arg = klasse_navne, 
  las = 2, 
  col = "lightgreen", 
  main = "Modellens Gæt",
  ylab = "Sandsynlighed",
  ylim = c(0, 1)
)

par(mfrow=c(1,1)) # Nulstil plot-vinduet til normal visning bagefter


#################################################################################
#BILLEDEOVERSIGT
#################################################################################


# Lav en dataframe med alle 10.000 billeder
df_oversigt <- data.frame(
  Billede_ID = 1:10000,
  Kategori = klasse_navne[y_test + 1] # +1 for at matche R's indeksering
)

# Tjek at den virker (viser de første 6 rækker)
View(df_oversigt)
