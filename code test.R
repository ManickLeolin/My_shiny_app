
data(starwars)
head(starwars) 

test <- ggplot(starwars, aes(x = height)) + 
  geom_histogram(
    binwidth = 10,
    fill = "white",
    color = "black"
  ) +
  labs(
    title = "Distribution des tailles sélectionnées"
  ) + 
  geom_density(color = "red", na.rm = TRUE) +  # Gérer les valeurs manquantes
  xlim(0, 750) +  # Optionnel, si tu veux limiter l'axe X
  geom_vline(aes(xintercept = mean(height, na.rm = TRUE)),
             color = "blue", linetype = "dashed", size = 1)

print(test)
summary(starwars$height)

testDensity <- ggplot(starwars %>% filter(!is.na(height)), aes(x = height)) + 
  geom_density(color = "red", na.rm = TRUE) +  # Afficher uniquement la densité
  labs(
    title = "Distribution des tailles sélectionnées"
  ) + 
  xlim(0, 750)  # Limiter l'axe X à la plage raisonnable

print(testDensity)





test_hist <- ggplot(starwars %>% filter(!is.na(height)), aes(x = height)) + 
  geom_histogram(
    binwidth = 10,
    fill = "white",
    color = "black"
  ) +
  labs(
    title = "Distribution des tailles avec Histogramme"
  ) +
  xlim(0, 750)

test_density <- ggplot(starwars %>% filter(!is.na(height)), aes(x = height)) + 
  geom_density(color = "red", na.rm = TRUE) +  # Densité seule
  labs(
    title = "Densité des tailles sélectionnées"
  ) +
  xlim(0, 750)

# Affichage séparé
print(test_hist)
print(test_density)


test <- ggplot(starwars %>% filter(!is.na(height)), aes(x = height)) + 
  geom_histogram(
    binwidth = 10,
    fill = "white",
    color = "black"
  ) +
  geom_density(aes(y = ..density..), color = "red", size = 1) +  # Ajustement de la densité
  labs(
    title = "Distribution des tailles avec densité"
  ) +
  xlim(0, 750) +
  geom_vline(aes(xintercept = mean(height, na.rm = TRUE)),
             color = "blue", linetype = "dashed", size = 1)

print(test)




