# Librairies -------------------------------------------------

library(ggplot2)
library(tidyverse)
library(lubridate)
library(here)

# Preparation des données -------------------------------------------------

# Lecture des fichiers

conso.inf36 <- read.csv(file = "conso-inf36.csv", header = TRUE, sep = ";", encoding = "UTF-8")
conso.sup36 <- read.csv(file = "conso-sup36.csv", header = TRUE, sep = ";", encoding = "UTF-8")
prod.region <- read.csv(file = "prod(1).csv", header = TRUE, sep = ";", encoding = "UTF-8")

# Transformation des données

# Conso inf 36

colnames(conso.inf36) <- c("horodate", "profil", "plage_de_puissance_souscrite",
                           "nb_points_soutirage", "total_energie_soutiree_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

conso.inf36 <- conso.inf36 %>% 
  mutate(horodate = horodate %>% str_split("\\+", simplify = TRUE) %>% .[,1]) %>%
  mutate(horodate = horodate %>% ymd_hms())

conso.inf36.juin <- conso.inf36 %>% 
  filter(horodate < as.POSIXct("2020-07-01 02:00:00")) %>% 
  filter(horodate >= as.POSIXct("2020-06-01 02:00:00"))

write_csv(conso.inf36.juin, "conso-inf36-juin.csv")

# Conso sup 36

colnames(conso.sup36) <- c("horodate", "profil", "plage_de_puissance_souscrite", "secteur_activite",
                           "nb_points_soutirage", "total_energie_soutiree_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

conso.sup36 <- conso.sup36 %>% 
  mutate(horodate = horodate %>% str_split("\\+", simplify = TRUE) %>% .[,1] %>% ymd_hms())

conso.sup36.juin <- conso.sup36 %>% 
  filter(horodate < as.POSIXct("2020-07-01 02:00:00")) %>% 
  filter(horodate >= as.POSIXct("2020-06-01 02:00:00"))

write_csv(conso.inf36.juin, "conso-inf36-juin.csv")

# Prod région

colnames(prod.region) <- c("horodate", "plage_de_puissance_injection", "filiere_de_production",
                           "nb_points_injection", "total_energie_injectee_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

prod.region <- prod.region %>% 
  mutate(horodate = horodate %>% str_split("\\+", simplify = TRUE) %>% .[,1] %>% ymd_hms())

prod.region.juin <- prod.region %>% 
  filter(horodate < as.POSIXct("2020-07-01 02:00:00")) %>% 
  filter(horodate >= as.POSIXct("2020-06-01 02:00:00"))

write_csv(conso.inf36.juin, "conso-inf36-juin.csv")

# Onglet 1 : Consommation <= 36 kVA ---------------------------------------

ggplot(data = conso.inf36 %>%
         filter(horodate %>% as.POSIXct() >= "2020-06-23 CEST") %>%
         filter(horodate %>% as.POSIXct() <= "2020-06-25 CEST") %>% 
         filter(plage_de_puissance_souscrite == "P0: Total <= 36 kVA")) +
  
  # Totale énergie soutirée
  
  geom_area(aes(x = horodate, y = total_energie_soutiree_wh / (10^6), fill = profil))
  
  # Nombre de points de soutirage
  
  # geom_line(aes(x = horodate, y = nb_points_soutirage, col = profil))
  
  # Courbe Moyenne
  
  # geom_line(data = . %>% filter(profil == "PRO3"),
  #           aes(x = horodate, y = courbe_moyenne_ndeg1_wh),
  #           col = "red") +
  # geom_line(data = . %>% filter(Profil == "PRO3"),
  #           aes(x = horodate, y = courbe_moyenne_ndeg2_wh),
  #           col = "blue") +
  # geom_line(data = . %>% filter(Profil == "PRO3"),
  #           aes(x = horodate, y = courbe_moyenne_ndeg1_ndeg2_wh),
  #           col = "green")

# Onglet 2 : Consommation >= 36 kVA ---------------------------------------

ggplot(data = conso.sup36 %>%
         filter(horodate %>% as.POSIXct() >= "2020-06-23 CEST") %>%
         filter(horodate %>% as.POSIXct() <= "2020-06-25 CEST") %>% 
         # select(plage_de_puissance_souscrite, profil, secteur_activite, total_energie_soutiree_wh, horodate) %>%
         filter(plage_de_puissance_souscrite == "P1: ]36-120] kVA")) +

# Totale énergie soutirée

  geom_area(data = . %>% group_by(horodate, secteur_activite) %>% summarise(Total = sum(total_energie_soutiree_wh)),
            aes(x = horodate, y = Total / (10^6), fill = secteur_activite))

# Nombre de points de soutirage

# geom_line(aes(x = horodate, y = nb_points_soutirage, col = profil))

# Courbe Moyenne

# geom_line(data = . %>% filter(profil == "PRO3"),
#           aes(x = horodate, y = courbe_moyenne_ndeg1_wh),
#           col = "red") +
# geom_line(data = . %>% filter(profil == "PRO3"),
#           aes(x = horodate, y = courbe_moyenne_ndeg2_wh),
#           col = "blue") +
# geom_line(data = . %>% filter(profil == "PRO3"),
#           aes(x = horodate, y = courbe_moyenne_ndeg1_ndeg2_wh),
#           col = "green")

# Production --------------------------------------------------------------

ggplot(data = prod.region %>%
         filter(horodate %>% as.POSIXct() >= "2020-06-23 CEST") %>%
         filter(horodate %>% as.POSIXct() <= "2020-06-25 CEST") %>% 
         filter(plage_de_puissance_injection == "P0 : Total toutes puissances") %>% 
         filter(filiere_de_production != "F0 : Total toutes filières")) +
  
# Totale énergie injectée
  
  geom_area(aes(x = horodate, y = total_energie_injectee_wh / (10^6), fill = filiere_de_production))

# Nombre de points de soutirage

# geom_line(aes(x = horodate, y = nb_points_injection, col = filiere_de_production))

# Courbe Moyenne

# geom_line(data = . %>% filter(filiere_de_production == "F1 : Thermique non renouvelable"),
#           aes(x = horodate, y = courbe_moyenne_ndeg1_wh),
#           col = "red") +
# geom_line(data = . %>% filter(filiere_de_production == "F1 : Thermique non renouvelable"),
#           aes(x = horodate, y = courbe_moyenne_ndeg2_wh),
#           col = "blue") +
# geom_line(data = . %>% filter(filiere_de_production == "F1 : Thermique non renouvelable"),
#           aes(x = horodate, y = courbe_moyenne_ndeg1_ndeg2_wh),
#           col = "green")
