# Libraries --------------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(stringr)

# Preparation des données ------------------------------------------------------

# Lecture des fichiers

conso.inf36 <- read.csv(file = "conso-inf36-juin.csv", header = TRUE, encoding = "UTF-8")
conso.sup36 <- read.csv(file = "conso-sup36-juin.csv", header = TRUE, encoding = "UTF-8")
prod.region <- read.csv(file = "prod-region-juin.csv", header = TRUE, encoding = "UTF-8")

# Transformation des données

# Nom des variables

colnames(conso.inf36) <- c("horodate", "profil", "plage_de_puissance_souscrite",
                           "nb_points_soutirage", "total_energie_soutiree_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

colnames(conso.sup36) <- c("horodate", "profil", "plage_de_puissance_souscrite", "secteur_activite",
                           "nb_points_soutirage", "total_energie_soutiree_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

colnames(prod.region) <- c("horodate", "plage_de_puissance_injection", "filiere_de_production",
                           "nb_points_injection", "total_energie_injectee_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

# Format de la date

conso.inf36 <- conso.inf36 %>% 
    mutate(plage_de_puissance_souscrite = plage_de_puissance_souscrite %>% 
               factor(levels = c("P1: ]0-3] kVA", "P2: ]3-6] kVA", "P3: ]6-9] kVA",
                                 "P4: ]9-12] kVA", "P5: ]12-15] kVA", "P6: ]15-18] kVA", "P7: ]18-24] kVA",
                                 "P8: ]24-30] kVA", "P9: ]30-36] kVA"))) %>% 
    filter(!is.na(plage_de_puissance_souscrite)) %>%
    mutate(horodate = horodate %>% str_split("\\+", simplify = TRUE) %>% .[,1] %>% ymd_hms()) %>% 
    mutate(date = horodate %>% str_split(pattern = " ", simplify = TRUE) %>% .[,1] %>% ymd())
    # select(-index) %>% 
    

conso.sup36 <- conso.sup36 %>%
    mutate(horodate = horodate %>% str_split("\\+", simplify = TRUE) %>% .[,1] %>% ymd_hms()) %>% 
    mutate(date = horodate %>% str_split(pattern = " ", simplify = TRUE) %>% .[,1] %>% ymd()) %>% 
    mutate(plage_de_puissance_souscrite = plage_de_puissance_souscrite %>% 
               factor(levels = c("P1: ]36-120] kVA", "P2: ]120-250] kVA", "P4: ]250-1000] kVA",
                                 "P5: ]1000-2000] kVA", "P6: > 2000 kVA"))) %>% 
    filter(!is.na(plage_de_puissance_souscrite))

prod.region <- prod.region %>%
    mutate(horodate = horodate %>% str_split("\\+", simplify = TRUE) %>% .[,1] %>% ymd_hms()) %>% 
    mutate(date = horodate %>% str_split(pattern = " ", simplify = TRUE) %>% .[,1] %>% ymd()) %>% 
    filter(plage_de_puissance_injection != "P0 : Total toutes puissances") %>% 
    filter(filiere_de_production != "F0 : Total toutes filières")

# conso.sup36 <- conso.sup36 %>%
#     filter(plage_de_puissance_souscrite != "P0: Total <= 36 kVA")
# 
# conso.inf36 <- conso.inf36 %>%
#     filter(plage_de_puissance_souscrite != "P0: Total <= 36 kVA")

options(scipen = 100)

# UI -------------------------

ui <- dashboardPage(
    
    dashboardHeader(
        title = "Analyse des consommations et productions régionales au pas demi horaire"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            id = "tab",
            menuItem("Consommation <= 36 kVA", tabName = "conso-inf36"),
            menuItem("Consommation >= 36 kVA", tabName = "conso-sup36"),
            menuItem("Production", tabName = "prod")
        ),
        
        dateRangeInput("daterange", "Date :",
                       start = "2020-06-23 00:00:00 UTC",
                       end = "2020-06-24 22:00:00 UTC"),
        
        uiOutput("pas"),
        
        uiOutput("choix_graph"),
        
        uiOutput("parametres")
        
    ),
    
    dashboardBody(
        tabItems(
            tabItem("conso-inf36",
                    plotOutput("graph_inf36")),
            tabItem("conso-sup36",
                    plotOutput("graph_sup36")),
            tabItem("prod",
                    plotOutput("graph_prod"))
        )
    )
    
)

# SERVER --------------------------

server <- function(input, output) {

    output$choix_graph <- renderUI({
        
        choix_possibles <- case_when(
            input$tab == "conso-inf36" ~ c("Totale énergie soutirée (MWh)",
                                           "Nb points de soutirage",
                                           "Courbes Moyennes (Wh)"),
            input$tab == "conso-sup36" ~ c("Totale énergie soutirée (MWh)",
                                           "Nb points de soutirage",
                                           "Courbes Moyennes (Wh)"),
            input$tab == "prod" ~ c("Totale énergie injectée (MWh)",
                                    "Nb points d'injection",
                                    "Courbes Moyennes (Wh)")
        )
        
        selectInput("choix_graph",
                    "Choisissez le graph à afficher : ",
                    choices = choix_possibles,
                    selected = choix_possibles[1])
    })
    
    output$parametres <- renderUI({
        
        dynamic_ui <- NULL
        
        if(input$tab == "conso-inf36"){
            dynamic_ui <- list(selectInput("profil", 
                                           "Profil : ",
                                           choices = conso.inf36$profil %>% unique() %>% sort(),
                                           selected = conso.inf36$profil %>% unique(),
                                           multiple = TRUE),
                               selectInput("plage_puissance", 
                                           "Plage de puissance souscrite : ", 
                                           choices = conso.inf36$plage_de_puissance_souscrite %>% unique() %>% sort(),
                                           selected = conso.inf36$plage_de_puissance_souscrite %>% unique(),
                                           multiple = TRUE))
        }
        
        if(input$tab == "conso-sup36"){
            dynamic_ui <- list(selectInput("profil", 
                                           "Profil : ", 
                                           choices = conso.sup36$profil %>% unique() %>% sort(),
                                           selected = conso.sup36$profil %>% unique(),
                                           multiple = TRUE),
                               selectInput("secteur_activite",
                                           "Secteur d'activité : ", 
                                           choices = conso.sup36$secteur_activite %>% unique() %>% sort(),
                                           selected = conso.sup36$secteur_activite %>% unique(),
                                           multiple = TRUE),
                               selectInput("plage_puissance",
                                           "Plage de puissance souscrite : ", 
                                           choices = conso.sup36$plage_de_puissance_souscrite %>% unique() %>% sort(),
                                           selected = conso.sup36$plage_de_puissance_souscrite %>% unique(),
                                           multiple = TRUE))
        }
        
        if(input$tab == "prod"){
            dynamic_ui <- list(selectInput("filiere", 
                                           "Filière : ", 
                                           choices = prod.region$filiere_de_production %>% unique() %>% sort(),
                                           selected = prod.region$filiere_de_production %>% unique(),
                                           multiple = TRUE),
                               selectInput("plage_puissance", 
                                           "Plage de puissance d'injection : ", 
                                           choices = prod.region$plage_de_puissance_injection %>% unique() %>% sort(),
                                           selected = prod.region$plage_de_puissance_injection %>% unique(),
                                           multiple = TRUE))
        }
        
        return(dynamic_ui)
    })
    
    output$pas <- renderUI({
        radioButtons(inputId = "pas", label = "Pas : ",
                           choices = c("Demi-horaire", "Journalier"),
                           selected = "Demi-horaire")
    })
    
    output$graph_inf36 <- renderPlot({
        
        data <- conso.inf36 %>%
            filter(horodate < input$daterange[2]) %>%
            filter(horodate >= input$daterange[1]) %>%
            filter(plage_de_puissance_souscrite %in% input$plage_puissance) %>% 
            filter(profil %in% input$profil)
        
        g <- ggplot(data = data)
        
        if(input$choix_graph == "Totale énergie soutirée (MWh)"){
            if(input$pas == "Demi-horaire"){
                g <- g + 
                    geom_area(data = . %>% group_by(horodate, profil) %>% summarise(Total = sum(total_energie_soutiree_wh)),
                              aes(x = horodate, y = Total / (10^6), fill = profil))
            } 
            if(input$pas == "Journalier"){
                g <- g + 
                    geom_area(data = . %>% group_by(date, profil) %>% summarise(Total = sum(total_energie_soutiree_wh)),
                              aes(x = date, y = Total / (10^6), fill = profil))
            }
        }
        
        if(input$choix_graph == "Nb points de soutirage"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_line(data = . %>% group_by(horodate, profil) %>% summarise(nb_points = sum(nb_points_soutirage)),
                              aes(x = horodate, y = nb_points, col = profil))
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_line(data = . %>% group_by(date, profil) %>% summarise(nb_points = sum(nb_points_soutirage)),
                              aes(x = date, y = nb_points, col = profil))
            }
        }
        if(input$choix_graph == "Courbes Moyennes (Wh)"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne1 = sum(courbe_moyenne_ndeg1_wh)),
                              aes(x = horodate, y = courbe_moyenne1),
                              col = "red") +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne2 = sum(courbe_moyenne_ndeg2_wh)),
                              aes(x = horodate, y = courbe_moyenne2),
                              col = "blue") +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne12 = sum(courbe_moyenne_ndeg1_ndeg2_wh)),
                              aes(x = horodate, y = courbe_moyenne12),
                              col = "green")
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne1 = sum(courbe_moyenne_ndeg1_wh)),
                              aes(x = date, y = courbe_moyenne1),
                              col = "red") +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne2 = sum(courbe_moyenne_ndeg2_wh)),
                              aes(x = date, y = courbe_moyenne2),
                              col = "blue") +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne12 = sum(courbe_moyenne_ndeg1_ndeg2_wh)),
                              aes(x = date, y = courbe_moyenne12),
                              col = "green")
            }
        }
        
        return(g)
    })
    
    output$graph_sup36 <- renderPlot({
        
        data <- conso.sup36 %>%
            filter(horodate < input$daterange[2]) %>%
            filter(horodate >= input$daterange[1]) %>%
            filter(plage_de_puissance_souscrite %in% input$plage_puissance) %>% 
            filter(secteur_activite %in% input$secteur_activite)
        
        g <- ggplot(data = data)
        
        if(input$choix_graph == "Totale énergie soutirée (MWh)"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_area(data = . %>% group_by(horodate, secteur_activite) %>% summarise(Total = sum(total_energie_soutiree_wh)),
                              aes(x = horodate, y = Total / (10^6), fill = secteur_activite))
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_area(data = . %>% group_by(date, secteur_activite) %>% summarise(Total = sum(total_energie_soutiree_wh)),
                              aes(x = date, y = Total / (10^6), fill = secteur_activite))
            }
        }
        
        if(input$choix_graph == "Nb points de soutirage"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_line(data = . %>% group_by(horodate, profil) %>% summarise(nb_points = sum(nb_points_soutirage)),
                              aes(x = horodate, y = nb_points, col = profil))
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_line(data = . %>% group_by(date, profil) %>% summarise(nb_points = sum(nb_points_soutirage)),
                              aes(x = date, y = nb_points, col = profil))
            }
        }
        
        if(input$choix_graph == "Courbes Moyennes (Wh)"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne1 = sum(courbe_moyenne_ndeg1_wh)),
                              aes(x = horodate, y = courbe_moyenne1),
                              col = "red") +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne2 = sum(courbe_moyenne_ndeg2_wh)),
                              aes(x = horodate, y = courbe_moyenne2),
                              col = "blue") +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne12 = sum(courbe_moyenne_ndeg1_ndeg2_wh)),
                              aes(x = horodate, y = courbe_moyenne12),
                              col = "green")
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne1 = sum(courbe_moyenne_ndeg1_wh)),
                              aes(x = date, y = courbe_moyenne1),
                              col = "red") +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne2 = sum(courbe_moyenne_ndeg2_wh)),
                              aes(x = date, y = courbe_moyenne2),
                              col = "blue") +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne12 = sum(courbe_moyenne_ndeg1_ndeg2_wh)),
                              aes(x = date, y = courbe_moyenne12),
                              col = "green")
            }
        }
        
        return(g)
    })
    
    output$graph_prod <- renderPlot({

        data <- prod.region %>%
            filter(horodate < input$daterange[2]) %>%
            filter(horodate >= input$daterange[1]) %>%
            filter(plage_de_puissance_injection %in% input$plage_puissance) %>% 
            filter(filiere_de_production %in% input$filiere)

        g <- ggplot(data = data)

        if(input$choix_graph == "Totale énergie injectée (MWh)"){
            if(input$pas == "Demi-horaire"){
                g <- g + 
                    geom_area(data = . %>% group_by(horodate, filiere_de_production) %>% summarise(Total = sum(total_energie_injectee_wh)),
                              aes(x = horodate, y = Total / (10^6), fill = filiere_de_production))
            }
            if(input$pas == "Journalier"){
                g <- g + 
                    geom_area(data = . %>% group_by(date, filiere_de_production) %>% summarise(Total = sum(total_energie_injectee_wh)),
                              aes(x = date, y = Total / (10^6), fill = filiere_de_production))
            }
        }
        
        if(input$choix_graph == "Nb points d'injection"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_line(data = . %>% group_by(horodate, filiere_de_production) %>% summarise(nb_points = sum(nb_points_injection)),
                              aes(x = horodate, y = nb_points, col = filiere_de_production))
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_line(data = . %>% group_by(date, filiere_de_production) %>% summarise(nb_points = sum(nb_points_injection)),
                              aes(x = date, y = nb_points, col = filiere_de_production))
            }
        }
        
        if(input$choix_graph == "Courbes Moyennes (Wh)"){
            if(input$pas == "Demi-horaire"){
                g <- g +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne1 = sum(courbe_moyenne_ndeg1_wh)),
                              aes(x = horodate, y = courbe_moyenne1),
                              col = "red") +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne2 = sum(courbe_moyenne_ndeg2_wh)),
                              aes(x = horodate, y = courbe_moyenne2),
                              col = "blue") +
                    geom_line(data = . %>% group_by(horodate) %>% summarise(courbe_moyenne12 = sum(courbe_moyenne_ndeg1_ndeg2_wh)),
                              aes(x = horodate, y = courbe_moyenne12),
                              col = "green")
            }
            if(input$pas == "Journalier"){
                g <- g +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne1 = sum(courbe_moyenne_ndeg1_wh)),
                              aes(x = date, y = courbe_moyenne1),
                              col = "red") +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne2 = sum(courbe_moyenne_ndeg2_wh)),
                              aes(x = date, y = courbe_moyenne2),
                              col = "blue") +
                    geom_line(data = . %>% group_by(date) %>% summarise(courbe_moyenne12 = sum(courbe_moyenne_ndeg1_ndeg2_wh)),
                              aes(x = date, y = courbe_moyenne12),
                              col = "green")
            }
        }
        
        return(g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
