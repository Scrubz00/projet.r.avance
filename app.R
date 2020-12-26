# Libraries --------------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)

# Preparation des données ------------------------------------------------------

# Lecture des fichiers

conso.inf36 <- read.csv(file = "conso-inf36-reduit.csv", header = TRUE, encoding = "latin1")
conso.sup36 <- read.csv(file = "conso-sup36-reduit.csv", header = TRUE, encoding = "latin1")
prod.region <- read.csv(file = "prod-region-reduit.csv", header = TRUE, encoding = "latin1")

# Transformation des données

# Nom des variables

colnames(conso.inf36) <- c("index", "horodate", "profil", "plage_de_puissance_souscrite",
                           "nb_points_soutirage", "total_energie_soutiree_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

colnames(conso.sup36) <- c("index", "horodate", "profil", "plage_de_puissance_souscrite", "secteur_activite",
                           "nb_points_soutirage", "total_energie_soutiree_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

colnames(prod.region) <- c("index", "horodate", "plage_de_puissance_injection", "filiere_de_production",
                           "nb_points_injection", "total_energie_injectee_wh",
                           "courbe_moyenne_ndeg1_wh", "indice_representativite_courbe_ndeg1",
                           "courbe_moyenne_ndeg2_wh", "indice_representativite_courbe_ndeg2",
                           "courbe_moyenne_ndeg1_ndeg2_wh", "indice_representativite_courbe_ndeg1_ndeg2",
                           "jour_max_du_mois_0_1", "semaine_max_du_mois_0_1")

# Format de la date

conso.inf36 <- conso.inf36 %>% 
    mutate(plage_de_puissance_souscrite = plage_de_puissance_souscrite %>% 
               factor(levels = c("P0: Total <= 36 kVA", "P1: ]0-3] kVA", "P2: ]3-6] kVA", "P3: ]6-9] kVA",
                                 "P4: ]9-12] kVA", "P5: ]12-15] kVA", "P6: ]15-18] kVA", "P7: ]18-24] kVA",
                                 "P8: ]24-30] kVA", "P9: ]30-36] kVA"))) %>% 
    filter(!is.na(plage_de_puissance_souscrite)) %>% 
    mutate(horodate = horodate %>% ymd_hms())

conso.sup36 <- conso.sup36 %>%
    mutate(horodate = horodate %>% ymd_hms())

prod.region$horodate <- prod.region$horodate %>%
    ymd_hms()

# conso.sup36 <- conso.sup36 %>%
#     filter(plage_de_puissance_souscrite != "P0: Total <= 36 kVA")
# 
# conso.inf36 <- conso.inf36 %>%
#     filter(plage_de_puissance_souscrite != "P0: Total <= 36 kVA")

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
        
        uiOutput("choix_graph"),
        
        uiOutput("parametres")
        
    ),
    
    dashboardBody(
        tabItems(
            tabItem("conso-inf36",
                    plotOutput("graph_inf36")),
            tabItem("conso-sup36",
                    h5("onglet 2"),
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
                                           choices = c("Tous les profils", conso.inf36$profil %>% unique() %>% sort())),
                               selectInput("plage_puissance", 
                                           "Plage de puissance souscrite : ", 
                                           choices = conso.inf36$plage_de_puissance_souscrite %>% unique() %>% sort()))
        }
        
        if(input$tab == "conso-sup36"){
            dynamic_ui <- list(selectInput("profil", 
                                           "Profil : ", 
                                           choices = conso.sup36$profil %>% unique() %>% sort()),
                               selectInput("secteur_activite",
                                           "Secteur d'activité : ", 
                                           choices = conso.sup36$secteur_activite %>% unique() %>% sort()),
                               selectInput("plage_puissance",
                                           "Plage de puissance souscrite : ", 
                                           choices = conso.sup36$plage_de_puissance_souscrite %>% unique() %>% sort()))
        }
        
        if(input$tab == "prod"){
            dynamic_ui <- list(selectInput("filiere", 
                                           "Filière : ", 
                                           choices = prod.region$filiere_de_production %>% unique() %>% sort()),
                               selectInput("plage_puissance", 
                                           "Plage de puissance d'injection : ", 
                                           choices = prod.region$plage_de_puissance_injection %>% unique() %>% sort()))
        }
        
        return(dynamic_ui)
    })
    
    output$graph_inf36 <- renderPlot({
        
        data <- conso.inf36 %>%
            filter(horodate <= input$daterange[2]) %>%
            filter(horodate >= input$daterange[1]) %>%
            filter(plage_de_puissance_souscrite == input$plage_puissance)
        
        if(input$profil != "Tous les profils"){
            data <- data %>%
                filter(profil == input$profil)
        }
        
        g <- ggplot(data = data)
        
        if(input$choix_graph == "Totale énergie soutirée (MWh)"){
            g <- g + 
                geom_area(aes(x = horodate, y = total_energie_soutiree_wh / (10^6), fill = profil))
        }
        
        if(input$choix_graph == "Nb points de soutirage"){
            g <- g +
                geom_line(aes(x = horodate, y = nb_points_soutirage, col = profil))
        }
        
        if(input$choix_graph == "Courbes Moyennes (Wh)" && input$profil != "Tous les profils"){
            g <- g +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg1_wh),
                          col = "red") +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg2_wh),
                          col = "blue") +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg1_ndeg2_wh),
                          col = "green")
        }
        
        return(g)
    })
    
    output$graph_sup36 <- renderPlot({

        data <- conso.sup36 %>%
            filter(horodate <= input$daterange[2]) %>%
            filter(horodate >= input$daterange[1]) %>%
            filter(plage_de_puissance_souscrite == input$plage_puissance) %>% 
            filter(secteur_activite == input$secteur_activite)

        if(input$profil != "Tous les profils"){
            data <- data %>%
                filter(profil == input$profil)
        }

        g <- ggplot(data = data)

        if(input$choix_graph == "Totale énergie soutirée (MWh)"){
            g <- g +
                geom_area(data = . %>% group_by(horodate, secteur_activite) %>% summarise(Total = sum(total_energie_soutiree_wh)),
                          aes(x = horodate, y = Total / (10^6), fill = secteur_activite))
        }

        if(input$choix_graph == "Nb points de soutirage"){
            g <- g +
                geom_line(aes(x = horodate, y = nb_points_soutirage, col = profil))
        }

        if(input$choix_graph == "Courbes Moyennes (Wh)" && input$profil != "Tous les profils"){
            g <- g +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg1_wh),
                          col = "red") +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg2_wh),
                          col = "blue") +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg1_ndeg2_wh),
                          col = "green")
        }

        return(g)
    })
    
    output$graph_prod <- renderPlot({

        data <- prod.region %>%
            filter(horodate <= input$daterange[2]) %>%
            filter(horodate >= input$daterange[1]) %>%
            filter(plage_de_puissance_injection == input$plage_puissance)
        
        if(input$filiere == "F0 : Total toutes filières"){
            data <- data %>%
                filter(filiere_de_production != "F0 : Total toutes filières")
        } else{
            data <- data %>% 
                filter(filiere_de_production == input$filiere)
        }
        
        g <- ggplot(data = data)

        if(input$choix_graph == "Totale énergie injectée (MWh)"){
            g <- g + 
                geom_area(aes(x = horodate, y = total_energie_injectee_wh / (10^6), fill = filiere_de_production))
        }
        
        if(input$choix_graph == "Nb points d'injection"){
            g <- g +
                geom_line(aes(x = horodate, y = nb_points_injection, col = filiere_de_production))
        }
        
        if(input$choix_graph == "Courbes Moyennes (Wh)" && input$filiere != "F0 : Total toutes filières"){
            g <- g +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg1_wh),
                          col = "red") +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg2_wh),
                          col = "blue") +
                geom_line(aes(x = horodate, y = courbe_moyenne_ndeg1_ndeg2_wh),
                          col = "green")
        }
        
        return(g)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
