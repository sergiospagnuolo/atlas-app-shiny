#########################################################################################
#########################################################################################
###################                                               #######################
###################         POLITICAL PULSE - SCRIPT APP          #######################
###################                                               #######################

#########################################################################################

### PACOTES
suppressMessages(library(shiny))
suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(rsconnect))
suppressMessages(library(lubridate))
suppressMessages(library(scales))
suppressMessages(library(DT))
suppressMessages(library(shinycssloaders))
suppressMessages(library(stringi))
suppressMessages(library(tidytext))
suppressMessages(library(extrafont))
suppressMessages(library(ggthemes))
suppressMessages(library(sysfonts))
suppressMessages(library(highcharter))
suppressMessages(library(shinyWidgets))

suppressMessages(library(ggplot2))

#suppressMessages(library(fst))
suppressMessages(library(waiter))
suppressMessages(library(newsatlasbr))

waiting_screen <- tagList(
  div(style = "position:fixed;
    top:10%;
     left: 0;
    right: 0;
    text-align: center;
    margin: 0 auto;
    font-family: Lato, sans-serif;
    max-width: 450px;
    z-index:100;",
      spin_fading_circles(),
      h2("Carregando os dados")
  )
)

atualiza <- tagList(
  div(style = "position:fixed;
    top:10%;
     left: 0;
    right: 0;
    text-align: center;
    font-family: Lato, sans-serif;
    margin: 0 auto;
    max-width: 450px;
    z-index:100;",
      spin_fading_circles(),
      h2("Obrigado por acessar o Atlas da Notícia.")
  )
)

### FUNCOES
source("functions.R")

# Sanitize error messages
options(shiny.sanitize.errors = TRUE)

# No need for token on gs4
#gs4_deauth()

#########################################################################################

### UI

ui <- fluidPage(
  use_waiter(), # include dependencies
  waiter_show_on_load(html = waiting_screen, color = "#008596"),
  
  navbarPage(
    title = "",
    
    theme = "custom.css",
    tabPanel(tags$div(icon("newspaper"), " Veículos cadastrados"),
             mod_allmedia_ui("allmedia")),
    tabPanel(tags$div(icon("map-marked-alt"), " Desertos, quase desertos e não-desertos"),
             mod_deserts_ui("deserts"))
  )
)

#########################################################################################

### SERVER

server <- function(input, output, session){
  waiter_hide()
  
  waiter_show( # carregador customizado
    html = waiting_screen, color = "#008596" # utiliza figura de gauge
  )
  
  ##################################
  # CONECTAR AO BANCO DE DADOS
  ## CREDENCIAIS API ATLAS VIA PACOTE NEWSATLASBR
  
  a <- atlas_signin(email = "dados@voltdata.info", password = "senhatokenaberto")
  
  ##################################
  mod_allmedia_server("allmedia", a)
  mod_deserts_server("deserts", a)
  #mod_term_popularity_server("popularity", monitor_db)
  #mod_tweets_server("tweets", monitor_db)
  #mod_profile_server("profiles", monitor_db)
  
  Sys.sleep(1)
  waiter_update(html = atualiza)
  Sys.sleep(2)
  waiter_hide() # esconde carregador
  
}


#########################################################################################

shinyApp(ui = ui, server = server)
