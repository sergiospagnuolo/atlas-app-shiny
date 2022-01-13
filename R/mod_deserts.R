
##### SERVER

mod_deserts_server <- function(id, base) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##################################################
      ##### PREPARAR BASE
      
      desert_status <- reactive({
        
        desertos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSQukkKzPoYCfw8y7jyLcKF9bYi_NPlTjqeLeGaoPLOdr7GnBkjN_zoFGLw6GHjPsqLXV6O2ERXJX3a/pub?gid=479991126&single=true&output=csv", show_col_types = FALSE)
        
        quase_desertos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSQukkKzPoYCfw8y7jyLcKF9bYi_NPlTjqeLeGaoPLOdr7GnBkjN_zoFGLw6GHjPsqLXV6O2ERXJX3a/pub?gid=1521924716&single=true&output=csv", show_col_types = FALSE)
        
        nao_desertos <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSQukkKzPoYCfw8y7jyLcKF9bYi_NPlTjqeLeGaoPLOdr7GnBkjN_zoFGLw6GHjPsqLXV6O2ERXJX3a/pub?gid=1030322613&single=true&output=csv", show_col_types = FALSE) %>% filter(qtd_veiculos > 0) 
        
        dados <- list(desertos = desertos, quase_desertos = quase_desertos, nao_desertos = nao_desertos)
        
        return(dados)
        
      })
      
      ##########
      # TABELA PRINCIPAL
      
      output$table <- DT::renderDataTable(DT::datatable({
        

        # Filtros: verificado, RTs e replies
        if (input$status == "Desertos") {
          main_table <- desert_status()[['desertos']]
        }
        if (input$status == "Quase desertos") {
          main_table <- desert_status()[['quase_desertos']]
        }
        if (input$status == "Não desertos") {
          main_table <- desert_status()[['nao_desertos']]
        }
        
        if (input$regiao != "Todas regiões") {
          main_table <- main_table[main_table$regiao == input$regiao,]
        }
        
        
        #unnest_wider(media_channels, names_repair = 'unique') %>%
        #separate(link, into = c("Site", "Facebook", "Instagram", "Twitter"), sep=", ")
        
        # Gera a tabela principal
        main_table
        
      }, escape = FALSE,
         rownames = FALSE,
      # CONFIGURACOES GERAIS DA TABELA
      options = list(
        #language = list(searchPlaceholder = "Busca por palavra-chave...",
        #              zeroRecords = "Não há resultados para a sua busca.",
        #             sSearch = ""),
        pageLength = 1000,
        #dom = "ftipr",
        dom = "tipr",
        language = list(
          #emptyTable = "INICIE SUA BUSCA POR TERMOS DE PESQUISA",
          zeroRecords = "SEM RESULTADOS PARA MOSTRAR, FAÇA NOVA BUSCA"),
        searchHighlight = TRUE,
        info = FALSE,
        lengthMenu = list(c(10, 50, 100, 1000), c('10', '50', '100', '1000'))
      )
      
      # Fecha DT::datatable
      )
      )
      # Fecha modulo
    }
    
    # Fecha server
  )}

###################################################################################################
###################################################################################################
###################################################################################################

### UI

mod_deserts_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    tags$div(
      ### TABELA PRINCIPAL
      column(12,
             column(2,selectInput(inputId = ns("status"),
                         label = tags$div(icon("map-marker-alt", class = "icons"),
                                          'Status do município', tags$br(), tags$span(style="font-weight:300;font-size:0.8em;line-height:1.3em", "Filtre por status")),
                         c("Desertos",
                           "Quase desertos",
                           "Não desertos"))),
             column(2,selectInput(inputId = ns("regiao"),
                                  selected = "Todas regiões",
                         label = tags$div(icon("map-marker", class = "icons"),
                                          'Região', tags$br(), tags$span(style="font-weight:300;font-size:0.8em;line-height:1.3em", "Filtre por região.")),
                         choices = c("Todas regiões",
                                     "Centro-Oeste",
                                     "Nordeste",
                                     "Norte",
                                     "Sudeste",
                                     "Sul"))),
             # results
             include_spinner_tables(ns("table"))
      )
      
      # Fecha TagList
    )
  )
  
  # Fecha UI
}
