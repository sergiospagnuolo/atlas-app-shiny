
##### SERVER

mod_allmedia_server <- function(id, base) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##################################################
      ##### PREPARAR BASE
      
      all_media <- reactive({
        main_table <- main_table <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSQukkKzPoYCfw8y7jyLcKF9bYi_NPlTjqeLeGaoPLOdr7GnBkjN_zoFGLw6GHjPsqLXV6O2ERXJX3a/pub?gid=0&single=true&output=csv", show_col_types = FALSE)
        
        return(main_table)
      })
      
      ##########
      # MENSAGEM COM DESCRICAO DA SELECAO
      
      output$basics <- renderText({
        
        main_table <- all_media()
        
        # Filtros 
        if (input$ativo != "Todos") {
          main_table <- main_table[main_table$ativo == input$ativo,]
        }
        
        if (input$segmento != "Todos") {
          main_table <- main_table[main_table$segmento == input$segmento,]
        }
        
        if (input$regiao != "Todas regiões") {
          main_table <- main_table[main_table$regiao == input$regiao,]
        }
        
        suppressMessages(if (input$uf != "Todas UFs") {
          main_table <- main_table[main_table$uf == input$uf,]
        })
        
        if(input$busca_cidade != ""){
          main_table <- main_table %>% 
            filter(str_detect(municipio, regex(input$busca_cidade, ignore_case = TRUE)))
        }
        
        if(input$busca_veiculo != ""){
          main_table <- main_table %>% 
            filter(str_detect(nome_veiculo, regex(input$busca_veiculo, ignore_case = TRUE)))
        }
        
        n_veiculos <- main_table %>% count()
        
        paste("Seus filtros resultaram em ", n_veiculos$n," veículos.")
        
      })
      
      output$estados = renderUI({
        
        ns <- session$ns
        
        main_table <- all_media()
        
        if (input$regiao != "Todas regiões") {
          main_table <- main_table[main_table$regiao == input$regiao,]
        }
        selectizeInput(inputId = ns("uf"), 
                        #multiple = TRUE,
                    label = tags$div(icon("map-marker-alt", class = "icons"),
                                     'Unidade Federativa', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Escolha um ou mais UFs.")),
                    choices  = c("Todas UFs", as.list(unique(main_table$uf))),
                    selected = "Todas UFs")
      })
      
      ##########
      # TABELA PRINCIPAL
      
      output$table <- DT::renderDataTable(DT::datatable({

        # Importa os dados principais e filtra pelas datas do input$date
        main_table <- all_media() 
        
        main_table$ativo <- gsub(1, "Ativo", main_table$ativo)
        main_table$ativo <- gsub(0, "Fechado", main_table$ativo)
        
        main_table$periodicity <- gsub("another", "outra", main_table$periodicity)
        main_table$periodicity <- gsub("bimonthly", "bimestral", main_table$periodicity)
        main_table$periodicity <- gsub("biweekly", "bissemanal", main_table$periodicity)
        main_table$periodicity <- gsub("continuous", "contínua", main_table$periodicity)
        main_table$periodicity <- gsub("daily", "diária", main_table$periodicity)
        main_table$periodicity <- gsub("fortnightly", "quinzenal", main_table$periodicity)
        main_table$periodicity <- gsub("monthly", "mensal", main_table$periodicity)
        main_table$periodicity <- gsub("undefined", "não definida", main_table$periodicity)
        main_table$periodicity <- gsub("weekly", "semanal", main_table$periodicity)
        
        
        # Filtros 
        if (input$ativo != "Todos") {
          main_table <- main_table[main_table$ativo == input$ativo,]
        }
        
        if (input$segmento != "Todos") {
          main_table <- main_table[main_table$segmento == input$segmento,]
        }
        
        if (input$regiao != "Todas regiões") {
          main_table <- main_table[main_table$regiao == input$regiao,]
        }

        suppressMessages(if (input$uf != "Todas UFs") {
          main_table <- main_table[main_table$uf == input$uf,]
        })
        
        if(input$busca_cidade != ""){
          main_table <- main_table %>% 
            filter(str_detect(municipio, regex(input$busca_cidade, ignore_case = TRUE)))
        }
        
        if(input$busca_veiculo != ""){
          main_table <- main_table %>% 
            filter(str_detect(nome_veiculo, regex(input$busca_veiculo, ignore_case = TRUE)))
        }
        
        #unnest_wider(media_channels, names_repair = 'unique') %>%
        #separate(link, into = c("Site", "Facebook", "Instagram", "Twitter"), sep=", ")
        
        # Gera a tabela principal
        
        main_table
        
      }, escape = FALSE,
      colnames = c('id', 'Nome do Veículo', 'Fonte', 'Segmento', 'Município', 'Cód Mun. (IBGE)', 'UF', 'Região', 'Núm. Funcionários', 'Periodicidade', 'Ativo', 'Data de Atualização', 'Data de Fechamento'),
      extensions = c("Buttons"), 
      rownames = FALSE,
      # CONFIGURACOES GERAIS DA TABELA
      options = list(
        #language = list(searchPlaceholder = "Busca por palavra-chave...",
        #              zeroRecords = "Não há resultados para a sua busca.",
        #             sSearch = ""),
        pageLength = 50,
        lengthMenu = list( c(10, 50, -1) # declare values
                           , c(10, 50, "Todos") # declare titles
        ),
        dom = 'Blrtip',
        buttons = 
          list('copy', list(
            extend = 'collection',
            buttons = c('csv', 'excel'),
            text = 'Baixe os dados',
            exportOptions = list(
              modifiers = list(selected = TRUE)
            )
          )),
        language = list(
          lengthMenu = "Mostrando _MENU_ registros",
          buttons = list(copy = 'Copiar tabela', 
                         copyTitle = "Tabela copiada com sucesso", 
                         copySuccess = "%d linhas copiadas"),
          info = 'FONTE: Atlas da Notícia',
          paginate = list(previous = 'Anterior', `next` = 'Próxima'),
          processing = "CARREGANDO OS DADOS...",
          emptyTable = "INICIE SUA BUSCA POR TERMOS DE PESQUISA",
          zeroRecords = "SEM RESULTADOS PARA MOSTRAR, FAÇA NOVA BUSCA"),
        info = TRUE
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

mod_allmedia_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    tags$div(
    ### TABELA PRINCIPAL
    column(12,
           
           # BUSCA POR STATUS
           column(2,
                  selectInput(inputId = ns("ativo"),
                              label = tags$div(icon("map-marker-alt", class = "icons"),
                                               'Ativo ou fechado', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Selecione o status do veículo")),
                              choices = c("Todos",
                                          "Ativo",
                                          "Fechado")
                  )),
           
           # BUSCA POR SEGMENTO
           column(2,
                  selectInput(inputId = ns("segmento"),
                             label = tags$div(icon("photo-video", class = "icons"),
                                              'Segmento', tags$br(), tags$span(style="font-weight:300;font-size:0.8em;line-height:1.3em", "Filtre por tipo de mídia.")),
                                choices = c("Todos",
                                           "Online" = "online",
                                           "Impresso" = "impresso",
                                           "Rádio" = "radio",
                                           "TV" = "televisao")
                       )),
           
           # SELECIONA REGIÃO
           column(2, selectInput(inputId = ns("regiao"),
                                 label = tags$div(icon("map-marker", class = "icons"),
                                                  'Região', tags$br(), tags$span(style="font-weight:300;font-size:0.8em;line-height:1.3em", "Filtre por região.")),
                                 choices = c("Todas regiões",
                                   "Centro-Oeste",
                                   "Nordeste",
                                   "Norte",
                                   "Sudeste",
                                   "Sul")
                            )
                  
           ),
           
           # SELECIONA UNIDADES FEDERATIVAS
           conditionalPanel(condition = "input.regiao != 'Todas regiões'",
                             ns = ns,
             column(2,uiOutput(ns('estados'))))
           ),
           
           #BUSCADORES TEXTUAIS
           column(12,
           column(4, searchInput(inputId = ns("busca_veiculo"),
                                 btnSearch = icon("search"),
                                 btnReset = icon("remove"),
                                 resetValue = "",
                                 label = tags$div(icon("search", class = "icons"), 'Busca por veículo', tags$br(), tags$span(style="font-weight:300;font-size:0.8em;line-height:1.3em", "Note que a busca é sensível a acentos")),
                                 value = "",
                                 width = "100%",
                                 placeholder = "Procure veículo por nome"
           ) 
           
           ),
           # BUSCA POR CIDADES
          column(4, conditionalPanel(condition = "input.uf != 'Todas UFs'", 
                            ns = ns,
                            searchInput(inputId = ns("busca_cidade"),
                                 btnSearch = icon("search"),
                                 btnReset = icon("remove"),
                                 resetValue = "",
                                 label = tags$div(icon("search", class = "icons"), 'Busca por município', tags$br(), tags$span(style="font-weight:300;font-size:0.8em;line-height:1.3em", "Note que a busca é sensível a acentos")),
                                 value = "",
                                 width = "100%",
                                 placeholder = "Procure um município por nome"
           ) 
           ))),
           # results
           column(12,tags$p(include_spinner_small(ns("basics")))),
           column(12,include_spinner_tables(ns("table")))
    
    # Fecha TagList
  )
)
  
  # Fecha UI
}
