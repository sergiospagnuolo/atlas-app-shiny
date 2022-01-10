
##### SERVER

mod_allmedia_server <- function(id, base) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##################################################
      ##### PREPARAR BASE
      
      all_media <- reactive({
        
        main_table <- newsatlasbr::organizations_state(uf = "all") %>%
          filter(eh_jornal == 1) %>%
          select(id, nome_veiculo, fonte, segmento, municipio, codmun, uf, regiao, num_funcionarios, periodicity, ativo, data_inclusao) 
        

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
        
        if (input$uf != "Todas UFs") {
          main_table <- main_table[main_table$uf == input$uf,]
        }
        
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
      
      ##########
      # TABELA PRINCIPAL
      
      output$table <- DT::renderDataTable(DT::datatable({

        # Importa os dados principais e filtra pelas datas do input$date
        main_table <- all_media() 
        
        main_table$ativo <- gsub(1, "Ativo", main_table$ativo)
        main_table$ativo <- gsub(0, "Fechado", main_table$ativo)
        
        
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
        
        if (input$uf != "Todas UFs") {
          main_table <- main_table[main_table$uf == input$uf,]
        }
        
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
      colnames = c('id', 'Nome do Veículo', 'Fonte', 'Segmento', 'Município', 'Cód Mun. (IBGE)', 'UF', 'Região', 'Núm. Funcionários', 'Periodicidade', 'Ativo', 'Data de Atualização'),
      rownames = FALSE,
      # CONFIGURACOES GERAIS DA TABELA
      options = list(
        #language = list(searchPlaceholder = "Busca por palavra-chave...",
        #              zeroRecords = "Não há resultados para a sua busca.",
        #             sSearch = ""),
        pageLength = 100,
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
                            # NORDESTE
                            conditionalPanel(condition = "input.regiao == 'Nordeste'", 
                                             ns = ns,
                            column(2,
                                   selectInput(inputId = ns("uf"),
                                   label = tags$div(icon("map-marker-alt", class = "icons"),
                                        'Unidade Federativa', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por UFs.")),
                       choices = c("Todas UFs",
                         "AL", "BA",	"CE",	"MA",	"PB",	"PE",	"PI",	"RN",	"SE")
                             ))
                             # FECHAR NORDESTE
                             ),
                           # NORTE
                           conditionalPanel(condition = "input.regiao == 'Norte'", 
                                        ns = ns,
                                        column(2,
                                       selectInput(inputId = ns("uf"),
                                                   label = tags$div(icon("map-marker-alt", class = "icons"),
                                                                    'Unidade Federativa', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por UFs.")),
                                       choices = c("Todas UFs",                                                               "AC", "AM",	"AP",	"PA",	"PB",	"RO",	"RR",	"TO")
                                               ))
                                        # FECHAR NORTE
                       ),
                       # CENTRO-OESTE
                       conditionalPanel(condition = "input.regiao == 'Centro-Oeste'", 
                                        ns = ns,
                                        column(2,
                                               selectInput(inputId = ns("uf"),
                                                           label = tags$div(icon("map-marker-alt", class = "icons"),
                                                                            'Unidade Federativa', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por UFs.")),
                                                           choices = c("Todas UFs",                                                               "DF", "GO", "MS",	"MT")
                                               ))
                                        # FECHAR CENTRO-OESTE
                       ),
                       # SUDESTE
                       conditionalPanel(condition = "input.regiao == 'Sudeste'", 
                                        ns = ns,
                                        column(2,
                                               selectInput(inputId = ns("uf"),
                                                           label = tags$div(icon("map-marker-alt", class = "icons"),
                                                                            'Unidade Federativa', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por UFs.")),
                                                           choices = c("Todas UFs",                                                               "ES", "MG", "RJ",	"SP")
                                               ))
                                        # FECHAR SUDESTE
                       ),
                       # SUL
                       conditionalPanel(condition = "input.regiao == 'Sul'", 
                                        ns = ns,
                                        column(2,
                                               selectInput(inputId = ns("uf"),
                                                           label = tags$div(icon("map-marker-alt", class = "icons"),
                                                                            'Unidade Federativa', tags$br(), tags$span(style="font-weight:300;font-size:0.7em;line-height:1.3em", "Filtre por UFs.")),
                                                           choices = c("Todas UFs",                                                               "PR", "RS", "SC")
                                               ))
                                        # FECHAR SUL
                       ),
                   # FECHA CONDITIONAL MAIOR
                   )
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
