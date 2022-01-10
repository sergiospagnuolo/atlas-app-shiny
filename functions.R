#########################################################################################
#########################################################################################
###########################                                   ###########################
###########################     FUNCOES DO SCIENCE PULSE      ###########################
###########################                                   ###########################

#########################################################################################

### POPULAR NO PULSE

# Entre os tweets da nossa amostra, quais tiveram o maior n. de RTs (considerando)
# RTs dados pelo universo de usuarios do Twitter? Conta inclui RT de usuarios
# fora do Science Pulse. Contudo, os tweets originais foram postados somente
# por contas monitoradas pela plataforma.

# assuntos_24h <- function(dataset){
#   show_dataset <- dataset
#
#   return(show_dataset)
# }

popular_within_pulse <- function(dataset){
  
  show_dataset <- dataset %>%
    filter(is_retweet == F)
  
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(screen_name) %>%
      arrange(desc(retweet_count), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RTs, eliminadas as repeticoes
      arrange(desc(retweet_count), created_at) %>%
      slice(1:20) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### RT-RATIO

# Entre os tweets autorais da amostra, quais tem a maior proporcao de RT/seguidores?
# Contagem inclui RTs de perfis fora do Pulse. Excluem tweets com menos de 2 RTs,
# para evitar que conteudo irrelevante de usuarios com poucos seguidores apareca.

rising_popularity <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra tweets autorais com +1 RT e calcula o ratio
    filter(is_retweet == F,
           retweet_count > 1) %>%
    mutate(followers_count = as.numeric(followers_count),
           retweet_count   = as.numeric(retweet_count),
           ratio = retweet_count/followers_count)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com maior ratio de cada usuario
      group_by(screen_name) %>%
      arrange(desc(ratio), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com maior ratio, eliminadas as repeticoes
      arrange(desc(ratio), created_at) %>%
      slice(1:20) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### DESEMPENHO EM ALTA

# Identifica os posts com maior numero de interacoes do que seria tradicionalmente
# observado por aquela conta. Isto e, ele considera a soma de RTs e curtidas, com os
# devidos pesos (curtidas valem menos que RTs), de cada post com a media dos ultimos
# posts daquela conta. Tambem existe uma "punicao" para contas com menos seguidores,
# para evitar que o resultado seja artificio de um pequeno numero de tweets.
# Baseado na medida de Overperforming do CrowdTangle.

overperforming <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra tweets autorais com +1 RT
    filter(is_retweet == F,
           retweet_count > 2)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      # Seleciona somente o tweet com maior final_score de cada usuario
      group_by(screen_name) %>%
      arrange(desc(final_score), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com maiores overperform, eliminadas as repeticoes
      arrange(desc(final_score), created_at) %>%
      slice(1:20) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:60%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### SORTEIA TWEETS COM MAIS DE UM RT

# Sorteia, aleatoriamente, 5 tweets com + de 1 RT

sample_more_than_one <- function(dataset){
  
  dataset %>%
    # Filtra tweets autorais com +1 RT
    filter(is_retweet == F,
           retweet_count > 1) %>%
    arrange(desc(retweet_count)) %>%
    # Cria o embed e seleciona somente essa coluna
    mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                         language, '" dir="ltr">',
                         text, '</p>&mdash;',
                         name, '(@',
                         screen_name, ') <a href="https://twitter.com/',
                         screen_name, '/status/',
                         status_id, '">',
                         created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
    )) %>%
    select(text) %>%
    mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
    rename(" " = text)
  
}

#########################################################################################

### USUARIOS MAIS ATIVOS

# Usuarios monitorados pelo Science Pulse com o maior n. de tweets nas ultimas 12h

active_users <- function(dataset){
  
  dataset %>%
    group_by(screen_name) %>%
    count(sort = T) %>%
    ungroup() %>%
    slice(1:5) %>%
    # Cria o embed e seleciona somente essa coluna
    mutate(screen_name = paste0("@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #4b31dd'>", screen_name, "</a>")) %>%
    select(screen_name) %>%
    rename("<i class='fas fa-users'></i>" = screen_name)
  
}

#########################################################################################

### HASHTAGS MAIS USADAS

# Hashtags mais usadas por contas monitoradas pelo Science Pulse nas ultimas 12h

most_hashtags <- function(dataset){
  
  dataset %>%
    mutate(hashtag = toupper(hashtag)) %>%
    filter(hashtag != "NA") %>%
    count(hashtag, sort = T) %>%
    slice(1:5) %>%
    # Cria o embed e seleciona somente essa coluna
    select(hashtag) %>%
    mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #4b31dd'>", hashtag, "</a>")) %>%
    rename("<i class='fas fa-hashtag'></i>" = hashtag)
  
}

#########################################################################################

### TAMBEM POPULARES NO PULSE

also_popular <- function(dataset){
  
  # Seleciona somente tweets autorais
  own_sample_trends <- dataset %>%
    filter(is_retweet == F)
  
  # Aplica um algoritmo de kmeans em 4 grupos, para identificar o segundo grupo com mais RT
  set.seed(12345) # para manter uniformidade entre grupos todas as vezes que usarmos o algoritmo
  # Pega o nome de cada grupo para ficarem em ordem
  centers <- sort(kmeans(as.numeric(own_sample_trends$retweet_count),
                         centers = 4, nstart = 1000)$centers)
  # Aplica o algoritmo com os nomes ordenados dos grupos
  own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count),
                                      centers = centers)$cluster
  
  # Filtra somente o segundo grupo com mais RTs
  own_sample_trends <- own_sample_trends %>%
    filter(cluster == 2)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(own_sample_trends) == 0){
    not_enough_tweets_pt()
    
  } else {
    
    own_sample_trends %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(screen_name) %>%
      arrange(desc(retweet_count), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RTs, eliminadas as repeticoes
      arrange(desc(retweet_count), created_at) %>%
      slice(1:5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### RADAR PULSE

# Amostra aleatoria de 5 tweets entre aqueles bastante populares no Pulse, mas
# que nao chegam ao topo do ranking abosluto de RTs.

pulse_radar <- function(dataset){
  
  # Seleciona somente tweets autorais
  own_sample_trends <- dataset %>%
    filter(is_retweet == F)
  
  # Aplica um algoritmo de kmeans em 4 grupos, para identificar o segundo grupo com mais RT
  set.seed(12345) # para manter uniformidade entre grupos todas as vezes que usarmos o algoritmo
  # Pega o nome de cada grupo para ficarem em ordem
  centers <- sort(kmeans(as.numeric(own_sample_trends$retweet_count),
                         centers = 4, nstart = 1000)$centers)
  # Aplica o algoritmo com os nomes ordenados dos grupos
  own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count),
                                      centers = centers)$cluster
  
  # Filtra somente o segundo grupo com mais RTs
  cluster2 <- own_sample_trends %>%
    filter(cluster == 2)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(cluster2) < 5){
    not_enough_tweets_pt()
    
  } else {
    
    cluster2 %>%
      # Sorteia cinco tweets aleatoriamente
      sample_n(5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
      select(text) %>%
      mutate(text = paste0("<strong>//</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### POPULAR ENTRE CIENTISTAS

# Entre os posts que foram retuitados por membros do Pulse, quais foram mais RTs,
# considerando somente os retuites feitos por contas monitoradas pelo Pulse.
# Eles incluem tweets de qualquer conta do Twitter. Contudo, eles aparecem segundo
# o n. de vezes que o tweet original apareceu em nossa amostra (ja que cada
# RT e uma linha do banco original).

popular_among_scientists <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra tweets nao-autorais com +1 RT
    filter(is_retweet == T) %>%
    group_by(retweet_status_id) %>%
    mutate(numero = n()) %>%
    ungroup() %>%
    filter(numero > 1)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      select(language, text, retweet_name, retweet_screen_name,
             retweet_status_id, retweet_created_at, numero) %>%
      distinct() %>%
      # Seleciona somente o tweet mais vezes RT de cada usuario
      group_by(retweet_screen_name) %>%
      arrange(desc(numero), retweet_status_id) %>%
      slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 mais vezes RT, eliminadas as repeticoes
      arrange(desc(numero), retweet_status_id) %>%
      slice(1:5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           retweet_name, '(@',
                           retweet_screen_name, ') <a href="https://twitter.com/',
                           retweet_screen_name, '/status/',
                           retweet_status_id, '">',
                           retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### OUTROS TWEETS POPULARES

# Entre os tweets que aparecem na amostra, quais foram mais RT por todo o
# universo de usuarios do Twitter? A contagem inclui RTs tanto de usuarios
# monitorados pelo Pulse, como de outras contas. Contudo, eles precisam
# ter sido RT ao menos uma vez por contas do Pulse.

other_popular_tweets <- function(dataset){
  
  show_dataset <- dataset %>%
    # Filtra RTs com 1+ RTs
    filter(is_retweet == T,
           retweet_count > 1)
  
  # Mensagem de nao existirem tweets, se necessario
  if(nrow(show_dataset) == 0) {
    
    not_enough_tweets_pt()
    
  } else {
    
    show_dataset %>%
      select(language, text, retweet_name, retweet_screen_name, retweet_status_id, retweet_created_at, retweet_count) %>%
      distinct() %>%
      # Seleciona somente o tweet com mais RT de cada usuario
      group_by(retweet_screen_name) %>%
      arrange(desc(retweet_count), retweet_status_id) %>%
      slice(1) %>%
      ungroup() %>%
      # Seleciona os 5 com mais RT, eliminadas as repeticoes
      arrange(desc(retweet_count), retweet_status_id) %>%
      slice(1:5) %>%
      # Cria o embed e seleciona somente essa coluna
      mutate(text = paste0('<blockquote class="twitter-tweet" style="width:90%"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           retweet_name, '(@',
                           retweet_screen_name, ') <a href="https://twitter.com/',
                           retweet_screen_name, '/status/',
                           retweet_status_id, '">',
                           retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong>", text)) %>%
      rename(" " = text)
    
  }
  
}

#########################################################################################

### NAO EXISTEM TWEETS SUFICIENTES

# Mensagem que indica nao existirem tweets suficiente para criar aquela coluna

not_enough_tweets_pt <- function(){
  data.frame(variable = "Desculpe, no momento não existem tweets suficientes para esta métrica.\nPor favor, verifique novamente em breve!") %>%
    rename(" " = variable)
}

selecionar_name <- function(mensagem){
  
  grafico_vazio <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 4, y = 40, size = 5,
                      label = mensagem,
                      family = "Barlow") +
    scale_y_continuous(limits = c(0,50)) +
    tema() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.minor.x = element_blank())
  
  return(grafico_vazio)
}

#########################################################################################

### SPINNERS

# Spinners que devem aparecer enquanto dados carregam. Sao diferentes tipos:

# Coluna Grande: spinner circular
include_spinner_large_column <- function(output){
  
  withSpinner(tableOutput(output),
              type = getOption("spinner.type", default = 3),
              color = getOption("spinner.color", default = "#0fb872"),
              size = getOption("spinner.size", default = 1),
              color.background = getOption("spinner.color.background", default = "#eeeeee"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(output))) NULL else "300px")
  
}

# Coluna Fina: spinner retangular
include_spinner_thin_column <- function(output){
  
  withSpinner(tableOutput(output),
              type  = getOption("spinner.type",  default = 1),
              color = getOption("spinner.color", default = "#0fb872"),
              size  = getOption("spinner.size",  default = 1),
              color.background = getOption("spinner.color.background", default = "#0fb872"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(output))) NULL else "300px")
  
}

# Spinner pequeno: textos, circular pequeno
include_spinner_small <- function(output){
  
  withSpinner(textOutput(output),
              type = getOption("spinner.type", default = 7),
              color = getOption("spinner.color", default = "#0fb872"),
              size = getOption("spinner.size", default = 0.4),
              color.background = getOption("spinner.color.background", default = "#0fb872"),
              custom.css = FALSE, proxy.height = "20px")
  
}

# Spinner tabelas: circular grande
include_spinner_tables <- function(output){
  
  withSpinner(DT::dataTableOutput(output),
              type = getOption("spinner.type", default = 6),
              color = getOption("spinner.color", default = "#0fb872"),
              size = getOption("spinner.size", default = 1),
              color.background = getOption("spinner.color.background", default = "#0fb872"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", DT::dataTableOutput(output))) NULL else "300px")
  
}

# Loader tabelas: circular grande
include_loader_tables <- function(output){
  
  withLoader(DT::dataTableOutput(output),
             type = "html",
             loader = "myloader")
  
}

########################################################################################

## TEMA GRAFICO

tema <- function(base_size = 14 , base_family = "Barlow"){(
  
  theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
      panel.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
      text = element_text(colour = "#231f20"),
      
      axis.text = element_text(size = rel(0.8), margin=margin(0,40,0,0)),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.title = element_text(size = rel(0.9), colour = "#999999"),
      
      legend.text = element_text(size=rel(0.9), angle = 0),
      legend.title = element_blank(),
      legend.key = element_rect(fill = "#eeeeee", colour = "#eeeeee", size = 0.5, linetype='dashed'),
      legend.key.width = unit(0.6, "cm"),
      legend.position = "top",
      legend.justification = c(-0.05, 0),
      legend.background = element_blank(),
      legend.direction = "horizontal",
      legend.margin = (margin=margin(0,0,0,0)),
      legend.box = NULL,
      
      panel.border = element_rect(colour = "#eeeeee", fill=NA, size=2),
      panel.grid.major = element_line(colour = "#e4e4e4"),
      panel.grid.minor = element_line(colour = "#e6e6e6"),
      panel.grid.minor.x = element_line(colour = "#e4e4e4"),
      
      plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold", colour = "#231f20"),
      plot.title.position = "plot",
      strip.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
      plot.subtitle = element_text(hjust = 0, margin=margin(0,0,40,0),size = rel(1), lineheight = 1),
      plot.caption = element_text(size = rel(0.75), hjust = 1, margin=margin(20,0,0,0), colour = "#555555", lineheight = 1),
      plot.margin = unit(c(1, 1, 1, 0), "lines")
    )
)
}
