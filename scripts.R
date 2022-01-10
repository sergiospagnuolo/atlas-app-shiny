library(tidyverse)
library(newsatlasbr)
library(clipr)


atlas_signin(email = "dados@voltdata.info", password = "senhatokenaberto")

dados <- newsatlasbr::organizations_state(uf = "all") 

dados <- dados %>% filter(eh_jornal == 1) %>%
  select(id, nome_veiculo, fonte, segmento, municipio, codmun, uf, regiao, num_funcionarios, periodicity, ativo, data_inclusao)

inativos <- dados %>%
  filter(eh_jornal == 1 & ativo == 0) %>%
  select(id, nome_veiculo, fonte, segmento, municipio, codmun, uf, regiao, num_funcionarios, periodicity, ativo, data_inclusao)

d %>% group_by(ano) %>% count() %>% arrange(desc(n)) %>% drop_na() %>% filter(ano > 2010 & ano < 2021) %>% ggplot() + geom_bar(aes((ano),n), stat = "identity") + labs(title = "Fechamentos de jornais no Brasil", subtitle = "Considera 394 fechamentos (de 644 cadastrados) com data de referência identificada", caption = "Atlas da Notícia")

d <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSN20W9_OwpYV2HTaZN4PCQEzQSRnStTm46dp2qenHjYDIAEQUljmfHUjsdVUlNN-S96-zWw89Ch9oS/pub?gid=0&single=true&output=csv", header = T)



# faz subset ignorando as ultimas colunas, que possuem listas
write_clip(dados[1:25])

d <- dados %>% filter(is.na(email)) %>% select(id,nome_veiculo,municipio,uf,regiao)

desertos <- newsatlasbr::news_deserts()
quase_desertos <- newsatlasbr::almost_deserts()
nao_desertos <- newsatlasbr::get_municipalities()

desertos %>% filter(populacao < 10000) %>% count()

ativos <- dados %>% filter(ativo == 1)

# veículos por região
regioes <- ativos %>% 
  group_by(regiao) %>% 
  count()

# BLOGS por região
blogs <- ativos %>% 
  filter(eh_site_pago == 1 | num_funcionarios == '1 a 5 colaboradores' | num_funcionarios == 'Um colaborador (operação individual/blog)' | str_detect(nome_veiculo, "BLOG", negate = FALSE)) %>%
  # group_by(regiao) %>%
  count()

# combina as duas tabelas acima para comparar
write_clip(regioes %>% 
             inner_join(blogs, by = "regiao", suffix = c(" veiculos", " blogs") ) %>%
             mutate(pct_blogs = round((`n blogs`/`n veiculos`)*100,1)))
s