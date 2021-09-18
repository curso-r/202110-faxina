# Pacotes -----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)

# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("janitor")


# Ler dados ---------------------------------------------------------------

base_bruta <- readxl::read_excel("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls")

# readLines("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls", n = 10)

base_bruta <- readr::read_delim("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls", delim = "\t")

base_bruta <- read.delim("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls", sep = "\t", stringsAsFactors = FALSE)

readr::guess_encoding("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls", threshold = 0)

base_bruta <- readr::read_delim("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls",
                                delim = "\t",
                                locale = readr::locale(encoding = "UTF-16LE"))

base_bruta <- read.delim("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls",
                         sep = "\t", stringsAsFactors = FALSE,
                         fileEncoding = "UTF-16LE") %>% 
  as_tibble()

# não funciona: 

# base_bruta <- data.table::fread("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls",
#                                 sep = "\t", encoding = "UTF-16LE")

# Experimentos ------------------------------------------------------------

base_bruta %>% 
  tibble::view()

base_bruta %>% 
  dplyr::glimpse()

# qual é a unidade que eu vou considerar?

# provavelmente carros:

# a unidade é a vítima?

base_bruta %>% 
  count(SEXO)

# quase tudo NA, vou não considerar. Posso até jogar fora essa ocorrência que tem esses
# masculinos

# a unidade é a ocorrência? 

base_bruta %>% 
  count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME)

# a unidade é o carro roubado?

# vamos tentar limpar as repeticoes da base:

base_bruta %>% 
  count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO,
        CIDADE_VEICULO, DESCR_COR_VEICULO, 
        DESCR_MARCA_VEICULO, STATUS, ESPECIE, RUBRICA, DESDOBRAMENTO) %>% 
  filter(n > 1)

base_bruta %>% 
  filter(ANO_BO == 2021, NUM_BO == 1692, DELEGACIA_NOME == "69º D.P. TEOTONIO VILELA") %>% 
  View()

# casos particulares

base_bruta %>% 
  filter(ANO_BO == 2021, NUM_BO == 437009, DELEGACIA_NOME == "DELEGACIA ELETRONICA") %>% 
  tibble::view()

base_bruta %>% 
  filter(ANO_BO == 2021, NUM_BO == 1347, DELEGACIA_NOME == "DELEGACIA ELETRONICA") %>% 
  tibble::view()

base_bruta %>% 
  filter(ANO_BO == 2021, NUM_BO == 1692, DELEGACIA_NOME == "16º D.P. VILA CLEMENTINO") %>% 
  tibble::view()

base_bruta %>% 
  filter(
    NUM_BO == 23, ANO_BO == 2021, DELEGACIA_NOME == "DEIC-5ª DELEGACIA DA DISCCPAT"
  ) %>% 
  tibble::view()

# Arrumar o nome das colunas ----------------------------------------------

base_bruta %>%
  count(PLACA_VEICULO) %>% 
  tibble::view()

base_bruta %>%
  count(ANO_BO, NUM_BO) %>% 
  tibble::view()

base_bruta %>%
  count(ESPECIE, DESDOBRAMENTO, RUBRICA, STATUS) %>% 
  tibble::view()

base_nomes_arrumados_preenchida <- base_bruta %>%
  # tira espacos e troca por "_"
  # tira acento
  # tira simbolos
  # se tiver repeticao de coluna, ela coloca um indice depois da coluna
  # passa tudo pra minuscula
  janitor::clean_names() %>% 
  # remove os carros com id vazio
  filter(!is.na(placa_veiculo), placa_veiculo != "",
         !(placa_veiculo %in% c("SEMPLA", "SEMPLAC", "TAMPADA",
                            "0000000", "000000", "ZZZ0000"))) %>% 
  filter(status == "Consumado")

base_nomes_arrumados_preenchida %>% 
  count(placa_veiculo) %>% 
  tibble::view()

# Separando as bases ------------------------------------------------------

# Estratégia:

# Separar as três bases e depois, monta a base final fazendo joins (left_join)
# a partir da base de unidade que eu escolher (que no caso provavelmente vai ser carro)

# as bases vão ser ocorrencias, crimes, carros
# dentro de cada base a gente vai escolher algumas colunas, remover as duplicaoes

# vai juntar pelas chaves correspondentes

## ocorrencias

ocorrencias <- base_nomes_arrumados_preenchida %>% 
  dplyr::select(ano_bo:delegacia_circunscricao,
                placa_veiculo:ano_fabricacao) %>% 
  dplyr::distinct()

## crimes

carros <- base_nomes_arrumados_preenchida %>% 
  dplyr::select(placa_veiculo:ano_fabricacao) %>% 
  dplyr::distinct()

crimes_passo1 <- base_nomes_arrumados_preenchida %>% 
  dplyr::select(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao,
                especie:desdobramento) %>% 
  dplyr::distinct() %>% 
  tidyr::unite(crime_completo, especie, rubrica, desdobramento, sep = " @@@ ")

# Opção A - sumarização

crimes_passo2a <- crimes_passo1 %>% 
  dplyr::group_by(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao) %>% 
  dplyr::summarise(
    todos_os_crimes = paste0(crime_completo, collapse = ", ")
  ) %>% 
  dplyr::ungroup()

# Opção B - pivot_wider

crimes_passo2b <- crimes_passo1 %>% 
  dplyr::group_by(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao) %>% 
  dplyr::mutate(
    crime_resumido = case_when(
      str_detect(crime_completo, "[Rr]oubo") ~ "Roubo",
      TRUE ~ crime_completo
    ), 
    nome_da_coluna = janitor::make_clean_names(crime_resumido)
  ) %>% 
  tidyr::pivot_wider(names_from = nome_da_coluna, values_from = crime_completo)

# Opção C - nest: transformar as linhas todas da tabela de crimes em uma list-column

crimes_passo2c <- crimes_passo1 %>% 
  dplyr::group_by(num_bo, ano_bo, delegacia_nome, delegacia_circunscricao) %>% 
  tidyr::nest()

# Construção da tabela final ----------------------------------------------

base_final_tidy <- carros %>% 
  dplyr::left_join(ocorrencias, by = c("placa_veiculo", "uf_veiculo",
                                       "cidade_veiculo", "descr_cor_veiculo", 
                                       "descr_marca_veiculo", "ano_fabricacao")) %>% 
  dplyr::left_join(crimes_passo2c)

  