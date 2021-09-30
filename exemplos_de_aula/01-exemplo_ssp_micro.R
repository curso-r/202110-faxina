# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)

# Ler dados ---------------------------------------------------------------

# dados obtidos aqui http://www.ssp.sp.gov.br/transparenciassp/Consulta.aspx

base_bruta <- readxl::read_excel("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls")
# infelizmente não funciona

# mas, depois de ler como texto ficou mais claro:

# base_bruta <- read.delim("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls", sep = "\t", header = T, stringsAsFactors = F)

base_bruta <- read.delim("dados/DadosBO_2021_3(ROUBO DE VEÍCULOS))_completa.xls",
                         fileEncoding = "UTF-16LE", sep = "\t",
                         header = T, stringsAsFactors = F) %>%
  tibble::as_tibble()

#base_bruta <- read.delim("dados/DadosBO_2021_3(ROUBO DE CELULAR).xls", fileEncoding = "UTF-16LE", sep = "\t", header = T, stringsAsFactors = F)

# Experimentos ------------------------------------------------------------

# Nessa seção eu vou rodar várias coisas pra ver e analisar manualmente

# Essa base não é tidy? vamos ver:

base_bruta %>%
  tibble::view()

base_bruta %>%
  dplyr::glimpse()

# Qual é a unidade que a gente deve considerar?

# A unidade é o fato? (roubo de carro)

base_bruta %>%
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME)

# A unidade é o carro roubado? (vítima que sofreu o roubo)

base_bruta %>%
  dplyr::count(ANO_BO, NUM_BO, DELEGACIA_CIRCUNSCRICAO, DELEGACIA_NOME, PLACA_VEICULO)

# Vamos ver o que pegou no BO de nro 12, ano 2021 e placa FIG6050

base_bruta %>%
  dplyr::filter(NUM_BO == 12, ANO_BO == 2021, PLACA_VEICULO == "FIG6050") %>%
  tibble::view()

# Arrumando nomes e placas ------------------------------------------------

base_nomes_arrumados_preenchida <- base_bruta %>%
  # ela passa todos os nomes para minusculo, mas também:
  # - tira espacos e troca por "_"
  # - tira acento
  # - tira simbilos tipo º ª
  # - se tiver repeticao nas colunas, ele coluca um numeral na frente.
  # - duas colunas X viram X1 e X2
  janitor::clean_names() %>%
  dplyr::filter(!is.na(placa_veiculo), placa_veiculo != "",
                !(placa_veiculo %in% c("0000000", "SEMPLAC", "TAMPADA",
                                       "000000", "SEMPLA")))

# vamos verificar se PARECE correto
base_nomes_arrumados_preenchida %>%
  dplyr::count(placa_veiculo) %>%
  tibble::view()

# Separando as bases ------------------------------------------------------

base_nomes_arrumados_preenchida %>%
  tibble::view("Base completa")

## Primeiro vamos separar as três bases e depois juntas pela mesma chave
## chave = num_bo, ano_bo, delegacia_circunscricao, delegacia_nome

# as bases vão ser ocorrencias, crimes, carros
# qual vai ser a estratégia:
# selecionas algumas colunas e no final, removar todas as duplicacoes

ocorrencias <- base_nomes_arrumados_preenchida %>%
  dplyr::select(ano_bo:delegacia_circunscricao) %>%
  dplyr::distinct()

carros <- base_nomes_arrumados_preenchida %>%
  dplyr::select(
    num_bo, ano_bo, delegacia_nome,
    # precisamos as colunas de chave
    placa_veiculo:ano_fabricacao) %>%
  dplyr::distinct(placa_veiculo, .keep_all = TRUE)

crimes_passo1 <- base_nomes_arrumados_preenchida %>%
  dplyr::select(
    num_bo, ano_bo, delegacia_nome,
    # precisamos as colunas de chave
    especie, rubrica, desdobramento
  ) %>%
  dplyr::distinct() %>%
  tidyr::unite(crime_completo, especie, rubrica, desdobramento, sep = " @@@ ")

tibble::view(crimes_passo1)

# o problema a partir do crimes_passo1 é transformar repeticoes indevidas de linhas
# em uma linha só

# para isso temos (pelo menos) três opcoes

## Opção A - Sumarização

crimes_passo2a <- crimes_passo1 %>%
  dplyr::group_by(num_bo, ano_bo, delegacia_nome) %>%
  dplyr::summarise(
    todos_os_crimes = stringr::str_c(crime_completo, collapse = "\n")
  )

## Opção B - pivot_wider

crimes_passo2b <- crimes_passo1 %>%
  dplyr::group_by(num_bo, ano_bo, delegacia_nome) %>%
  dplyr::mutate(
    nomes_das_colunas_largas = stringr::str_c("crime_", 1:n())
  ) %>%
  tidyr::pivot_wider(names_from = nomes_das_colunas_largas, values_from = crime_completo)

## Opção C - nest: transformar as linhas todas em uma tabela e list-column

crimes_passo2c <- crimes_passo1 %>%
  dplyr::group_by(num_bo, ano_bo, delegacia_nome) %>%
  tidyr::nest()

# Construcao da tabela final ----------------------------------------------

base_final_tidy <- carros %>%
  dplyr::left_join(ocorrencias, by = c("num_bo", "ano_bo", "delegacia_nome")) %>%
  dplyr::left_join(crimes_passo2a, by = c("num_bo", "ano_bo", "delegacia_nome")) %>% 
  dplyr::mutate(
    bo_iniciado = lubridate::dmy_hms(bo_iniciado),
    bo_emitido = lubridate::dmy_hms(bo_emitido),
    dataelaboracao = lubridate::dmy_hms(dataelaboracao),
    dataocorrencia = lubridate::dmy(dataocorrencia),
    datacomunicacao = lubridate::dmy(datacomunicacao),
    horaocorrencia = lubridate::hm(horaocorrencia)
  )

# as colunas de datas deveriam ser do tipo, date, POSIXCt (date time) etc. Exemplos:
# base_final_tidy %>% with(bo_emitido) %>% class()
# base_final_tidy %>% with(data_ocorrencia) %>% class()


tibble::view(base_final_tidy)