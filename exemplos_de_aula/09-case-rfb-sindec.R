
library(magrittr)

path <- "dados/crf2019-dados-abertos/CRF2019 Dados Abertos.csv"
da_sindec <- readr::read_csv2(path)

problemas <- readr::problems(da_sindec)

nrow(problemas)

da_sindec <- readr::read_csv2(path, na = c("NULL", "", "NA"))

problemas <- readr::problems(da_sindec)
nrow(problemas)

linhas <- readr::read_lines(path)
linhas_bugadas <- linhas[unique(problemas$row)]
stringr::str_count(linhas_bugadas, ";")

stringr::str_count(linhas[2], ";")

## GATO NET
stringr::str_count(linhas_bugadas, "; ")
stringr::str_count(linhas_bugadas, ";") - stringr::str_count(linhas_bugadas, "; ")

linhas[unique(problemas$row)] <- 
  stringr::str_remove_all(linhas[unique(problemas$row)], "; ")

da_sindec <- linhas %>% 
  # construi o csv original, mas arrumado
  stringr::str_c(collapse = "\n") %>% 
  # le o arquivo a partir do objeto, nao do caminho
  readr::read_csv2(
    na = c("NULL", "", "NA"), 
    col_types = readr::cols(.default = readr::col_character())
  ) %>% 
  janitor::clean_names()

problemas <- readr::problems(da_sindec)
nrow(problemas)


# let the games begin -----------------------------------------------------

# tidy --------------------------------------------------------------------

# (aquecimento) 1. um grafico de barras de faixa etaria e sexo
# (tranquilo) 2. um mapa de % atendida por uf
# (trabalhoso) 3. um grafico de barras de assuntos mais frequentes
# (fogo no parquinho) 3. um grafico de barras de natureza juridica mais frequentes

# 1. um grafico de barras de faixa etaria e sexo

da_sindec %>% 
  dplyr::count(faixa_etaria_consumidor, sexo_consumidor)

# da_sindec %>% 
#   dplyr::count(faixa_etaria_consumidor)

# vamos mexer nessa base
da_sindec_tidy <- da_sindec %>% 
  dplyr::mutate(
    sexo = dplyr::case_when(
      sexo_consumidor %in% c("N") ~ NA_character_,
      TRUE ~ sexo_consumidor
    ),
    faixa = dplyr::na_if(faixa_etaria_consumidor, "Nao Informada")
  )

contagem_sexo_faixa <- da_sindec_tidy %>% 
  dplyr::count(sexo, faixa)

contagem_sexo_faixa %>% 
  tidyr::drop_na(faixa, sexo) %>% 
  ggplot2::ggplot() +
  ggplot2::aes(n, faixa, fill = sexo) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::scale_fill_viridis_d()


# 2. mapa de % atendido por uf --------------------------------------------

da_sindec %>% 
  dplyr::count(atendida)

pct_atendimento_uf <- da_sindec_tidy %>% 
  # alternativa
  # dplyr::group_by(abbrev_state = uf) %>% 
  dplyr::group_by(uf) %>% 
  dplyr::summarise(p_atendida = mean(atendida == "S"))

map_uf <- geobr::read_state()
gg_mapa <- map_uf %>% 
  dplyr::left_join(pct_atendimento_uf, c("abbrev_state" = "uf")) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(
    fill = p_atendida
  ), colour = "black", size = .2)


# 3. assuntos mais frequentes ---------------------------------------------

## OLHAR OS SCRIPTS DO CURSO

# 4. % atendimento por natureza juridica ----------------------------------

da_sindec_empresas <- da_sindec %>% 
  dplyr::select(str_razao_social, numero_cnpj, atendida) %>% 
  dplyr::mutate(
    cnpj = stringr::str_pad(numero_cnpj, 14, "left", "0")
  )
  ## Não é bom pois pode gerar CNPJs incorretos por arredondamento
  # dplyr::mutate(numero_cnpj = as.character(numero_cnpj))

da_sindec %>% 
  dplyr::count(str_razao_social) %>% 
  tibble::view()

da_empresa_cnpj <- da_sindec_empresas %>% 
  dplyr::filter(!is.na(cnpj))

da_empresa_cnpj %>% 
  dplyr::count(str_razao_social, sort = TRUE) %>% 
  print(n = 30)

da_empresa_nm <- da_sindec_empresas %>% 
  dplyr::filter(is.na(cnpj))

# vamos tentar recuperar o cnpj na própria base

da_empresa_nm %>% 
  dplyr::anti_join(da_empresa_cnpj, "str_razao_social")

# vamos tentar fazer uma limpeza da coluna empresa

x <- da_empresa_nm$str_razao_social

limpar_empresa <- function(x) {
  
  # sort(unique(limpo))  
  limpo <- x %>% 
    stringr::str_to_upper() %>% 
    abjutils::rm_accent() %>% 
    stringr::str_remove_all("[^A-Z ]") %>% 
    stringr::str_squish() %>% 
    stringr::str_replace_all(" S A", " SA")
  
  # stringr::str_squish("   aaaaa    aaaaa   ")
  
  dplyr::case_when(
    stringr::str_detect(limpo, "EDITORA ABRIL") ~ "EDITORA ABRIL SA",
    stringr::str_detect(limpo, "SUPERMERCADO DB") ~ "SUPERMERCADO DB LTDA",
    TRUE ~ limpo
  )

}

aux_cnpj_limpo <- da_empresa_cnpj %>% 
  dplyr::mutate(str_razao_social = limpar_empresa(str_razao_social))

da_empresa_nm %>% 
  dplyr::mutate(str_razao_social = limpar_empresa(str_razao_social)) %>% 
  dplyr::anti_join(aux_cnpj_limpo, "str_razao_social")

# vamos usar {fuzzyjoin}!!! Cuidado...

# para mais detalhes, ver o pacote {stringdist}

# area de pesquisa: record linkage
resultado <- da_empresa_nm %>% 
  dplyr::mutate(str_razao_social = limpar_empresa(str_razao_social)) %>% 
  fuzzyjoin::stringdist_anti_join(
    aux_cnpj_limpo,
    "str_razao_social",
    max_dist = 5
  )

resultado <- da_empresa_nm %>% 
  dplyr::mutate(str_razao_social = limpar_empresa(str_razao_social)) %>% 
  fuzzyjoin::stringdist_left_join(
    aux_cnpj_limpo,
    "str_razao_social",
    max_dist = 5,
    distance_col = "dist"
  )

# abjutils::rm_accent("ááá")
# usethis::use_data(da_sindec_tidy)


# AGORA precisamos cruzar com a base da RFB. Vamos fazer em chunks!

filtrar_empresas <- function(dados, pos) {
  dados %>%
    dplyr::semi_join(da_sindec_empresas_arrumado, "cnpj")
}

path_rfb <- "dados/csv_dados_qsa_cnpj_23-11-20/cnpj_dados_cadastrais_pj.csv"

callback <- readr::DataFrameCallback$new(filtrar_empresas)

dados_rfb <- readr::read_delim_chunked(
  path_rfb,
  delim = "#",
  callback,
  guess_max = 100000
)

# legal! agora preciso sumarizar para obter os numeros finais

# tab_nat <- qsacnpj::tab_natureza_juridica
tab_nat <- readr::read_rds("dados/case/tab_nat.rds")

tab_natureza <- da_sindec_empresas_arrumado %>%
  dplyr::inner_join(
    dplyr::select(dados_rfb, cnpj, codigo_natureza_juridica),
    "cnpj"
  ) %>%
  dplyr::mutate(codigo_natureza_juridica = as.character(codigo_natureza_juridica)) %>%
  dplyr::inner_join(
    tab_nat,
    c("codigo_natureza_juridica" = "cod_subclass_natureza_juridica")
  ) %>%
  dplyr::group_by(nm_subclass_natureza_juridica) %>%
  dplyr::summarise(
    n = dplyr::n(),
    p_atendida = mean(atendida == "S")
  ) %>%
  dplyr::filter(n > 10) %>%
  dplyr::arrange(dplyr::desc(p_atendida))
