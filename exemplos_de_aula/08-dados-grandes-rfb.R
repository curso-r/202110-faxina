library(magrittr)

# base da receita federal -------------------------------------------------

path <- "dados/csv_dados_qsa_cnpj_23-11-20/cnpj_dados_cadastrais_pj.csv"

## se você tiver menos de 8GB RAM, sugerimos testar com um arquivo menor
# path <- "dados/dados_rfb_small.csv"

## NAO RODE
# NAO da_rfb_fail <- readr::read_csv(path)
# da_rfb_fail <- data.table::fread(path)

# algumas linhas

readr::read_lines(path, n_max = 10)

# vamos tentar ler as primeiras 10000 linhas
dados <- readr::read_delim(path, delim = "#", n_max = 10000)

# pela ciencia
dados <- readr::read_delim(
  path, delim = "#", 
  n_max = 10000, 
  guess_max = 10000
)

# vamos ver como resolver o problema com 10 mil linhas

dados %>% 
  dplyr::count(situacao_cadastral, uf)


# alternativa 1: usando os chunks -----------------------------------------

sumarizar <- function(dados) {
  dados %>% 
    dplyr::count(situacao_cadastral, uf)
}

callback <- readr::DataFrameCallback$new(sumarizar)

dados_chunked_tentativa1 <- readr::read_delim_chunked(
  path,
  delim = "#", 
  callback = callback, 
  guess_max = 10000
)


sumarizar_2args <- function(dados, pos) {
  dados %>% 
    dplyr::count(situacao_cadastral, uf)
}

callback_2args <- readr::DataFrameCallback$new(sumarizar_2args)

dados_chunked_tentativa2 <- readr::read_delim_chunked(
  path,
  delim = "#", 
  callback = callback_2args, 
  guess_max = 10000
)

resultado <- dados_chunked_tentativa2 %>% 
  dplyr::group_by(situacao_cadastral, uf) %>% 
  dplyr::summarise(
    n = sum(n),
    .groups = "drop"
  )


# alternativa 2: vroom ----------------------------------------------------

n_rows <- length(readr::read_lines(path)) - 1

chunk_size <- 1e6

n_chunks <- ceiling(n_rows / chunk_size)

estrutura_dados <- readr::read_delim(
  path,
  n_max = 100, 
  delim = "#",
  show_col_types = FALSE
)

nm <- names(estrutura_dados)

colunas_interesse <- c(
  "situacao_cadastral", "uf"
)

colunas_spec <- rep("_", length(nm))
colunas_spec[nm %in% colunas_interesse] <- "c"
colunas_spec <- stringr::str_c(colunas_spec, collapse = "")

readr::read_delim(
  path,
  n_max = 100,
  delim = "#",
  col_types = colunas_spec
)

sumarizar_manual <- function(skip, path, chunk_size,
                             colunas_spec, colunas) {
  
  message(skip)
  
  dados <- readr::read_delim(
    path,
    delim = "#",
    skip = skip,
    n_max = chunk_size,
    col_types = colunas_spec,
    col_names = FALSE,
    progress = FALSE
  )
  
  dados %>% 
    purrr::set_names(colunas) %>% 
    dplyr::count(situacao_cadastral, uf)
  
}

# vetor de pulos
skips <- seq(0, n_chunks - 1) * chunk_size + 1

dados_chunked <- purrr::map_dfr(
  skips,
  sumarizar_manual,
  path, 
  chunk_size,
  colunas_spec, 
  colunas_interesse
)

resultado <- dados_chunked %>%
  dplyr::group_by(situacao_cadastral, uf) %>%
  dplyr::summarise(n = sum(n), .groups = "drop")
