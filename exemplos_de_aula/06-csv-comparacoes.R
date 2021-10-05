library(magrittr)

# leitura de csv e slimilares ---------------------------------------------

path_csv_leitos <- "https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/Leitos/2021-05-03/esus-vepi.LeitoOcupacao.csv"


# cria arquivo temporario
arquivo_temporario <- fs::file_temp("leitos_", ext = ".csv")

# baixa arquivo
httr::GET(
  path_csv_leitos, 
  httr::write_disk(arquivo_temporario, TRUE),
  httr::progress()
)

# alternativa 1: readr::read_csv() ----------------------------------------

dados <- readr::read_csv(
  arquivo_temporario#,
  # guess_max = 
)

tictoc::tic()
names(dados)
tictoc::toc()

tictoc::tic()
dados2 <- dados %>% 
  dplyr::select(cnes, origem, dataNotificacao)
tictoc::toc()

tictoc::tic()
dados2 <- dados %>% 
  dplyr::arrange(cnes)
tictoc::toc()

tictoc::tic()
dados %>% 
  dplyr::count(origem)
tictoc::toc()


# alternativa 2: data.table::fread() --------------------------------------

dados <- data.table::fread(arquivo_temporario)

tictoc::tic()
dados %>% 
  dplyr::count(origem)
tictoc::toc()

# alternativa 3: arrow::read_csv_arrow() ----------------------------------

# vantagens: rapido(?), intuitivo, facilita trabalho com Spark, integração com {dplyr}
# desvantagens: muito novo!
dados <- arrow::read_csv_arrow(arquivo_temporario)

# comparação naive (não é um benchmark!)
tictoc::tic()
dados <- readr::read_csv(arquivo_temporario)
tictoc::toc()

tictoc::tic()
dados <- data.table::fread(arquivo_temporario)
tictoc::toc()

tictoc::tic()
dados <- arrow::read_csv_arrow(arquivo_temporario)
tictoc::toc()



# exportar ----------------------------------------------------------------

# caso você esteja só gerando um novo arquivo
readr::write_csv()
data.table::fwrite()

# passos intermediarios de um processo de faxina
readr::write_rds()

# exportar para pessoas que não usam R
writexl::write_xlsx()

# exportar para sistemas mais avançados
arrow::write_parquet()
arrow::write_feather()

# exportar para outros softwares estatísticos
haven::write_dta()
