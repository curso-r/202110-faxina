library(magrittr)

# {janitor} ---------------------------------------------------------------

# O janitor não tem apenas `janitor::clean_names()`. 
# Existem outras funções úteis que Utilizaremos ao longo dos exemplos.

dados <- tibble::tibble(
  `Nome Zoado açentoado` = 2,
  outro_nome_ZOADO = 4
)
dados %>% 
  janitor::clean_names()


# `janitor::remove_empty()`

# Remove linhas e colunas vazias. Possui os amigos 
# `janitor::remove_empty_cols()` e `janitor::remove_empty_rows()`

da <- tibble::tribble(
  ~a, ~b, ~c,
  NA, 10, NA,
  10, 10, NA,
  NA, NA, NA
)
janitor::remove_empty(da, "rows")
janitor::remove_empty(da, "cols")

# `janitor::get_dupes()` 
# Identifica duplicatas de colunas.

da <- tibble::tribble(
  ~nome, ~papel, ~salario,
  "Athos", "Professor", 10,
  "Athos", "Professor", 20,
  "Fernando", "Professor", 30
)

janitor::get_dupes(da, nome) %>% 
  tibble::view()
janitor::get_dupes(da, nome, papel)
# janitor::get_dupes(da, nome, papel)

# `janitor::compare_df_cols()`

# Compara as colunas de 2 ou mais bases. Útil quando comparamos bases que 
# deveriam ter o mesmo formato. Também compara os tipos.

da_2020 <- tibble::tribble(
  ~ano, ~coluna_velha, ~salario,
  "2020", "olar", 10,
)
da_2021 <- tibble::tribble(
  ~ano, ~coluna_nova, ~salario,
  2021, "olar", 10,
)

da_2020 %>% 
  dplyr::mutate(ano = as.numeric(ano)) %>% 
  dplyr::rename(coluna_nova = coluna_velha) %>% 
  dplyr::bind_rows(da_2021)

janitor::compare_df_cols(da_2020, da_2021) %>% 
  dplyr::filter(da_2020 != da_2021)

dplyr::bind_rows(da_2021)

# `janitor::adorn_totals()`

# Adiciona totais nas linhas ou colunas de uma tabela. Útil para depois de 
# fazer um sumário. Combina com `janitor::tabyl()`, que também pode ser útil 
# para sumários rápidos.

mtcars %>% 
  dplyr::count(cyl) %>% 
  dplyr::mutate(prop = n/sum(n))

mtcars %>% 
  janitor::tabyl(cyl)

sumario <- tibble::tribble(
  ~nome, ~papel, ~n, ~prop,
  "Julio", "Professor", 20, .1,
  "Fernando", "Professor", 40, .2,
  "Athos", "Professor", 60, .3,
  "Athos", "Consultor", 80, .4,
)

# janitor::adorn_totals(sumario, "row")
# install.packages("gtExtras")

# `{tidyr}` e `{dplyr}` ---------------------------------------------------


# As funções a seguir podem te salvar muito código!

# Não esqueça também de usar `across()`, `case_when()`, `separate()` e `unite()` 
# que já foram discutidos no curso de R para ciência de dados II.


# `dplyr::na_if()`
# Substitui por `NA` se bater alguma condição.

vetor <- c("julio", "athos", "fernando", "vazio")

dplyr::if_else(vetor == "vazio", NA_character_, vetor)

dplyr::case_when(
  vetor == "vazio" ~ NA_character_,
  TRUE ~ vetor
)

dplyr::na_if(vetor, "vazio")


## nao funciona, infelizmente :(
# dplyr::na_if(vetor, c("vazio", "athos"))

# `tidyr::replace_na()`
# Preenche `NA`s de um vetor ou de uma base

vetor <- c("julio", "athos", "fernando", NA)
tidyr::replace_na(vetor, "vazio")

da <- tibble::tribble(
  ~col1, ~col2,
  NA, "Julio",
  "Fernando", NA,
  "Julio", "Caio",
  NA, NA
)

da %>% 
  tidyr::replace_na(list(col1 = "vazio", col2 = "NULL"))

da %>% 
  dplyr::mutate(
    dplyr::across(
      c(col1, col2),
      tidyr::replace_na, "vazio"
    )
  )

da %>% 
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      tidyr::replace_na, "vazio"
    )
  )

# `dplyr::coalesce()`
# Pega o primeiro valor não vazio. Útil para joins / correção de base.

da <- tibble::tribble(
  ~col1, ~col2,
  NA, "Julio",
  "Fernando", NA,
  "Julio", "Caio", # cuidado!
  NA, NA
)
da %>% 
  dplyr::mutate(
    col_arrumada = dplyr::coalesce(col1, col2)
  )

## se precisar fazer algo mais complexo
da %>% 
  dplyr::mutate(
    col_arrumada = dplyr::case_when(
      !is.na(col1) & !is.na(col2) ~ paste(col1, col2, sep = ", "),
      TRUE ~ dplyr::coalesce(col1, col2)
    )
  )

mtcars %>% 
  dplyr::group_by(am) %>% 
  dplyr::summarise(
    dplyr::across(
      c(wt, qsec), 
      .fns = list(media = mean, variancia = var),
      .names = "{.fn}_{.col}"
    )
  )


# `tidyr::fill()`
# Preenche `NA`, para baixo ou para cima

da_com_id <- tibble::tribble(
  ~id, ~col1, ~col2,
  1, NA, "Julio",
  1, "Fernando", "Caio",
  2, "Julio", NA, # cuidado!
  2, NA, NA
)
da %>% 
  tidyr::fill(col1, col2)

# com agrupamento
da_com_id %>% 
  dplyr::group_by(id) %>% 
  tidyr::fill(col1, col2)

# sem agrupamento
da_com_id %>% 
  tidyr::fill(col1, col2)

da %>% 
  tidyr::fill(col1, col2, .direction = "down")

# {unheadr}: sugestão do Halian!

dados <- tibble::tribble(
  ~X1, ~X2,
  "nome1", "nome2",
  "valor1", "valor2",
  "valor3", "valor4"
)

dados %>% 
  janitor::row_to_names(1)

## para series temporais (objetos xts)
# {tsibble}

## alternativa para bases com nomes
# tibble::rowid_to_column()

# `tidyr::drop_na()`
# Retira linhas com `NA` da base

na.omit()

da <- tibble::tribble(
  ~col1, ~col2,
  NA, "Julio",
  "Fernando", NA,
  "Julio", "Caio", # cuidado!
  NA, NA
)

# x <- na.omit(as.data.frame(da))
# attributes(x)

da %>% 
  tidyr::drop_na()

da %>% 
  tidyr::drop_na(col1)

# `tidyr::complete()`
# Completa uma base com combinações faltantes

da <- tibble::tribble(
  ~ano, ~categoria, ~valor,
  "2020", "A", 1,
  # aqui tem um vazio implicito
  "2021", "A", 2,
  "2021", "B", 3
)

da %>% 
  tidyr::complete(
    ano, categoria, 
    fill = list(valor = 100)
  )

da %>% 
  tidyr::pivot_wider(names_from = categoria, values_from = valor) %>% 
  tidyr::pivot_longer(c(A, B))


# `tidyr::unnest()`
# Serve para lidar com bases cuja coluna é uma __lista__ de vetores/data.frames

# https://www.tidyverse.org/blog/2019/09/tidyr-1-0-0/#nesting

da <- tibble::tibble(
  coluna_normal = c(1, 2),
  coluna_tabela = tibble::tibble(a = c(3, 4), b = c(5, 6))
)
da$coluna_tabela

da %>% 
  purrr::pluck("coluna_tabela", "a")

names(da)
tidyr::unnest(da, coluna_tabela)
tidyr::unchop(da, coluna_tabela)
tidyr::unnest_wider(da, coluna_tabela)

da <- tibble::tibble(
  coluna_normal = c(1, 2),
  coluna_lista_tabelas = list(
    tibble::tibble(a = c(3, 4), b = c(5, 6)),
    tibble::tibble(a = c(3, 4), b = c(5, 6))
  )
)

da <- tibble::tibble(
  coluna_normal = c(1, 2),
  coluna_tabela = list(
    tibble::tibble(a = c(3), b = c(5)),
    tibble::tibble(a = c(4), b = c(6))
  )
)
da
tidyr::unnest(da, coluna_tabela)


exemplo_pegar_elemento_de_lista <- list(
  a = list(
    1, list(
      c = 5,
      d = 7
    )
  )
)
exemplo_pegar_elemento_de_lista %>% 
  purrr::pluck("a", 2, "c")


# `dplyr::anti_join()`
# Mostra as linhas da base da esquerda que não estão na base da direita. 
# Ideal para arrumar joins de base

da1 <- tibble::tribble(
  ~municipio, ~valor1,
  "Mogi", 1,
  "Osasco", 2
)
da2 <- tibble::tribble(
  ~municipio, ~valor2,
  "Mogi-Mirim", 1,
  "Osasco", 2
)

# lista de dados para arrumar
da1 %>% 
  dplyr::filter(!municipio %in% da2$municipio)

da1 %>% 
  dplyr::anti_join(da2, "municipio")

# agora deu bom!
da1 %>% 
  dplyr::mutate(municipio = dplyr::case_when(
    municipio == "Mogi" ~ "Mogi-Mirim",
    TRUE ~ municipio
  )) %>% 
  dplyr::anti_join(da2, "municipio")


dados_municipio <- tibble::tibble(
  cod_ibge = "353080",
  nome1 = "Mogi",
  nome2 = "Mogi-Mirim",
  nome3 = "Mogi3"
)

da1 %>% 
  dplyr::left_join(dados_municipio, c("municipio" = "nome1")) %>% 
  dplyr::left_join(dados_municipio, c("municipio" = "nome2")) %>% 
  dplyr::left_join(dados_municipio, c("municipio" = "nome3")) %>% 
  dplyr::mutate(
    municipio_correto = dplyr::coalesce(
      cod_ibge.x, cod_ibge.y, cod_ibge 
    )
  ) %>% 
  View()

nomes_colunas <- c("nome1", "nome2", "nome3")

# codigo um pouco mais avançado para fazer varios joins
purrr::reduce(
  nomes_colunas, 
  ~dplyr::left_join(.x, dados_municipio, c("municipio" = .y)),
  .init = da1
)


# fazendo com base de dados externa

da1 %>% 
  dplyr::anti_join(da2, "municipio") %>% 
  dplyr::mutate(coluna_arrumada = NA_character_) %>% 
  writexl::write_xlsx("problemas.xlsx")

de_para <- readxl::read_excel("problemas_revisado.xlsx") %>% 
  dplyr::select(-valor1)

da1 %>% 
  dplyr::left_join(de_para, c("municipio")) %>% 
  dplyr::mutate(
    coluna_arrumada = dplyr::coalesce(coluna_arrumada, municipio)
  ) %>% 
  dplyr::inner_join(da2, c("coluna_arrumada" = "municipio"))



  
