# tabulizer ---------------------------------------------------------------

library(magrittr)

# Tabulizer é uma biblioteca em Java com uma série de heurísticas para
# trabalhar com tabelas em PDF

# O problema de tabelas em PDF é que elas não têm um padrão interno,
# sendo muitas vezes necessário "descobrir" as células da tabela

path_pdf_tabulizer <- "http://portalcovid.riobranco.ac.gov.br/vaccinateds/02-03-202114_32_18.pdf"
browseURL(path_pdf_tabulizer)

# usando {pdftools}: sofrência
texto_pagina1 <- pdftools::pdf_text(path_pdf_tabulizer)[1]
cat(texto_pagina1)
# mesmo com pdf_data() é dificil
dados_pagina1 <- pdftools::pdf_data(path_pdf_tabulizer)[[1]]
dados_pagina1

# com tabulizer 😎

tabela_pagina1 <- tabulizer::extract_tables(
  path_pdf_tabulizer, 
  pages = 1, 
  method = "stream", # existem 2, tente os dois para ver qual funciona melhor
  output = "data.frame"
)

tabela_pagina1_tidy <- tabela_pagina1 %>% 
  dplyr::first() %>% 
  tibble::as_tibble() %>% 
  purrr::set_names(c("nome", "unidade", "data", "grupo")) %>% 
  dplyr::slice(-c(1:2)) %>% 
  dplyr::mutate(data = lubridate::dmy(data))
