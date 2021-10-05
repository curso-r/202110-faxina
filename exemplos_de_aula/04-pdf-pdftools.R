
# pdftools ----------------------------------------------------------------

library(magrittr)

# O pdftools é uma das melhores ferramentas para trabalhar diretamente com
# arquivos PDF. Vamos ver algumas de suas funcionalidades?

# O pdftools é construido em uma biblioteca em C++ chamada "poppler",
# bastante utilizada para trabalhar com arquivos PDF.

# PDFs digitais -----------------------------------------------------------


path_pdf_digital <- "https://cran.r-project.org/web/packages/pdftools/pdftools.pdf"
browseURL(path_pdf_digital)

httr::GET("https://cran.r-project.org/web/packages/pdftools/pdftools.pdf",
          httr::write_disk("dados/pdftools.pdf"))

# metadados
infos <- pdftools::pdf_info(path_pdf_digital)
infos$keys <- list(infos$keys)
tibble::as_tibble(infos) %>% 
  tibble::glimpse()

# table of contents
toc <- pdftools::pdf_toc(path_pdf_digital)
help_pacote <- toc %>% 
  purrr::pluck("children") %>% 
  purrr::map_chr("title") %>% 
  tibble::enframe()

help_pacote

# texto por pagina
texto <- pdftools::pdf_text(path_pdf_digital)
length(texto)
cat(texto[1])

# posicao de cada elemento
pdftools::pdf_data(path_pdf_digital)[[1]]

# pdfminer

# PDFs digitalizados ------------------------------------------------------

# OCR: Optical Character Recognition
# OCR é feita em 3 passos
# 1. PDF é transformado em imagem
# 2. A imagem é lida no pacote {magick}
# 3. O {magick} chama o {tesseract} para passar OCR

# obs: tesseract é uma ferramenta de OCR open source desenvolvida pela Google.

path_pdf_digitalizado <- "https://jeroen.github.io/images/ocrscan.pdf"
browseURL(path_pdf_digitalizado)

# é possível controlar vários detalhes aqui, como a lingua etc
texto_ocr <- pdftools::pdf_ocr_text(path_pdf_digitalizado)

cat(texto_ocr)

# a partir daqui, a limpeza é feita com {stringr} e criatividade :)