
library(tidyverse)
library(udpipe)
library(here)

message("Iniciando procesamiento de texto")

# Creo la carpeta si no existe
output_dir <- here("TP2", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Creando el directorio: ", output_dir)
} else {
  message("El directorio output ya existe.")
}

# Leo los datos que se generaron en el script anterior
data_dir <- here("TP2", "data")
noticias_oea <- read_rds(file.path(data_dir, "tabla_scraping_oea.rds"))
message("Comunicados cargados: ", nrow(noticias_oea))

# Limpio el texto
noticias_oea <- noticias_oea |>
  mutate(
    cuerpo = str_replace_all(cuerpo, "[\\r\\n\\t]+", " "),      # saco saltos de línea
    cuerpo = str_replace_all(cuerpo, "[\\d]+", ""),             # saco números
    cuerpo = str_replace_all(cuerpo, "[[:punct:]]", ""),        # saco puntuación
    cuerpo = str_replace_all(cuerpo, "[[:punct:]]", ""),        # saco caracteres especiales
    cuerpo = str_squish(cuerpo)                                 # saco espacios dobles
  )

# Lematización con udpipe - del tutorial 05 y 06
# La lematización reduce las palabras a su forma base

message("Cargando modelo de español para udpipe...")

m_es <- udpipe_download_model(language = "spanish", overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

message("Lematizando ", nrow(noticias_oea), " comunicados...")

noticias_lemas <- udpipe_annotate(
  modelo_es,
  x      = noticias_oea$cuerpo,
  doc_id = noticias_oea$id
) |>
  as.data.frame() |>
  mutate(id = as.integer(doc_id)) |>
  select(id, lemma, upos)

# Me voy a quedar solo con sustantivos, verbos y adjetivos porque son los que tienen más carga semántica
noticias_lemas <- noticias_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(lemma = str_to_lower(lemma))  # paso todo a minúsculas

message("Tokens tras filtrar por categoría gramatical: ", nrow(noticias_lemas))

# Ahora voy a eliminar las stopwords (palabras que no aportan significado)
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

n_antes <- nrow(noticias_lemas)

noticias_lemas <- noticias_lemas |>
  anti_join(stop_words, by = "lemma") |>
  filter(
    !str_detect(lemma, "^\\d+$"), # elimino tokens que son solo números
    nchar(lemma) > 2              # elimino tokens muy cortos
  )

message("Tokens antes de remover stopwords: ", n_antes)
message("Tokens después de remover stopwords: ", nrow(noticias_lemas))

# Agrego los títulos para no perder el contexto
noticias_lemas <- noticias_lemas |>
  left_join(
    noticias_oea |> select(id, titulo),
    by = "id"
  )

# Guardo el resultado - del tutorial 04
attr(noticias_lemas, "fecha_procesamiento") <- Sys.time()
write_rds(noticias_lemas, file.path(output_dir, "processed_text.rds"))

message("Guardado en: ", file.path(output_dir, "processed_text.rds"))
message("=== Procesamiento finalizado ===")


