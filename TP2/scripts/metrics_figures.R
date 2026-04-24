
library(tidyverse)
library(tidytext)
library(tm)
library(here)

message("Iniciando cálculo de métricas y figuras")

# Leo los datos procesados 
output_dir <- here("TP2", "output")
tokens <- read_rds(file.path(output_dir, "processed_text.rds"))
message("Tokens cargados: ", nrow(tokens))

# Calculo la frecuencia de cada palabra por cominicado
frecuencia_tokens <- tokens |>
  count(id, lemma, name = "n") |>
  arrange(id)

# Construyo la DTM (cáda fila es un comunicado y cada columna es una palabra, con la frecuencia de esa palabra en ese comunicado)
matriz_dtm <- frecuencia_tokens |>
  cast_dtm(document = id, term = lemma, value = n)

message("DTM construida: ", nrow(matriz_dtm), " documentos x ",
        ncol(matriz_dtm), " términos")

# Elijo cinco términos relevantes para el contexto
terminos_de_interes <- c("democracia", "elección", "derecho", 
                         "seguridad", "misión")

# Filtro la DTM para quedarme solo con esos 5 términos
matriz_filtrada <- matriz_dtm[
  , colnames(matriz_dtm) %in% terminos_de_interes
]

message("Términos encontrados: ",
        paste(colnames(matriz_filtrada), collapse = ", "))

# Sumo las frecuencias de cada uno de los términos en todos los comunicados 
dtm_df <- as.data.frame(as.matrix(matriz_filtrada)) |>
  rownames_to_column(var = "id") |>
  pivot_longer(-id, names_to = "lemma", values_to = "n") |>
  group_by(lemma) |>
  summarise(frecuencia_total = sum(n))

message("Frecuencias calculadas:")
print(dtm_df)

# Genero un gráfico de barras 
grafico <- ggplot(dtm_df, aes(x = lemma, y = frecuencia_total)) +
  geom_col(fill = "steelblue") +
  labs(
    title    = "Frecuencia de términos seleccionados",
    subtitle = "Comunicados de Prensa OEA - Enero a Abril 2026",
    x        = "Término",
    y        = "Frecuencia total",
    caption  = "Fuente: www.oas.org"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Guardo la figura con ggsave - del tutorial 06
ggsave(
  filename = file.path(output_dir, "frecuencia_terminos.png"),
  plot     = grafico,
  width    = 8,
  height   = 6,
  dpi      = 300
)

message("Figura guardada en: ", file.path(output_dir, "frecuencia_terminos.png"))
message("=== Métricas y figuras finalizadas ===")