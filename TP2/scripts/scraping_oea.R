
library(tidyverse) 
library(rvest) 
library(here) 
library(xml2)


#Iniciando scraping OEA

#Creo la carpeta
data_dir <- here("TP2", "data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message("Creando el directorio: ", data_dir)
} else {
  message("El directorio data ya existe.")
}


# Según el robots.txt de la OEA (https://oas.org/robots.txt) el Crawl-delay es 10 segundos, o sea que tenemos que esperar  10 segundos entre cada pedido para no sobrecargar el servidor
CRAWL_DELAY <- 10

# Voy a crear un vector vacío donde para ir guardando todos los links
meses <- 1:4
total_links <- c()

# Entro a la página de cada mes y junto todos los links
for (mes in meses) {
  url <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
    mes, "&nAnio=2026"
  )
  message("Leyendo mes: ", mes)
  Sys.sleep(CRAWL_DELAY)
  
  pagina <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(pagina)) next
  
  # Extraigo los links de los comunicados de ese mes
  # Selector confirmado con SelectorGadget
  links <- pagina |>
    html_elements("a.itemmenulink") |>
    html_attr("href") |>
    paste0("https://www.oas.org/es/centro_noticias/", .) |>
    unique()
  
  total_links <- c(total_links, links)
  message("Links encontrados en mes ", mes, ": ", length(links))
}

message("Total de comunicados a scrapear: ", length(total_links))

# Entro a cada uno de los links y extraigo el título y el cuerpo 
tabla <- tibble()
id <- 1

for (link in total_links) {
  Sys.sleep(CRAWL_DELAY)
message("Procesando comunicado ", id, " de ", length(total_links), ": ", link)

html <- tryCatch(read_html(link), error = function(e) return(NULL))
if (is.null(html)) {
  message("Error al leer: ", link)
  next
}

# Guardo el HTML con la fecha de descarga - buena práctica del tutorial 04
write_html(
  html,
  here("TP2/data", paste0("oea_raw_", id, "_", Sys.Date(), ".html"))
)

# Extraigo el título y el cuerpo
# Selectores confirmados con SelectorGadget
titulo <- html |>
  html_element("h1") |>
  html_text(trim = TRUE)

cuerpo <- html |>
  html_elements("p") |>
  html_text() |>
  paste(collapse = " ")

tabla <- bind_rows(
  tabla,
  tibble(id = id, titulo = titulo, cuerpo = cuerpo)
)

id <- id + 1
}

# Guardo el resultado en un .rds
df_oea <- tabla
saveRDS(df_oea, here("TP2/data/tabla_scraping_oea.rds"))
message("Se guardó 'tabla_scraping_oea.rds' en /data")
message("=== Scraping finalizado. Total: ", nrow(df_oea), " comunicados ===")
