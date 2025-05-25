# Paquetes necesarios
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, readxl, lubridate, stringr, hms, writexl)

# --- CONFIGURACIÓN GENERAL ---

urls <- list(
  "2020" = "https://www.fgjcdmx.gob.mx/storage/app/media/Transparencia/Incidencia%20Delictiva%202020/INCIDENCIA%20DELICTIVA%202020%201B%20VICTIMAS%20ALTO%20IMPACTO.xlsx",
  "2021" = "https://www.fgjcdmx.gob.mx/storage/app/media/Transparencia/Incidencia%20delictiva%202021/Enero%20Noviembre%202021/1b-victimas-alto-impacto-2.xlsx",
  "2022" = "https://www.fgjcdmx.gob.mx/storage/app/media/Transparencia/Incidencia%20Delictiva%202022/Diciembre/1B%20VICTIMAS%20ALTO%20IMPACTO.xlsx",
  "2023" = "https://www.fgjcdmx.gob.mx/storage/app/media/Transparencia/Incidencia%20Delictiva%202023/diciembre/1B%20VICTIMAS%20ALTO%20IMPACTO.xlsx",
  "2024" = "https://www.fgjcdmx.gob.mx/storage/app/media/Transparencia/Incidencia%20Delictiva%202024/Diciembre/1B%20VICTIMAS%20ALTO%20IMPACTO.xlsx",
  "2025" = "https://www.fgjcdmx.gob.mx/storage/app/media/Transparencia/2025/Febrero%20Marzo%202025/1B%20VICTIMAS%20ALTO%20IMPACTO.xlsx"
)

destination <- "microdatos"
dir.create(destination, showWarnings = FALSE)

# --- FUNCIONES ---

descargar_archivos <- function(urls, carpeta = "microdatos") {
  for (anio in names(urls)) {
    tryCatch({
      archivo <- file.path(carpeta, paste0("delitos_", anio, ".xlsx"))
      download.file(urls[[anio]], destfile = archivo, mode = "wb")
      message(paste("Descargado:", archivo))
      datos <- read_excel(archivo, skip = 1) %>%
        mutate(across(everything(), as.character))
      assign(paste0("delitos_", anio), datos, envir = .GlobalEnv)
    }, error = function(e) {
      warning(paste("Error en el año", anio, ":", conditionMessage(e)))
    })
  }
}

unir_y_limpiar_datos <- function(anios = 2020:2025) {
  bind_rows(lapply(anios, function(anio) {
    nombre <- paste0("delitos_", anio)
    if (exists(nombre)) {
      get(nombre) %>%
        select(-any_of(c("ID_AP", "CT - INICIO AP", "TIPO IMPACTO"))) %>%
        mutate(
          año = as.integer(anio),
          `FECHA DE INICIO` = parse_date_time(`FECHA DE INICIO`,
                                              orders = c("ymd", "dmy", "mdy", "Ymd", "Y-m-d", "d/m/Y"),
                                              tz = "America/Mexico_City")
        )
    } else NULL
  }))
}

limpiar_variables <- function(df) {
  df %>%
    mutate(
      SEXO = case_when(
        str_to_upper(SEXO) %in% c("MASCULINO", "MASCULINO ") ~ "MASCULINO",
        str_to_upper(SEXO) %in% c("FEMENINO", "FEMENINO ") ~ "FEMENINO",
        TRUE ~ "NO ESPECIFICA"
      ),
      EDAD = str_to_upper(EDAD),
      EDAD = case_when(
        EDAD %in% c("NO ESPECIFICA", "NO SE ESPECIFICA", "NO ESPECIFICADO", "NO ESPECIFICA ", "NO CONTESTÓ") ~ "NO CONTESTÓ",
        str_detect(EDAD, "^[0-9]+$") ~ EDAD,
        TRUE ~ "NO CONTESTÓ"
      ),
      EDAD_RANGO = case_when(
        EDAD == "NO CONTESTÓ" ~ "NO CONTESTÓ",
        as.numeric(EDAD) < 18 ~ "0–17",
        as.numeric(EDAD) >= 18 & as.numeric(EDAD) <= 29 ~ "18–29",
        as.numeric(EDAD) >= 30 & as.numeric(EDAD) <= 44 ~ "30–44",
        as.numeric(EDAD) >= 45 & as.numeric(EDAD) <= 64 ~ "45–64",
        as.numeric(EDAD) >= 65 ~ "65+"
      ),
      EDAD = as.numeric(if_else(EDAD == "NO CONTESTÓ", "0", EDAD)),
      `HORA DE INICIO` = case_when(
        str_detect(`HORA DE INICIO`, "^\\d{1,2}:\\d{2}$") ~ paste0(`HORA DE INICIO`, ":00"),
        str_detect(`HORA DE INICIO`, "^\\d{1,2}:\\d{2}:\\d{2}$") ~ `HORA DE INICIO`,
        TRUE ~ NA_character_
      ),
      `HORA DE INICIO` = as_hms(`HORA DE INICIO`),
      `MODALIDAD - DELITO` = case_when(
        str_detect(`MODALIDAD - DELITO`, "ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA") ~ "CON VIOLENCIA",
        str_detect(`MODALIDAD - DELITO`, "ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA") ~ "SIN VIOLENCIA",
        TRUE ~ `MODALIDAD - DELITO`
      )
    )
}

filtrar_metro <- function(df) {
  df %>% filter(DELITO == "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA")
}

guardar_excel_metro <- function(df, nombre = "microdatos/delitos_metro.xlsx") {
  write_xlsx(df, path = nombre)
}

borrar_excels_no_metro <- function(dir_path = "microdatos", archivo_metro = "delitos_metro.xlsx") {
  archivos <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)
  archivos_a_borrar <- archivos[!grepl(archivo_metro, archivos, ignore.case = TRUE)]
  file.remove(archivos_a_borrar)
  cat("Archivos eliminados del directorio:\n")
  print(basename(archivos_a_borrar))
  
  objetos_en_entorno <- ls(envir = .GlobalEnv)
  objetos_a_borrar <- setdiff(objetos_en_entorno, "delitos_metro")
  rm(list = objetos_a_borrar, envir = .GlobalEnv)
  cat("\n Objetos eliminados del entorno de R:\n")
  print(objetos_a_borrar)
}

# --- EJECUCIÓN ---

descargar_archivos(urls)

delitos_total <- unir_y_limpiar_datos()
delitos_total <- limpiar_variables(delitos_total)
delitos_metro <- filtrar_metro(delitos_total)
guardar_excel_metro(delitos_metro)
borrar_excels_no_metro()

str(delitos_metro)

library(writexl)

write_xlsx(delitos_metro, path = "microdatos/delitos_metro.xlsx")

