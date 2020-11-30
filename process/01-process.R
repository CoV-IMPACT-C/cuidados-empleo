# Code 1: Process ENE -----------------------------------------------------

# 1. Cargar librerias -----------------------------------------------------
pacman::p_load(tidyverse)

# 2. Cargar base de datos -------------------------------------------------

ene <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-01-def.dta?sfvrsn=3dabbf2d_11&amp;download=true")

#"https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-01-def.dta?sfvrsn=3dabbf2d_11&amp;download=true"
url <- "https://www.ine.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion"
web <- read_html(url)

# Extraer CSS
css_base <- ".widArchNavTituloArchivos.widArchNavArchivoDescarga"

base_html <- html_nodes(web, css_base)
