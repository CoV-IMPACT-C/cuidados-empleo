# Code 1: Process ENE -----------------------------------------------------

# 1. Cargar librerias -----------------------------------------------------
pacman::p_load(tidyverse, xml2)

# 2. Cargar base de datos -------------------------------------------------

ene1 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-01-def.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene2 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-02-efm.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene3 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-03-fma.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene4 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-04-mam.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene5 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-05-amj.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene6 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-06-mjj.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene7 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-07-jja.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene8 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-08-jas.dta?sfvrsn=3dabbf2d_11&amp;download=true")
ene9 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-09-aso.dta?sfvrsn=3dabbf2d_11&amp;download=true")
#ene10 <- haven::read_dta("https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-01-jas.dta?sfvrsn=3dabbf2d_11&amp;download=true")

# conglomerado en enero y en septiembre diferentes
str(ene9$conglomerado)
ene9$conglomerado <- as.character(ene9$conglomerado)
#nivel 

# Merge data bases
ene <- lapply(ls(pattern="ene"), get)
ene <- Reduce(function(...) merge (..., all = T), ene)
#x <- do.call(cbind, ene)
ene <- plyr::rbind.fill(ene)

# Intento de webscrapping -------------------------------------------------
#"https://www.ine.cl/docs/default-source/ocupacion-y-desocupacion/bbdd/2020/stata/ene-2020-01-def.dta?sfvrsn=3dabbf2d_11&amp;download=true"
url <- "https://www.ine.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion"
web <- read_html(url)

# Extraer CSS
css_base <- ".widArchNavArchivoDescarga"

base_html <- html_nodes(web, css_base)
base_html
web

web %>%
  html_nodes("a") %>%
  html_text()

# url %>%
#   xml2::read_html() %>%
#   rvest::html_nodes(".stats-grid") %>%
#   rvest::html_table() %>%
#   purrr::map(~ janitor::clean_names(.)) %>% 
#   purrr::reduce(dplyr::left_join, by = c("x", "athlete_name")) %>%
#   dplyr::filter(!is.na(x))

# 3. Seleccionar variables ------------------------------------------------
ene_proc <- ene %>% 
  select(mes_central, region, r_p_c, tramo_edad, edad, cine,  nivel, nacionalidad, #sociodemograficas
         parentesco, sexo,est_conyugal, proveedor, #sociales importantes
         a1:a8, a6_orig, a6_otro_covid,  #empleo, a6 razones de no trabajo 
         b2, b5, b6, b7a_1, b7a_2, b7a_3, b8, #informalidad b7a y b8
         starts_with("b7b_"), # derechos sociales (ojo con guarderia que es 3 y 4)
         c5,c6, c7, c8, c9, c12, c9_orig, c9_otro_covid, #sobre carga horario y subempleo / c9 razones (ver 6), c12 (5)
        e2, e4, e5_mes, e5_ano, e9, e12, e9_orig, e9_otro_covid, e12_orig, e12_otro_covid, #no buscar empleo , e4 ver 6, e9 (3, 11,14,16), e12 (4, 11)
        fact_cal,
        e24, e24_otro) #motivos renuncia mirar la de 2 (razones de cuidados)  y ver otro x si aparecen ollas comunes

x <- ene_proc %>% 
  mutate(date_text = str_c("1", mes_central, "2020", sep="-"),
         date = ymd_hm(date_text))

# Guardar -----------------------------------------------------------------
save(ene_proc, ene, file = "output/data/ene2020.RData")
