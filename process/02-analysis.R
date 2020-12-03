# Code 2: Analysis --------------------------------------------------------
# 1. Cargar librerias -----------------------------------------------------
pacman::p_load(tidyverse, xml2, lubridate, ggthemes)

# 2. Cargar base de datos -------------------------------------------------
load("output/data/ene2020.RData")

# 3. Theme set ------------------------------------------------------------
theme_set(theme_few() + 
            theme(text = element_text(size = 14),
                  axis.text.x = element_text(angle = -45),
                  legend.position = "bottom",
                  panel.spacing.x = unit(4, "mm")))

options(scipen=999) 

# 4. Graficos ---------------------------------------------------

# Inactividad 
ene_proc %>% 
  group_by(date,sexo, activ) %>%
  filter(!is.na(activ)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         activ = as_factor(activ),
         sexo = as_factor(sexo)) %>%
  ggplot(aes(x = date, y = prop, color = activ)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~sexo) + scale_colour_few() +
  labs(title = "Condición de actividad según sexo (enero 2020 a septiembre 2020)",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=65, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura1.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 32,
       height = 15)


# Razones de inactividad
ene_proc %>% 
  group_by(date,sexo, cae_especifico) %>%
  filter(!is.na(cae_especifico)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         cae_especifico = as_factor(cae_especifico),
         sexo = as_factor(sexo)) %>% 
  filter(cae_especifico %in% c("Razones familiares permanentes (Potencial)","Razones familiares permanentes (Habitual)")) %>%
  ggplot(aes(x = date, y = prop, color = cae_especifico)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~sexo) + scale_colour_few(labels = c("Potenciales", "Permanentes")) +
  labs(title = "Razones familiares para inactividad según sexo (enero 2020 a septiembre 2020)",
       caption = "Nota: Los porcentajes están calculados en base a toda la Condición de Actividad Económica\nEsta corresponde a la clasificación de todas las personas de 15 años y más, según su situación laboral, basado en el Código Sumario Empleo Específico.\nFuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=12, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura2.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 32,
       height = 15)

# Informalidad
ene_proc %>% 
  group_by(date,sexo, ocup_form) %>%
  filter(!is.na(sector)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         ocup_form = as_factor(ocup_form),
         sexo = as_factor(sexo)) %>%
  ggplot(aes(x = date, y = prop, color = ocup_form)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~sexo) + scale_colour_few() +
  labs(title = "Ocupados según formalidad  por sexo (enero 2020 a septiembre 2020)",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=73, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura3.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 32,
       height = 15)

#  Por edad -------------------


# Inactividad 
ene_proc %>% 
  group_by(date,tramo_edad, activ) %>%
  filter(!is.na(activ)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         activ = as_factor(activ),
         tramo_edad = as_factor(tramo_edad)) %>%
  ggplot(aes(x = date, y = prop, color = activ)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~tramo_edad) + scale_colour_few() +
  labs(title = "Condición de actividad según tramo de edad (enero 2020 a septiembre 2020)",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=65, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura4.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 35,
       height = 20)


# Razones de inactividad
ene_proc %>% 
  group_by(date,tramo_edad, cae_especifico) %>%
  filter(!is.na(cae_especifico)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         cae_especifico = as_factor(cae_especifico),
         tramo_edad = as_factor(tramo_edad)) %>% 
  filter(cae_especifico %in% c("Razones familiares permanentes (Potencial)","Razones familiares permanentes (Habitual)")) %>%
  ggplot(aes(x = date, y = prop, color = cae_especifico)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~tramo_edad) + scale_colour_few(labels = c("Potenciales", "Permanentes")) +
  labs(title = "Razones familiares para inactividad según tramo de edad (enero 2020 a septiembre 2020)",
       caption = "Nota: Los porcentajes están calculados en base a toda la Condición de Actividad Económica\nEsta corresponde a la clasificación de todas las personas de 15 años y más, según su situación laboral, basado en el Código Sumario Empleo Específico.\nFuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=10.5, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura5.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 35,
       height = 20)

# Informalidad
ene_proc %>% 
  group_by(date,tramo_edad, ocup_form) %>%
  filter(!is.na(sector)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         ocup_form = as_factor(ocup_form),
         tramo_edad = as_factor(tramo_edad)) %>%
  ggplot(aes(x = date, y = prop, color = ocup_form)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~tramo_edad) + scale_colour_few() +
  labs(title = "Ocupados según formalidad  por tramo de edad (enero 2020 a septiembre 2020)",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=73, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura6.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 32,
       height = 15)


#  Por nivel educacional -------------------
# Inactividad 
ene_proc %>% 
  group_by(date,edcine, activ) %>%
  filter(!is.na(activ)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         activ = as_factor(activ),
         edcine = as_factor(edcine)) %>%
  ggplot(aes(x = date, y = prop, color = activ)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~edcine) + scale_colour_few() +
  labs(title = "Condición de actividad según nivel educacional (enero 2020 a septiembre 2020)",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=65, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura7.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 35,
       height = 20)


# Razones de inactividad
ene_proc %>% 
  group_by(date,edcine, cae_especifico) %>%
  filter(!is.na(cae_especifico)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         cae_especifico = as_factor(cae_especifico),
         edcine = as_factor(edcine)) %>% 
  filter(cae_especifico %in% c("Razones familiares permanentes (Potencial)","Razones familiares permanentes (Habitual)")) %>%
  ggplot(aes(x = date, y = prop, color = cae_especifico)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~edcine) + scale_colour_few(labels = c("Potenciales", "Permanentes")) +
  labs(title = "Razones familiares para inactividad según nivel educacional (enero 2020 a septiembre 2020)",
       caption = "Nota: Los porcentajes están calculados en base a toda la Condición de Actividad Económica\nEsta corresponde a la clasificación de todas las personas de 15 años y más, según su situación laboral, basado en el Código Sumario Empleo Específico.\nFuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=10.5, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura8.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 35,
       height = 20)

# Informalidad
ene_proc %>% 
  group_by(date,edcine, ocup_form) %>%
  filter(!is.na(sector)) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n / sum(n), 4)*100,
         ocup_form = as_factor(ocup_form),
         edcine = as_factor(edcine)) %>%
  ggplot(aes(x = date, y = prop, color = ocup_form)) +
  geom_line(size = 1)  +
  geom_point() +
  scale_y_continuous(name = "",labels = function(x) paste0(x,"%" )) +
  scale_x_date(name = "", date_breaks = "1 month", date_labels = "%b", expand = c(0,0)) +
  facet_grid(.~edcine) + scale_colour_few() +
  labs(title = "Ocupados según formalidad  por nivel educacional (enero 2020 a septiembre 2020)",
       caption = "Fuente: Elaboración propia en base a Encuesta Nacional de Empleo",
       color = "") +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-01"))), linetype = 3) + 
  geom_text(aes(x=as.Date("2020-03-01"), y=73, label = "COVID-19\nen Chile"), hjust = 0.5,
            colour="black", size = 3.5)

# Guardar
ggsave(plot = last_plot(),
       filename = "output/figures/figura9.png",
       device = "png",
       dpi = 500,
       units = "cm",
       width = 32,
       height = 15)
