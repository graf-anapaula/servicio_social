# Librerías ====
library(tidyverse)
library(lubridate)
library(scales)

# Lectura de datos
hombres <- read_csv("data/interim/trabajo_hombres.csv")
mujeres <- read_csv("data/interim/trabajo_mujeres.csv")
trabajo <- read_csv("data/interim/trabajo_total.csv")


# Manipulación de datos ====
hombres <- hombres %>%  
     mutate(m_puestos = total_puestos, m_salario = salario) %>% 
     select(fecha, m_puestos, m_salario) 

mujeres <- mujeres %>% 
     mutate(f_puestos = total_puestos, f_salario = salario) %>% 
     select(fecha, f_puestos, f_salario)

brechas <- full_join(hombres, mujeres, by = 'fecha') %>% 
     mutate(brecha_puestos = m_puestos - f_puestos,
            mtrabajo_por = m_puestos / f_puestos - 1,
            brecha_salarial = m_salario - f_salario,
            fsalario_por = m_salario / f_salario - 1)
     
# Visualizaciones ====

# Serie de tiempo
trabajo %>% 
     ggplot(aes(fecha, total_puestos, color = sexo)) +
     geom_line(size = 1) +
     scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
     scale_y_continuous("Puestos de trabajo", labels = scales::dollar_format()) +
     theme_minimal()

trabajo %>% 
     ggplot(aes(fecha, salario, color = sexo)) +
     geom_line(size = 1) +
     scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
     scale_y_continuous("Salario por hora", labels = scales::dollar_format()) +
     theme_minimal()

brechas %>% 
     group_by(anio = year(fecha)) %>% 
     summarise(fsalario_por = mean(fsalario_por)) %>% 
     mutate(cambio = if_else(fsalario_por - lag(fsalario_por) > 0, 'Aumento', 'Disminución')) %>% 
          filter(!is.na(cambio)) %>%
     ggplot(aes(anio, fsalario_por)) +
     geom_bar(aes(color = cambio, fill = cambio), stat = "identity", width = 0.6) +
     scale_x_continuous(NULL, breaks = seq(1998, 2020, by = 2)) +
     scale_y_continuous("Cambio porcentual", labels = scales::percent_format(accuracy = 1L)) +
     theme_minimal() 
     
brechas %>% 
     group_by(anio = year(fecha)) %>% 
     summarise(mtrabajo_por = mean(mtrabajo_por)) %>% 
     mutate(cambio = if_else(mtrabajo_por - lag(mtrabajo_por) > 0, 'Aumento', 'Disminución')) %>% 
          filter(!is.na(cambio)) %>%
     ggplot(aes(anio, mtrabajo_por)) +
     geom_bar(aes(color = cambio, fill = cambio), stat = "identity", width = 0.6) +
     scale_x_continuous(NULL, breaks = seq(1998, 2020, by = 2)) +
     scale_y_continuous("Cambio porcentual", labels = scales::percent_format(accuracy = 1L)) +
     theme_minimal() 
     


