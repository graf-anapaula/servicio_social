# Librerías ====
library(tidyverse)
library(janitor)

# Lectura de datos ====
pt_hombres <- janitor::clean_names(read_csv("data/raw/pt_hombres.csv"))
pt_mujeres <- janitor::clean_names(read_csv("data/raw/pt_mujeres.csv"))
salario_hombres <- janitor::clean_names(read_csv("data/raw/salario_hombres.csv"))
salario_mujeres <- janitor::clean_names(read_csv("data/raw/salario_mujeres.csv"))

# Limpieza de datos =====
hombres_puesto <- pt_hombres %>% 
     filter(x3 != 'Tiempo') %>% 
     mutate(fecha = parse_date(x3, "%Y/%b"), 
            sexo = 'M',
            total_puestos = nacional) %>% 
     select(fecha, sexo, total_puestos)

hombres_salario <- salario_hombres %>% 
     filter(x3 != 'Tiempo') %>% 
     mutate(fecha = parse_date(x3, "%Y/%b"), 
            sexo = 'M',
            salario = parse_double(nacional)) %>% 
     select(fecha, sexo, salario)

mujeres_puesto <- pt_mujeres %>% 
     filter(x3 != 'Tiempo') %>% 
     mutate(fecha = parse_date(x3, "%Y/%b"), 
            sexo = 'F',
            total_puestos = nacional) %>% 
     select(fecha, sexo, total_puestos)

mujeres_salario <- salario_mujeres %>% 
     filter(x3 != 'Tiempo') %>% 
     mutate(fecha = parse_date(x3, "%Y/%b"), 
            sexo = 'F',
            salario = parse_double(nacional)) %>% 
     select(fecha, sexo, salario)

# Joins (tratar de hacer en SQL) ====
hombres <- full_join(hombres_puesto, hombres_salario, by = c("fecha", "sexo")) 
mujeres <- full_join(mujeres_puesto, mujeres_salario, by = c("fecha", "sexo"))
trabajo <- union(hombres, mujeres) %>% 
     arrange(fecha)


data.table::fwrite(hombres, "data/interim/trabajo_hombres.csv")
data.table::fwrite(mujeres, "data/interim/trabajo_mujeres.csv")
data.table::fwrite(trabajo, "data/interim/trabajo_total.csv")


