# Librerías =====
library(tidyverse)
library(tidylog)
library(lubridate)

# Lectura =====
banxico <- readxl::read_excel('data/raw/banxico.xlsx', 
                   skip = 17) %>% 
     janitor::clean_names()

# Limpieza ====
banxico_limpia <- banxico %>% 
     select(fecha, 
            tasa_objetivo = sf61745) %>%
        filter(!is.na(tasa_objetivo)) %>% 
        mutate(tasa_objetivo = parse_number(tasa_objetivo))
# data.table::fwrite(banxico_limpia, "data/interim/banxico_limpia.csv")

# Colores ====
dark_blue <- '#142850'
med_blue <- '#27496d'
light_blue <- '#27496d'


# Cambios ====
gg_banxico <- banxico_limpia %>% 
     mutate(delta_tasa = (tasa_objetivo - lag(tasa_objetivo)),
            delta_por = scales::percent(delta_tasa),
                          cambio = if_else(delta_tasa > 0, 'positivo', 'negativo')) %>% 
     filter(delta_tasa != 0)
data.table::fwrite(gg_banxico, "data/interim/banxico_tabla.csv")

gg_banxico %>% 
     ggplot(aes(x = fecha, y = tasa_objetivo)) +
        geom_line(size = 1) +
        geom_point(aes(color = cambio, fill = cambio), size = 4) +
        scale_x_datetime("Años") + #breaks = date_breaks("1 year")) +
        scale_y_continuous("Tasa de interés")

gg_banxico %>%
        ggplot(aes(x = month(fecha), y = year(fecha))) +
        geom_point(aes(fill = cambio, color = cambio, 
                       size = abs(delta_tasa)), show.legend = F) +
        scale_x_continuous("Mes", breaks = c(1, 3, 5, 7,9, 12)) +
        scale_y_continuous("Año", breaks = c(2008, 2009, 2013, 2014, 2016, 2017, 2018, 2019, 2020),  
                           expand = expansion(c(0.1, 0.25))) +
        theme_minimal() +
        ggtitle("Cambios en la tasa de interés")
        
        
        
        
# Gráficas ----

gg_banxico +
     geom_bar(aes(x = fecha, y = tasa_objetivo), stat = 'identity')

gg_banxico +    
        geom_col(aes(x = fecha, y = delta_tasa))


