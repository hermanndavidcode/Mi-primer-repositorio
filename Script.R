library(ggplot2)
library(dplyr)
library(janitor)
library(writexl)
library(gridExtra)
library(lubridate)
library(forcats)
library(readr)
library(descr)
library(RColorBrewer)
library(stringi)

Homicidiosinv <- read_csv("C:/Users/herma/Desktop/Homicidiosinv.csv", 
                          col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                           Hora = col_time(format = "%H:%M:%S")))
View(Homicidiosinv)

base<-Homicidiosinv

names(base)

names <- c("Municipio", "Día", "Barrio", "Zona", 
           "Clase_de_sitio", "Arma_empleada", "Móvil_agresor",
           "Móvil_víctima", "Sexo", "Estado_civil", "País_de_nacimiento",
           "Clase_de_empleado", "Escolaridad", "Siniestro", "catHora", "Mes",
           "Año", "Provincia")


base[,names] <- lapply(base[,names], factor)

base$Edad <- as.numeric(base$Edad)

base[,names] <- lapply(base[,names], stri_trans_totitle)

#Crear categorías con case_when DPLYR

base = base %>% 
        mutate(catEdad = case_when(Edad >= 0 & Edad <= 4 ~ "0-04",  
                                   Edad >= 5 & Edad <= 9 ~ "05-09",
                                   Edad >= 10 & Edad <= 14 ~ "10-14", 
                                   Edad >= 15 & Edad <= 19 ~ "15-19", 
                                   Edad >= 20 & Edad <= 24 ~ "20-24", Edad >= 25 & Edad <= 29 ~ "25-29",
                                   Edad >= 30 & Edad <= 34 ~ "30-34", Edad >= 35 & Edad <= 39 ~ "35-39", 
                                   Edad >= 40 & Edad <= 44 ~ "40-44", Edad >= 45 & Edad <= 49 ~ "45-49", 
                                   Edad >= 50 & Edad <= 54 ~ "50-54", Edad >= 55 & Edad <= 59 ~ "55-59", 
                                   Edad >= 60 & Edad <= 64 ~ "60-64", Edad >= 65 & Edad <= 69 ~ "65-69", 
                                   Edad >= 70 & Edad <= 74 ~ "70-74", Edad >= 75 & Edad <= 79 ~ "75-79", 
                                   Edad >= 80 & Edad <= 84 ~ "80-84", Edad >= 85 ~ "85 y más"))

#Crear provincias con case_when DPLYR

base = base %>% 
        mutate(Provincia = case_when(Municipio == "Choconta" ~ "Almeidas", 
                                     Municipio == "Macheta" ~ "Almeidas", Municipio == "Manta" ~ "Almeidas",
                                     Municipio == "Sesquile" ~ "Almeidas", Municipio == "Suesca" ~ "Almeidas",
                                     Municipio == "Tibirita" ~ "Almeidas", Municipio == "Villapinzon" ~ "Almeidas",
                                     Municipio == "Agua De Dios" ~ "Alto Magdalena", Municipio == "Girardot" ~ "Alto Magdalena",
                                     Municipio == "Guataqui" ~ "Alto Magdalena", Municipio == "Jerusalen" ~ "Alto Magdalena",
                                     Municipio == "Nariño" ~ "Alto Magdalena", Municipio == "Nilo" ~ "Alto Magdalena",
                                     Municipio == "Ricaurte" ~ "Alto Magdalena", Municipio == "Tocaima" ~ "Alto Magdalena",
                                     Municipio == "Caparrapi" ~ "Bajo Magdalena", Municipio == "Guaduas" ~ "Bajo Magdalena", Municipio == "Puerto Salgar" ~ "Bajo Magdalena",
                                     Municipio == "Alban" ~ "Gualivá", Municipio == "La Peña" ~ "Gualivá", Municipio == "Quebradanegra" ~ "Gualivá",
                                     Municipio == "La Vega" ~ "Gualivá", Municipio == "Nimaima" ~ "Gualivá",
                                     Municipio == "Nocaima" ~ "Gualivá", Municipio == "Qubradanegra" ~ "Gualivá",
                                     Municipio == "San Francisco" ~ "Gualivá", Municipio == "Sasaima" ~ "Gualivá",
                                     Municipio == "Supata" ~ "Gualivá", Municipio == "Utica" ~ "Gualivá", 
                                     Municipio == "Vergara" ~ "Gualivá", Municipio == "Villeta" ~ "Gualivá",
                                     Municipio == "Gachala" ~ "Guavio", Municipio == "Gacheta" ~ "Guavio",
                                     Municipio == "Gama" ~ "Guavio", Municipio == "Guasca" ~ "Guavio",
                                     Municipio == "Guatavita" ~ "Guavio", Municipio == "Junin" ~ "Guavio",
                                     Municipio == "La Calera" ~ "Guavio", Municipio == "Ubala" ~ "Guavio",
                                     Municipio == "Beltran" ~ "Magdalena Centro", Municipio == "Bituima" ~ "Magdalena Centro",
                                     Municipio == "Chaguani" ~ "Magdalena Centro", Municipio == "Guayabal De Siquima" ~ "Magdalena Centro",
                                     Municipio == "Puli" ~ "Magdalena Centro", Municipio == "San Juan De Rio Seco" ~ "Magdalena Centro",
                                     Municipio == "Viani" ~ "Magdalena Centro", Municipio == "Paratebueno" ~ "Medina", Municipio == "Medina" ~ "Medina",
                                     Municipio == "Caqueza" ~ "Oriente", Municipio == "Chipaque" ~ "Oriente",
                                     Municipio == "Choachi" ~ "Oriente", Municipio == "Caqueza" ~ "Oriente",
                                     Municipio == "Fosca" ~ "Oriente", Municipio == "Guayabetal" ~ "Oriente",
                                     Municipio == "Gutierrez" ~ "Oriente", Municipio == "Quetame" ~ "Oriente",
                                     Municipio == "Ubaque" ~ "Oriente", Municipio == "Une" ~ "Oriente", Municipio == "Fomeque" ~ "Oriente",
                                     Municipio == "El Peñon" ~ "Rionegro", Municipio == "La Palma" ~ "Rionegro",
                                     Municipio == "Pacho" ~ "Rionegro", Municipio == "Paime" ~ "Rionegro",
                                     Municipio == "San Cayetano" ~ "Rionegro", Municipio == "Topaipi" ~ "Rionegro",
                                     Municipio == "Villagomez" ~ "Rionegro", Municipio == "Yacopi" ~ "Rionegro",
                                     Municipio == "Cajica" ~ "Sabana Centro", Municipio == "Chia" ~ "Sabana Centro",
                                     Municipio == "Cogua" ~ "Sabana Centro", Municipio == "Cota" ~ "Sabana Centro",
                                     Municipio == "Gachancipa" ~ "Sabana Centro", Municipio == "Nemocon" ~ "Sabana Centro",
                                     Municipio == "Sopo" ~ "Sabana Centro", Municipio == "Tabio" ~ "Sabana Centro",
                                     Municipio == "Tenjo" ~ "Sabana Centro", Municipio == "Tocancipa" ~ "Sabana Centro",
                                     Municipio == "Zipaquira" ~ "Sabana Centro", Municipio == "Chia" ~ "Sabana Centro",
                                     Municipio == "Bojaca" ~ "Sabana Occidente", Municipio == "El Rosal" ~ "Sabana Occidente",
                                     Municipio == "Facatativa" ~ "Sabana Occidente", Municipio == "Funza" ~ "Sabana Occidente",
                                     Municipio == "Madrid" ~ "Sabana Occidente", Municipio == "Mosquera" ~ "Sabana Occidente",
                                     Municipio == "Subachoque" ~ "Sabana Occidente", Municipio == "Zipacon" ~ "Sabana Occidente",
                                     Municipio == "Sibate" ~ "Soacha", Municipio == "Arbelaez" ~ "Sumapaz",
                                     Municipio == "Cabrera" ~ "Sumapaz", Municipio == "Fusagasuga" ~ "Sumapaz",
                                     Municipio == "Granada" ~ "Sumapaz", Municipio == "Pandi" ~ "Sumapaz",
                                     Municipio == "Pasca" ~ "Sumapaz", Municipio == "San Bernardo" ~ "Sumapaz",
                                     Municipio == "Silvania" ~ "Sumapaz", Municipio == "Tibacuy" ~ "Sumapaz",
                                     Municipio == "Venecia" ~ "Sumapaz", Municipio == "Anapoima" ~ "Tequendama",
                                     Municipio == "Anolaima" ~ "Tequendama", Municipio == "Apulo" ~ "Tequendama",
                                     Municipio == "Cachipay" ~ "Tequendama", Municipio == "El Colegio" ~ "Tequendama",
                                     Municipio == "La Mesa" ~ "Tequendama", Municipio == "Quilpe" ~ "Tequendama",
                                     Municipio == "San Antonio Del Tequendama" ~ "Tequendama", Municipio == "Tena" ~ "Tequendama",
                                     Municipio == "Viota" ~ "Tequendama", Municipio == "Quipile" ~ "Tequendama",
                                     Municipio == "Carmen De Carupa" ~ "Ubaté", Municipio == "Cucunuba" ~ "Ubaté", Municipio == "Fuquene" ~ "Ubaté",
                                     Municipio == "Guacheta" ~ "Ubaté", Municipio == "Lenguazaque" ~ "Ubaté",
                                     Municipio == "Simijaca" ~ "Ubaté", Municipio == "Susa" ~ "Ubaté",
                                     Municipio == "Sutatausa" ~ "Ubaté", Municipio == "Tausa" ~ "Ubaté",
                                     Municipio == "Soacha" ~ "Soacha", Municipio == "Villa De San Diego De Ubate" ~ "Ubaté"))

#Modificar el formato de hora con lubridate

base$Hora <- hour(base$Hora)

#Crear categorías por hora con DPLYR

base = base %>% 
        mutate(catHora = case_when(Hora >= 0 & Hora <=5  ~ "Madrugada", 
                                   Hora >= 6 & Hora <= 12 ~ "Mañana", 
                                   Hora >= 13 & Hora <= 18 ~ "Tarde", 
                                   Hora >= 17 & Hora <= 23 ~ "Noche"))

tabyl(base$catHora) %>%
        adorn_pct_formatting(digits = 0) %>% 
        adorn_totals("row")

base %>% 
        tabyl(catEdad) %>%
        adorn_pct_formatting(digits = 0) %>% 
        adorn_totals("row")

#Extraer mes de la fecha

base = base %>%
        mutate(Mes = month(Fecha))

base$Mes <- factor(base$Mes, levels = c(seq(1:12)), labels = c("Enero","Febrero", 
                                                               "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", 
                                                               "Octubre","Noviembre", "Diciembre"))

#Extraer año

base = base %>%
        mutate(Año = year(Fecha))

#CrossTabulations

tabla1 <- base %>% 
        filter(Móvil_víctima %in% c("A Pie", "Bicicleta", "Conductor Motocicleta"), 
               Siniestro == "Lesiones")%>% 
        tabyl(Móvil_víctima, Móvil_agresor, Municipio) %>% 
        adorn_totals(c("row", "col")) %>% 
        adorn_percentages("row") %>% 
        adorn_pct_formatting(rounding = "half up", digits = 0) %>%
        adorn_ns()

tabla1 <- base %>% 
        filter(Siniestro == "Lesiones", Año == "2019") %>% 
        tabyl(Municipio) %>% 
        adorn_totals("row") %>% 
        adorn_pct_formatting(rounding = "half up", digits = 0)

write_xlsx(tabla1,"C:\\Users\\herma\\Desktop\\Lesiones Agresor x víctima 2019.xlsx")



#Crear paleta de colores 
nb.cols <- 18

mycolors <- colorRampPalette(brewer.pal(8, "Oranges"))(nb.cols)
# Use scale_fill_manual

#Gráfico de barras ordenado

ggplot(base, aes(x = fct_infreq(as.factor(Mes)))) + 
        geom_bar(fill = "blue", alpha = 0.7)

#Gráfco de barras sin ordenar

ggplot(base, aes(as.factor(Mes))) + 
        geom_bar(fill = "blue", alpha = 0.7)

base %>% 
        filter(Año == "2019") %>% 
        ggplot(aes(x = catEdad, fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), vjust = -0.5, size = 4)  +
        xlab("Rango de edad") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=15), axis.text.x = element_text(angle = 45)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Siniestro))

#Stacked geom_bar

base %>% 
        filter(Siniestro == "Lesiones", Año == "2019") %>% 
        ggplot(aes(x = catEdad, fill = Móvil_víctima)) + geom_bar(alpha = 0.7, position = "stack") + 
        geom_text(stat = "count",aes(label=..count..), position = position_stack(vjust=0.5), size = 3) + 
        xlab("Año") + ylab("Número de casos") + theme_minimal() 
+ facet_grid(rows = vars(Año))


#Counted Dodged Geom_bar

base %>%
        ggplot(aes(x = Siniestro, fill = Año, group = Móvil_víctima)) + 
        geom_bar(alpha = 0.7, stat = "count", width = 0.9, position = "dodge") + 
        xlab("Rango de edad") + ylab("Número de casos") + labs(fill = "Móvil víctima") +
        geom_text(stat = "count",aes(label=..count..), position =  position_dodge(0.8), 
                  vjust = -0.5, size = 3) + theme_minimal() + scale_fill_brewer(palette = "Spectral")
scale_y_continuous(breaks = 1:12)

#Percetaged and counted dodged geom_bar

base %>%
        ggplot(aes(x = catEdad, fill = ..count.., group = Siniestro)) + 
        geom_bar(width = 0.9, position = "dodge") + 
        geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))),
                  stat='count',position =  position_dodge(0.8), vjust = -0.5, size = 3) + 
        xlab("Rango de edad") + ylab("Número de casos") + labs(fill = "Móvil víctima") +
        theme_minimal() + scale_fill_brewer(palette = "Spectral")

#ANÁLISIS

base %>%
        filter(Siniestro == "Lesiones", Sexo %in% c("Femenino", "Masculino")) %>% 
        ggplot(aes(x = Sexo, 
                   fill = ..count..)) + geom_bar(width = 0.7, alpha = 0.8) +
        geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))),
                  stat='count',vjust = -0.5, size = 3) + labs(y="Cantidad de casos", 
                                                              x="Sexo") + theme_minimal() + guides(fill = FALSE) + 
        theme(text = element_text(size=13)) +
        facet_grid(rows = vars(Año))


#catEdad
base %>% 
        ggplot(aes(x = fct_infreq(Móvil_víctima), fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), vjust = -0.5, size = 4)  +
        expand_limits(y = c(1, 130)) + xlab("Rango de edad") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=13), axis.text.x = element_text(angle = 30)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Año))

#Agresor
base %>% 
        ggplot(aes(x = fct_infreq(Provincia), fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), hjust = 0, size = 3)  +
        expand_limits(y = c(1, 1700)) + xlab("Móvil de la víctima") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=11)) + 
        guides(fill=FALSE) + facet_grid(cols = vars(Siniestro), rows = vars(Año)) + coord_flip()

base %>% 
        filter(Siniestro == "Lesiones") %>% 
        ggplot(aes(x = Sexo, fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), hjust = 0, size = 3)  +
        expand_limits(y = c(1, 660)) + xlab("Provincia") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=11)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Año), cols = vars(Provincia)) + coord_flip()


scale_x_discrete(guide = guide_axis(n.dodge = 2))


p1 <- base %>%
        filter(Siniestro == "Homicidios") %>%
        ggplot(aes(x = Año, fill = ..count..)) + 2
geom_bar(width = 0.9, alpha = 0.8) + 
        geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))),
                  stat='count',vjust = -0.5, size = 4) + labs(y="Cantidad de casos", x="Homicidios") +
        theme_minimal() + guides(fill = FALSE) + theme(text = element_text(size=13))

p2 <- base %>%
        filter(Siniestro == "Lesiones") %>% 
        ggplot(aes(x = Año, fill = ..count..)) + 
        geom_bar(width = 0.9, alpha = 0.8) +
        geom_text(aes(label = sprintf('%s (%.1f%%)', after_stat(count), after_stat(count / sum(count) * 100))),
                  stat='count',vjust = -0.5, size = 4) + labs(y="Cantidad de casos", x="Lesiones") +
        theme_minimal() + guides(fill = FALSE) + theme(text = element_text(size=13))

grid.arrange(p1, p2, ncol=2)

write_xlsx(tabla1,"C:\\Users\\herma\\Desktop\\Municipios 2019.xlsx")

base %>% 
        filter(Provincia %in% c("Sumapaz", "Soacha", "Sabana Centro", "Sabana Occidente"), Siniestro == "Lesiones") %>% 
        ggplot(aes(x = fct_infreq(Móvil_víctima), fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), hjust = 0, size = 3)  +
        expand_limits(y = c(1, 320)) + xlab("Móvil de la víctima") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=11)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Año), cols = vars(Provincia)) + coord_flip()

base %>% 
        filter(Provincia %in% c("Ubaté", "Alto Magdalena", "Bajo Magdalena", "Oriente"), Siniestro == "Lesiones") %>% 
        ggplot(aes(x = fct_infreq(Móvil_víctima), fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), hjust = 0, size = 3)  +
        expand_limits(y = c(1, 245)) + xlab("Móvil de la víctima") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=11)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Año), cols = vars(Provincia)) + coord_flip()

base %>% 
        filter(Provincia %in% c("Gualivá", "Almeidas", "Tequendama", "Guavio"), Siniestro == "Lesiones") %>% 
        ggplot(aes(x = fct_infreq(Móvil_víctima), fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), hjust = 0, size = 3)  +
        expand_limits(y = c(1, 80)) + xlab("Móvil de la víctima") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=11)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Año), cols = vars(Provincia)) + coord_flip()

base %>% 
        filter(Provincia %in% c("Rionegro", "Magdalena Centro", "Medina"), Siniestro == "Lesiones") %>% 
        ggplot(aes(x = fct_infreq(Móvil_víctima), fill = ..count..))+  geom_bar(alpha = 0.7) + 
        geom_text(stat = "count",aes(label=..count..), hjust = 0, size = 3)  +
        expand_limits(y = c(1, 40)) + xlab("Móvil de la víctima") + ylab("Número de casos") + theme_minimal() + theme(text = element_text(size=11)) + 
        guides(fill=FALSE) + facet_grid(rows = vars(Año), cols = vars(Provincia)) + coord_flip()

#Dodge geom_bar with guides on bottom

base %>%
        filter(Municipio %in% c("Soacha", "Fusagasuga", "Girardot", "Mosquera", "Funza", "Chia", "Facatativa", "Zipaquira"), Siniestro == "Lesiones") %>% 
        ggplot(aes(x = fct_infreq(Municipio), fill = Móvil_víctima, group = Móvil_víctima)) + 
        geom_bar(alpha = 0.7, stat = "count", width = 0.9, position = "dodge") + 
        expand_limits(y = c(0, 330)) + xlab("Municipio") + ylab("Número de casos") + labs(fill = "Móvil víctima") +
        geom_text(stat = "count",aes(label=..count..), position =  position_dodge(width = 0.9), 
                  vjust = -0.5, size = 3) + theme_minimal() + theme(legend.position="bottom") + scale_fill_brewer(palette = "Paired") +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) + facet_grid(rows = vars(Año))

base %>%
        filter(Municipio %in% c("Cota", "Granada", "Madrid", "Ricaurte", "Villeta", "Puerto Salgar", "Villa De San Diego De Ubate"), Siniestro == "Lesiones") %>% 
        ggplot(aes(x = fct_infreq(Municipio), fill = Móvil_víctima, group = Móvil_víctima)) + 
        geom_bar(alpha = 0.7, stat = "count", width = 0.9, position = "dodge") + 
        expand_limits(y = c(0, 60)) + xlab("Municipio") + ylab("Número de casos") + labs(fill = "Móvil víctima") +
        geom_text(stat = "count", aes(label=..count..), position =  position_dodge(width = 0.9), 
                  vjust = -0.5, size = 3) + theme_minimal() + theme(legend.position="bottom") + scale_fill_brewer(palette = "Paired") +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) + facet_grid(rows = vars(Año))

#Homicidios 2017 "Soacha","Puerto Salgar", "Fusagasuga", "Mosquera",
"Chia", "Facatativa", "Granada", "Funza", "Cajica",
"Cota", "Chipaque", "Girardot", "Madrid", "Villeta",
"Choconta"

#Homicidios 2019 "Soacha", "Fusagasuga", "Mosquera", "Funza", "Cota", "Facatativa", "Chia", "Zipaquira", "Madrid", "Silvania", "Cajica", "Simijaca", "Tocancipa", "Girardot", "Puerto Salgar"


base %>% 
        select(Móvil_agresor, Móvil_víctima, Siniestro, Municipio, Año) %>% 
        filter(Móvil_víctima %in% c("A Pie", "Conductor Motocicleta", "Bicicleta"), 
               Siniestro == "Lesiones", Móvil_agresor %in% c("A Pie", "Conductor Vehiculo", "Conductor Motocicleta"),
               Municipio %in% c("Soacha","Puerto Salgar", "Fusagasuga", "Mosquera",
                                "Chia", "Facatativa", "Granada", "Funza", "Cajica",
                                "Cota", "Chipaque", "Girardot", "Madrid", "Villeta",
                                "Choconta"),
               Año =="2017") %>% 
        ggplot(aes(Móvil_víctima, group = Móvil_agresor, fill = Móvil_agresor)) + 
        geom_bar(alpha = 0.7, stat = "count", width = 0.9, position = "dodge") +
        xlab("Móvil de la víctima") + ylab("Número de casos") + labs(fill = "Móvil del agresor") +
        expand_limits(y = c(0, 140)) + geom_text(stat = "count", aes(label=..count..), position =  position_dodge(width = 0.9), 
                                                 angle = 90, hjust = -0.1, size = 3) + theme_minimal() + scale_fill_brewer(palette = "Paired") +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) + facet_wrap(vars(Municipio))

tabla1 <- base %>% 
        select(Móvil_agresor, Móvil_víctima, Siniestro, Municipio, Año) %>% 
        filter(Móvil_víctima %in% c("A Pie", "Conductor Motocicleta", "Bicicleta"), 
               Siniestro == "Homicidios", Móvil_agresor %in% c("A Pie", "Conductor Vehiculo", "Conductor Motocicleta"),
               Municipio %in% c("Soacha",
                                "Fusagasuga",
                                "Mosquera",
                                "Funza",
                                "Cota",
                                "Facatativa",
                                "Chia",
                                "Zipaquira",
                                "Madrid",
                                "Silvania",
                                "Cajica",
                                "Simijaca",
                                "Tocancipa",
                                "Girardot",
                                "Puerto Salgar"), Año == "2019") %>%  
        tabyl(Móvil_víctima, Móvil_agresor, Municipio) %>% 
        adorn_totals(c("row", "col")) %>% 
        adorn_percentages("row") %>% 
        adorn_pct_formatting(rounding = "half up", digits = 0) %>%
        adorn_ns()

write_xlsx(tabla1,"C:\\Users\\herma\\Desktop\\Homicidios Municipales Agresor x víctima 2019(2).xlsx")


varpor <- function(arg1) {
        año <- summary(arg1)
        Año1 <- año[c(0,3)]
        Año2 <- año[c(0,2)]
        añoant <- as.numeric(paste(Año1))
        añoact <- as.numeric(paste(Año2))
        diferencia <- añoant-añoact
        difporcent <- diferencia*100/añoant
        texto <- paste0("n casos 2018: ", Año1, ", ", "n casos 2019: ", Año2,", ", "Variación %: ", difporcent,", ", "Variación de casos: ", 
                        diferencia, ", ", "Tendencia: ")
        if(difporcent<0){
                print(paste0(texto, "Disminuyó"))
        } else {
                print(paste0(texto, "Aumentó"))
        }
        }

base1 = base  %>% 
        select(Provincia, Año, Municipio) %>%                 
        filter(Municipio %in% c("Zipaquira"))

varpor(base1$Año)

summary(SabanaCentro$Año)

SabanaCentro <- c("Cajica","Chia","cogua","Cota","Gachancipa","Nemocon","Tabio",
                  "Tenjo","Tocancipa","Sopo","Zipaquira")


lapply(SabanaCentro, varpor)
