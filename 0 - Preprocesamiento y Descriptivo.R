library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(tibble)

# Load the dataset

years = c("2023", "2022", "2021") #, "2019", "2018", "2017", "2016")
data <- NULL


for(year in years) {
  file_path = paste0("Data/delitos_", year, ".xlsx")
  data_year = read_excel(file_path)
  
  print(year)
  print(length(data_year$cantidad))
  print(names(data_year))
  print(str(data_year))
  
  # Standarize DataFrames
  data_year <- data_year %>%
    mutate(franja = as.character(franja))  %>%
    mutate(comuna = as.character(comuna))  %>%
    mutate(latitud = as.character(latitud))  %>%
    mutate(longitud = as.character(longitud))
  
  data = bind_rows(data, data_year)

}
rm(data_year)

# Delete columns
data <- data %>%
  select(-c('id-sum','uso_arma', 'uso_moto', 'id-mapa', 'cantidad'))

# Import Holidays
holidays <- read.csv("Data/Feriados_Arg.csv", sep=';')
holidays <- holidays %>%
  mutate(fecha = as.POSIXct(holidays$Fecha, format = "%d/%m/%Y", tz = 'UTC')) 

# Add column to Holidays
data = data %>%
  left_join(holidays, by = "fecha")
data = data %>%
  mutate(No_Laborable = ifelse(dia == "DOM" | !is.na(Fecha), 1, 0)) %>%
  select(-c("Anio", 'Fecha'))


# Count Number of "Crimen" by Year
data %>%
  group_by(tipo, anio) %>%   
  summarise(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = anio, values_from = count, values_fill = 0) 


# DATA: "Hurto" & "Robo" 
data_robo_hurto =  data %>%
  filter(tipo %in% c("Hurto", "Robo")) %>%
  filter(fecha >= as.POSIXct("2020-12-31"))

write.csv(data_robo_hurto, "data/datos_hurto_robo.csv")



# DATA: "Hurto" & "Robo" 
data_robo_hurto_agrupado =  data %>%
  filter(tipo %in% c("Hurto", "Robo")) %>%
  group_by(fecha, No_Laborable, anio) %>%   
  summarise(count = n(), .groups = 'drop') %>%
  filter(fecha >= as.POSIXct("2020-12-31")) %>%
  #filter(fecha <= as.POSIXct("2023-10-01")) %>%
  mutate(ma7 = rollmean(count, k = 7, fill = NA, align = "right"))  %>%
  mutate(ma30 = rollmean(count, k = 30, fill = NA, align = "right"))

data_robo_hurto_agrupado %>%
  ggplot(., aes(x = fecha)) +
    geom_line(aes(y = count), color = "black") +      # Original count line
    geom_line(aes(y = ma7), color = "red") +       # Moving average line
    #geom_line(aes(y = ma30), color = "blue") +       # Moving average line
    ylim(0, NA) +                                    # Set minimum ylim to 0
    labs(title = "Cantidad Crimenes por Dia y MA7",
         x = "Fecha",
         y = "Cantidad") +
    theme_minimal()


# Count Number of "Crimen" by Year
data_robo_hurto_agrupado %>%
  group_by(anio) %>%   
  summarise(cant = sum(count), avg = mean(count), median(count), .groups = 'drop') %>% 
  #pivot_wider(names_from = anio, values_from = c(count, avg), values_fill = 0) 

  
# DATA: "Hurto" & "Robo" 
data %>%
  filter(tipo %in% c("Hurto", "Robo")) %>%
  group_by(fecha, dia) %>%  
  summarise(cant = n(), .groups = 'drop') %>%
  group_by(dia) %>%
  summarise(cant_por_dia = sum(cant), avg_dia = mean(cant), median(cant),.groups = 'drop')
  
  
  
  

data_robo_hurto_agrupado %>%
  filter(fecha >= as.POSIXct("2023-08-31")) %>%
  filter(fecha <= as.POSIXct("2023-10-01")) %>%
  ggplot(., aes(x = fecha)) +
  geom_line(aes(y = count), color = "black") +      # Original count line
  geom_point(aes(y = count, color = ifelse(No_Laborable, "No Laborable", "Laborable")), size = 2, show.legend = FALSE) +  # Conditional color
  geom_line(aes(y = ma7), color = "red") +       # Moving average line
  ylim(0, NA) +                                    # Set minimum ylim to 0
  labs(title = "Daily Counts and 7-Day Moving Average of Hurto and Robo",
       x = "Fecha",
       y = "Count") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 day", date_minor_breaks = "1 day") +  # Set daily breaks
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  +
  scale_color_manual(values = c("Laborable" = "black", "No Laborable" = "blue"))


# By Barrio
data_robo_hurto_barrio =  data %>%
  filter(tipo %in% c("Hurto", "Robo")) %>%
  group_by(fecha, barrio) %>%   
  summarise(count = n(), .groups = 'drop') %>%
  filter(fecha >= as.POSIXct("2023-01-01"))

ggplot(data_robo_hurto_barrio, aes(x = fecha, color= barrio)) +
  geom_line(aes(y = count)) +      # Original count line
  ylim(0, NA) +                                    # Set minimum ylim to 0
  labs(title = "Daily Counts",
       x = "Fecha",
       y = "Count") +
  theme_minimal()






