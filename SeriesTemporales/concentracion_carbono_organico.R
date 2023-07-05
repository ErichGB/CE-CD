# Series temporales

library(forecast)
library(zoo) #install.packages("zoo")

# establecer el directorio de trabajo
setwd("/Users/erichgonzalez/Documents/Maestria/CD/ConvocatoriaExtraordinaria")

# Carga los datos del fichero DOC_Thames_1883_2014_monthly.csv en un data frame.
data = as.data.frame(read.csv("DOC_Thames_1883_2014_monthly.csv",fileEncoding="UTF-8"))

# Verificar si hay valores NA en la serie de datos
missing_values <- sum(is.na(data$DOC))

if (missing_values > 0) {
  data <- na.omit(data) # Si hay valores NA, eliminar las filas con NA
}

total_months <- ((2014 - 1883) * 12) + 12 #1584

# verificar la continuidad de la serie
consecutive_dates <- seq(min(data$year), max(data$year), by = 1)
consecutive_months <- rep(1:12, length.out = total_months)
consecutive_data <- data.frame(year = consecutive_dates, month = consecutive_months)

# Unir los datos originales y los datos consecutivos
complete_data <- merge(data, consecutive_data, by = c("year", "month"), all = TRUE)

# Escoger el periodo más largo posible con datos ininterrumpidos
start_date <- min(complete_data$date)
end_date <- max(complete_data$date)

data_cleaned <- data[data$date >= start_date & data$date <= end_date, ]

# Convertir las columnas "year" y "month" a un formato de fecha
data_cleaned$date <- as.Date(paste(data_cleaned$year, data_cleaned$month, "1", sep = "-"), format = "%Y-%m-%d")

# Ordenar los datos por fecha
data_cleaned <- data_cleaned[order(data_cleaned$date), ]

# Crear una serie temporal utilizando la columna "DOC"
time_series <- zoo(data_cleaned$DOC, order.by = data_cleaned$date)

# Reservar los últimos 5 datos para validar el modelo
validation_data <- tail(time_series, 5)

# Ajustar el modelo ARIMA(1,1,1) a la serie de datos
model <- arima(time_series, order = c(1, 1, 1))


