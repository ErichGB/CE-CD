# Clustering
library(tidyr)
library(factoextra)

# [a] Carga los datos del Customer_Data.csv en un data frame.
input_orig = as.data.frame(read.csv("/Users/erichgonzalez/Documents/Maestria/CD/ConvocatoriaExtraordinaria/Customer_Data.csv",sep=",",fileEncoding="UTF-8"))

# [b] Seleccionar los datos correspondientes a:
# balance, purchases, cash_advance, 
# purchases_frequency, cash_advance_frequency, 
# credit_limit, prc_full_payment, tenure
selected_cols <- c("balance", "purchases", "cash_advance", "purchases_frequency", "cash_advance_frequency", "credit_limit", "prc_full_payment", "tenure")
input_selected <- input_orig[, selected_cols]

# [c] Eliminar filas con valores NA
input_cleaned <- na.omit(input_selected)

# [d] Clustering de los clientes
# Ponderación o escalamiento de los datos
#scaled_data <- scale(input_cleaned)
scaled_data <- input_cleaned


# Aplicar el algoritmo K-means con diferentes valores de k
k_values <- 1:10  # Prueba valores de k desde 1 hasta 10
wss <- vector("numeric", length = length(k_values))  # Vector para almacenar los valores de WSS

for (i in k_values) {
  kmeans_result <- kmeans(scaled_data, centers = i, nstart = 25)
  wss[i] <- kmeans_result$tot.withinss  # Guarda el valor de WSS para cada valor de k
}

# Graficar el método del codo (elbow method)
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, xlab = "Número de clusters (k)", ylab = "Suma de cuadrados dentro de los clusters (WSS)")

k <- 2
km <- kmeans(scaled_data, k)

fviz_cluster(km, data = scaled_data)


