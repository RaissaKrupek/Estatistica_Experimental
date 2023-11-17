## AULA PARCELAS SUBDIVIDIDAS

sulcos <- c(rep("simples", 20), rep("duplo", 20))
cortes <- c(rep("planta", 10), rep("soca", 10), rep("planta", 10), rep("soca", 10))
rep <- rep(1:10, 4)
cbind(sulcos,cortes)

prod <- c(92.9, 128.6, 121.7, 122.8, 118.1, 115.7, 121.4, 126.9, 118.1, 122.4, 84.5, 86.7,
           84.5, 77.0, 88.1, 82.4, 84.0, 88.8, 85.7, 78.8, 122.5, 110.0, 115.0, 125.0, 105.0,
           110.0, 115.0, 105.0, 108.5, 118.3, 84.5, 85.0, 85.5, 88.0, 86.7, 80.7, 88.3, 89.3, 94.3, 90.0)
cbind(sulcos,cortes, prod, rep)

