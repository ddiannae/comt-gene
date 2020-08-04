library(readr)
library(MASS)
library(ggplot2)

data <- read_tsv("data/datos_comt.csv", 
                 col_names = c("folio", "sexo", "edad", "peso", "talla", "imc", "imc_categoria", 
                               "eat40", "eat40_categoria", "genotipo"), na = "NP", skip = 1)

##### eat40_categoria vs genotipo
eat40c_genotipo <- table(data$eat40_categoria, data$genotipo)
chisq.test(eat40c_genotipo)

chisq.test(eat40c_genotipo)
## Dejamos el homocigoto común y sumamos el heterocigoto y el homocigoto minoritario
eat40c_genotipo <- cbind(eat40c_genotipo[,1] + eat40c_genotipo[,3], eat40c_genotipo[,2])
colnames(eat40c_genotipo) <- c("SNP", "comun")
chisq.test(eat40c_genotipo)

##### Sexo vs genotipo
sexo_genotipo <- table(data$sexo, data$genotipo)
chisq.test(sexo_genotipo)

# Pearson's Chi-squared test with Yates' continuity correction
# data:  eat40c_genotipo
# X-squared = 13.027, df = 1, p-value = 0.000307
# Warning message:
#   In chisq.test(eat40c_genotipo) : Chi-squared approximation may be incorrect

# Pearson's Chi-squared test
# data:  sexo_genotipo
# X-squared = 0.20786, df = 2, p-value = 0.9013

#### IMC vs genotipo
imcc_genotipo <- table(data$imc_categoria, data$genotipo)
chisq.test(imcc_genotipo)

# Pearson's Chi-squared test
# data:  imcc_genotipo
# X-squared = 8.0812, df = 8, p-value = 0.4256
# Warning message:
# In chisq.test(imcc_genotipo) : Chi-squared approximation may be incorrect

## Reducimos las variables.
### Sumamos las columnas de homocigotos
imcc_genotipo <- cbind(imcc_genotipo[,1], imcc_genotipo[,2] + imcc_genotipo[,3])
colnames(imcc_genotipo) <- c("Homocigoto", "Heterocigoto")
### Sumamos las filas de obesidad y quitamos a los de bajo peso porque son muy pocos
imcc_genotipo <- rbind(imcc_genotipo[2,], imcc_genotipo[3,] + imcc_genotipo[4,], imcc_genotipo[5,])
rownames(imcc_genotipo) <- c("normal", "obesidad", "sobrepeso")

chisq.test(imcc_genotipo)
# Pearson's Chi-squared test
# data:  imcc_genotipo
# X-squared = 0.28662, df = 2, p-value = 0.8665
# Warning message:
# In chisq.test(imcc_genotipo) : Chi-squared approximation may be incorrect

# Quitamos las de obesidad
chisq.test(imcc_genotipo[c(1,3),])
# Pearson's Chi-squared test with Yates' continuity correction
# data:  imcc_genotipo[c(1, 3), ]
# X-squared = 0.024036, df = 1, p-value = 0.8768

imcc_sexo <- table(data$imc_categoria, data$sexo)
chisq.test(imcc_sexo)

# Pearson's Chi-squared test
# data:  imcc_sexo
# X-squared = 9.949, df = 4, p-value = 0.0413
# Warning message:
# In chisq.test(imcc_sexo) : Chi-squared approximation may be incorrect

## Otra vez quitamos bajo peso y obesidad
chisq.test(imcc_sexo[c(2,5), ])
# Pearson's Chi-squared test with Yates' continuity correction
# data:  imcc_sexo[c(2, 5), ]
# X-squared = 4.9408, df = 1, p-value = 0.02623


