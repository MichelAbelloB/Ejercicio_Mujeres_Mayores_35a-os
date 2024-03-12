
#https://www.kaggle.com/ronitf/heart-disease-uci?select=heart.csv

#1. Cuantas y cuales mujeres son mayores de 35 años?

library(tidyverse)
library(dplyr)

#1=male
#0=female

base_datos= read.csv("C:/Users/Michel Abello/Downloads/Primer entrega/heart.csv")
head(base_datos)

names(base_datos)
Numero_mujeres = filter(count(base_datos, sex==0))

mujeres_35 = filter(base_datos, sex==0,�..age>=35)
print(mujeres_35)

print(count(mujeres_35))

#0. Describa y reconozca el dataset (Nulidad, completitud,limpieza)

?datasets

library(help = "datasets")

#RESPUESTA: un dataset es un tipo de dato estructura, el cual tiene filas y 
#columnas de manera orgenada. Dentro de la clasificacion de un dataset se
#encuentra la nulidad de datos no relevantes, ya sean filas o columnas sin 
#relevancia. Como segundo encontramos completitud, lo cual hace referencia
#al relleno o complementacion de datos segun corresponda en la base y por 
#ultimo tenemos limpieza, el cual limpia la estructura de la base de datos
#con parametros logicos que ayuden a la correcta interpretacion de la matrix.

#2. �Quienes tienen fbs con valor 1 y no son mayores de 42?

hombres_42 = filter(base_datos, fbs==1,�..age<=42)
print(hombres_42)

print(count(hombres_42))

#3. Genere un �rbol de decisi�n para el sexo de los registros �Quitar�a 
#variables?�Por qu�?

library(rpart)
library(rattle)
library(rpart.plot)

arbol<-rpart(
  formula= cp ~ sex+�..age,
  data = base_datos,
  method = 'class'
)

fancyRpartPlot(arbol)

#En mi opinion todos los datos tienen un grado de importancia para el analisis
#final de un modelo. Sin embargo, para el arbol de desicion planteado en este 
#momento es necesario los datos mas relevantes, como lo es el genero, edad, 
#tipo de dolor de pecho.

#4. Prediga la edad de las personas a partir de la variable que considera m�s 
#relevante

#�..age=edad
#sex=genero
#cp=tipo de dolor de pecho
#trestbps=presi�n arterial en reposo
#chol=colestoral s�rica en mg / dl
#fbs=(az�car en sangre en ayunas & gt; 120 mg / dl) (1 = verdadero; 0 = falso)
#restecg=resultados electrocardiogr�ficos en reposo
#thalach=frecuencia card�aca m�xima alcanzada
#exang=angina inducida por ejercicio (1 = s�; 0 = no)
#oldpeak=Depresi�n del ST inducida por el ejercicio en relaci�n con el reposo

cor.test(base_datos$cp, base_datos$thalach)

modelo_prediccion<- lm(base_datos$cp, base_datos$thalach)

#5. �C�mo agrupar�a a los individuos? �Por qu�?

#En este caso agruparia a los inviduos segun su genero, despues segun el 
#intervalo de edad y por ultimo por tipo de dolor. Ya que dependiendo del genero
#ciertos individuos padeceran mas de un dolor y segun su intervalo de edad
#su tipo de dolor sera mas frecuente en ciertas edades.

#6. �Cu�l es el valor medio del thalach por tipo de target?

Num_mujeres = filter(base_datos, sex==0)
Num_hombres = filter(base_datos, sex==1)

summary(Num_hombres$thalach)
summary(Num_mujeres$thalach)

#7. �Cu�l es la mediana y el log del m�ximo del chol por sexo? �C�mo se pueden 
#interpretar estos valores?
  
summary(Num_hombres$chol)
summary(Num_mujeres$chol)

#existen valores semejantes en los dos grupos, si entendemos que existen mas 
#mujeres que hombres dentro de la muestra
