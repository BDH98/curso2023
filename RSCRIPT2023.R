# INSTALAR PAQUETES
install.packages("tidyverse")
install.packages("gapminder")
installed.packages("viridis")
installed.packages("ggthemes")
installed.packages("datos")
install.packages("skimr")
installed.packages("janitor")
install.packages("palemrpenguins")

# Tambien se pueden instalar los paquetes desde: Tools > Install packages > paq


# CARGAR LIBRERIAS
library(tidyverse)
library(gapminder)
library(viridis)
library(ggthemes)
library(datos)
library(skimr)
library(janitor)
library(palmerpenguins)
library(here)
library(ggplot2)
library(dplyr)

# VER VERSION DE R
R.version

# Escribir CRAN para describir 

# ATAJOS:
# alt + -
# ctrl + enter
# shift + ctrl + m --> sirve para concatenar funciones

# BORRAR DATOS QUE NO QUERAMOS
na.rm=

# DIRECTORIO DE TRABAJO
getwd()

# PARA GUARDAR SET DE DATOS
write.csv(objeto, "nombre del archivo.csv")

# NUEVO DIRECTORIO
setwd()
# VER ARCHIVOS DEL DIRECTORIO
list.files()

# CREAR UN OBJETO

miprimerobjeto <- 54
misegundoobjeto <- "hola"
miprimerobjeto

# VER QUE TIPO DE VARIABLE ES:
class(miprimerobjeto)
class(misegundoobjeto)

# VECTORES
NOMBRES <- c("raul", "diego", "maria", "fernanda", "pablo")
class(NOMBRES)

PESO <- c(58,65,89,75,56)
ALTURA <- c(1.65,1.45,1.84,1.56,1.75)

# CREAR UNA FUNCION, EJEMPLO EL IMC
IMC <- PESO/ALTURA^2
IMC

sum(PESO)
length(PESO)

MEDIA_PESO <- sum(PESO)/length(PESO)

# CREAR UN DATA FRAME
NOMBRES_ALTURA_PESO <- data.frame(NOMBRES, PESO, ALTURA, IMC)

NOMBRES_ALTURA_PESO

# SELECCIONAR ELEMENTOS DE NUESTRO DATA FRAME O VECTORES
PESO[4]
PESO[-4]
PESO[2:4]
PESO[c(1,3)]

# HACER UNA SECUENCIA DE NUMEROS
SECUENCIA <- (1:10)
# HACER UN AECUENCIA POR RANGO
seq(1,10, by=0.5)

# VER FILAS DE UN DATA FRAME
nrow(NOMBRES_ALTURA_PESO)
# VER COLUMNAS DEL DATA FRAME
ncol(NOMBRES_ALTURA_PESO)

# BASE DE DATOS YA PRECARGADA
data("iris")
# DAR UN VISTAZO DE LAS PRIMERAS CINCO OBSERVACIONES/cabeza
head(iris)
# DAR UN VISTAZO DE LAS ULTIMAS CINCO OBSERVACIONES/cola
tail(iris)
# DAR UN VISTAZO DE COMO ESTAN CONFORMADOS LOS DATOS
glimpse(iris)
# VER LA ESTRUCTURA DE BASE DE DATOS
str(iris)
# SELECCIONAR ELEMENTOS
LARGOSEPALO <- iris$Sepal.Length
summary(LARGOSEPALO)
summary(iris)

# OPERADORES ARITMETICO
# SUMA +
# RESTA -
# DIVISION /
# MULTIPLICACION *
# EXPONENTE ^
# RESIDUO DE UNA DIVISION %%

O1 <- 5+6
VECTOR1 <- c(4,5,6,6)
VECTOR2 <- c(8,4,6,2)
VECTOR1+VECTOR2

# OPERADORES BOOLEANOS O CONDICIONALES
5==4
5==5
OBJETO1 <- 5
OBJETO2 <- 11
OBJETO1==OBJETO2
OBJETO1!=OBJETO2
OBJETO1>=OBJETO2
OBJETO1<=OBJETO2

# TRABAJADO CON PALMERPENGUINS 
# USAREMOS JANITOR Y SKIMR

# LIMPIEZA DE DATOS

data("penguins")
# NOS DA INFORMACION RESPECTO A LAS VARIABLES
skim_without_charts(penguins)
glimpse(penguins)

# Seleccionar especies
penguins %>% select(species)

# Seleccionar todas las variables menos especie
penguins %>% 
  select(-species) %>% 
  rename(isla = island)

view(penguins)
unique(penguins$species)
unique(penguins$island)
colnames(penguins)
penguins %>% 
  distinct(species)

# LIBRERIA JANITOR
rename_with(penguins,toupper)
rename_with(penguins, tolower)

# FUNCION PARA LIMPIAR EL NOMBRE DE NUESTRAS VARIABLES
dataset_limpio <- clean_names(penguins)

# LIMPIAR DE NUMERO O COSAS QUE NO QUEREMOS
penguins %>% 
  drop_na()

MAYOREASA35 <- penguins %>% 
  filter(bill_length_mm>35) %>% 
  drop_na()
view(MAYOREASA35)

is.na(MAYOREASA35)
is.na(penguins)

VERPERDIDOS <- sum(is.na(penguins))
VERPERDIDOS

# DPLYR: TIENE UNA HERRAMIENTA PARA CONCATENAR NUESTRAS FUNCIONES: %>% 

#FILTER: ASIGNANDO UN OBJETO, VECTOR, DATA FRAME, LUEGO SE AGREGA LA FUNCION
# OBJETO %>% FILTER(ATRIBUTO<=1)

#MUTATE: CREAR NUEVAS VARIABLES
# OBJETO %>% MUTATE(ATRIBUTONUEVO=ATRIBUTO ORIGINAL)

#GROUP_BY: AGRUPA VARIABLES CATEGORICAS, MEDIAS, MINIMOS, MAXIMOS
# OBJETO %>% GROUP_BY(ATRIBUTO) %>% 
#   SUMMARISE(ATRIBUTO_MEDIA=MEAN(ATRIBUTOORIGINAL))
# LA RAYA SIGNIFICA | "ESTO O EL OTRO"

# ORGANIZAR DATOS POR TAMAÑO / LARGO DEL PICO

penguins %>% 
  arrange(bill_length_mm)

penguins %>% 
  group_by(island) %>% 
  drop_na %>%
  summarize(mean_largopico = mean(bill_length_mm))

penguins %>% 
  group_by(island) %>% 
  drop_na %>%
  summarize(max_largopico = max(bill_length_mm))

# FILTRAR DATOS DE ESPECIE ADELIE
SOLOADELIE <- penguins %>% 
  filter(species == "Adelie") %>% 
  drop_na()

view(SOLOADELIE)

SOLOADELIEDREAM <- penguins %>% 
  filter(species == "Adelie", island=="Dream") %>% 
  drop_na()

# CONVERTIR VARIABLES / CAMBIO A KG
SOLOADELIE <- SOLOADELIEDREAM %>% 
  mutate(pesokg=body_mass_g/1000, aleta_m=flipper_length_mm/1000)

view(SOLOADELIE)

# DATA SET GAPMINDER

data("gapminder")
?gapminder
skim_without_charts(gapminder)

# FILTRAR INFORMACIÓN / DEL CONTINENTE EUROPEO 2007

EUROPA2007 <- gapminder %>% 
  filter(continent=="Europe"&year==2007)
view(EUROPA2007)
mean(EUROPA2007$lifeExp)
mean(EUROPA2007$pop)

EUROPA1957 <- gapminder %>% 
  filter(continent=="Europe"&year==1957)
view(EUROPA1957)
mean(EUROPA1957$lifeExp)
mean(EUROPA19577$pop)

EUROPA_ASIA <- gapminder %>% 
  filter(continent=="Europe" | continent=="Asia")

summary(EUROPA_ASIA)

gapminder %>% mutate(gdp=gdpPercap*pop)

gapminder %>% 
  filter(gdpPercap>800) %>% 
  mutate(gdp=gdpPercap*pop)

gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(year) %>% 
  summarise(exp_mean=mean(lifeExp, na.rm = T))

gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summarise(exp_mean=mean(lifeExp, na.rm = T)) %>% 
  arrange(desc(exp_mean))

# ARRANGE: ARREGLARME U ORDENARME LOS DATOS DE MENOR A MAYOR
# O CON ARRAGE(DESC()) LO PONDRA DE MAYOR A MENOR

exp_africa <- gapminder %>% 
  filter(continent=="Africa") %>% 
  group_by(country) %>% 
  summarise(exp_mean=mean(lifeExp, na.rm = T),
            exp_median=median(lifeExp, na.rm = T)) %>% 
  arrange(desc(exp_mean)) %>% 
  arrange(desc(exp_mean))

getwd()
write.csv(exp_africa,"expectativaafrica.csv")
list.files()

# GUARDAR EN EXCEL CON CSV

# GRAFICOS BÁSICOS EN R --> salen en plots

# HISTOGRAMA
#hist(setdedatos$variable)
hist(gapminder$pop)
hist(gapminder$lifeExp)
hist(gapminder$lifeExp, col="pink", xlab = "Expectativa de vida (años)",
     ylab = "Frecuencia", main = "Histograma de expectativa de vida")
plot(gapminder$gdpPercap, gapminder$lifeExp, col="pink")

# GRAFICA EN GGPLOT

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  ylab("Expectativa de vida")+
  xlab("gdp per capita")

# CON LOGARITMO
ggplot(gapminder, aes(x=log(gdpPercap), y=lifeExp))+
  geom_point()+
  ylab("Expectativa de vida")+
  xlab("gdp per capita")

# AGREGANDO ESCALA
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  ylab("Expectativa de vida")+
  xlab("GPD per capita")+
  scale_x_log10()+
  theme_dark()
  ggtitle("Relacion entre GDP per capita y expectativa de vida")

# FILTRADO POR CONTINENTE
  gapminder %>% filter(continent=="Americas") %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp))+
    geom_point()+
    ylab("Expectativa de vida")+
    xlab("gdp percapita")+
    scale_x_log10()+
    theme_grey()+
    ggtitle("Relacion entre GDP per capita y expectativa de vida")

# SE PUEDE USAR DPLYR Y GGPLOT JUNTOS
# SE PUEDEN AGREGAR MAS DIMENSIONES

#TAMAÑO DE POBLACION
  gapminder %>% filter(continent=="Americas") %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, size=pop))+
    geom_point()+
    ylab("Expectativa de vida")+
    xlab("gdp percapita")+
    scale_x_log10()+
    theme_grey()+
    ggtitle("Relacion entre GDP per capita y expectativa de vida")
  
# si yo quisiera ver la poblacion por pais
  gapminder %>% filter(continent=="Americas") %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
    geom_point()+
    ylab("Expectativa de vida")+
    xlab("gdp percapita")+
    scale_x_log10()+
    theme_grey()+
    ggtitle("Relacion entre GDP per capita y expectativa de vida")
# filtre por año
  
  #2007
  gapminder %>% filter(continent=="Americas" & year==2007) %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
    geom_point()+
    ylab("Expectativa de vida")+
    xlab("gdp percapita")+
    scale_x_log10()+
    theme_grey()+
    ggtitle("Relacion entre GDP per capita y expectativa de vida")
  #1957
  gapminder %>% filter(continent=="Americas" & year==1957) %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
    geom_point()+
    ylab("Expectativa de vida")+
    xlab("gdp percapita")+
    scale_x_log10()+
    theme_grey()+
    ggtitle("Relacion entre GDP per capita y expectativa de vida")
  
# ver la expectativa de vida por año de cada país / facet_ wrap
  gapminder %>% filter(continent=="Americas") %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=country))+
    geom_point()+
    ylab("Expectativa de vida")+
    xlab("gdp percapita")+
    scale_x_log10()+
    scale_colour_viridis(option = "D", discrete = TRUE)+
    theme_grey()+
    facet_wrap(~year)+
    ggtitle("Relacion entre GDP per capita y expectativa de vida")
  
# EXPECTATIVA DE VIDA POR CONTINENTE DEL AÑO 2007
gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(x=continent, lifeExp, fill=continent))+
  geom_boxplot()+
  theme_economist_white()+
  ylab("Expectativa de vida")+
  xlab("Continente")+
  labs(fills="Contiente")

# CONVERTIR A BOX PLOTS
gapminder %>% 
  filter(continent=="Asia") %>% 
  mutate(year1=as.factor(year)) %>% 
  ggplot(aes(x=year1, lifeExp, fill=year1))+
  geom_boxplot()+
  theme_economist_white()+
  ylab("Expectativa de vida")+
  xlab("Continente")+
  labs(fills="Contiente")

# SACAR LAS MEDIAS Y GRAFICAS CON PUNTOS
gapminder %>% 
  filter(continent=="Asia") %>% 
  group_by(year) %>% 
  summarise(mean_lifeexp=mean(lifeExp, na.rm = T)) %>% 
  ggplot(aes(x=year, y=mean_lifeexp))+
  geom_point()+
  geom_line()+
  theme_classic()+
  ylab("Expectativa de vida")+
  xlab("Año")+
  ggtitle("Cambios en la expectativa de vida por año")+
  labs(subtitle = "Datos de gapminder",
       caption = "Visualizado por Brenda D")+
  geom_vline(xintercept = 1990, linetype="dotted", color="blue")

?ggthemes

# COMIENZO CON GITHUB

# R > TOOLS > GLOBAL options > GIT SVN > BUSCAR > CARPETA DONDE SE DESCARGO GIT
#   IR A LA TERMINAL

# git es un sistema de control de versiones,
# existe un flujo de trabajo
# 1. directorio de trabajo
# 2. stage
# 3. repositorio local
# 4. repositorio remoto

# git add > git comit > git push >

# git init
# git config --global user.name Brend duana
# git config --global user.email brenda.duana.h@gmail.com
# git branch -M main
# git remote add origin +link
# git remote -v
# git status
# git add.
# modifique un código para guardar en Save As


