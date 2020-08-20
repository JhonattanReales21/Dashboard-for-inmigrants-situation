#############################################
##
##                Fecha: 19/08/2020
##    
##    Nombres: Viviana Florez & Jhonattan Reales
##
##      dplyr: Paquete base para un completo
##          Analisis exploratorio de datos
##
##############################################

# Paquetes/librerias para desarrollar la sesion

#install.packages("funModeling", dependencies = T)
#install.packages("Hmisc", dependencies = T)
#install.packages("dplyr", dependencies = T)
#install.packages("datos", dependencies = T)
#install.packages("fueleconomy", dependencies = T)

library(dplyr)
library(Hmisc)
library(funModeling)
library(datos)
library(fueleconomy)



#----------- Teoria de conjuntos y dplyr #-----------

# 1. Intersección
# 2. Unión
# 3. Complemento

# 1. 
#  filter(data, x1=10)
#  filter(data, x2="Masculino")
#  filter(data, x1=10 & x2="Masculino" )    |      filter(data, x1=10, x2="Masculino" )


# 2. 
#  filter(data, x1=10)
#  filter(data, x2="Masculino")
#  filter(data, x1=10 | x2="Masculino" )   

# 3. 
#  filter(data, x1=10)
#  filter(data, x2="Masculino")
#  filter(data, x1!=10, x2="Masculino")  

#    !(condicion logica)     !=

#     %in%  >    >=   <     <=     is.na()     !is.na()      !


#--------- Funciones interesantes de mutate() #-------

# min_rank   ->  ordenamiento

paises <- datos::paises
glimpse(paises)

rankeo <- mutate(paises, "Ranking"=min_rank(desc(paises$esperanza_de_vida)))
rankeo %>% filter(Ranking %in% 1:10) %>% View()

.
.
.
.
.



#----------- Utilizar dplyr para limpiar una base de datos #---------

# *** Utilizaremos los principios del complemento (!)

View(head(paises, 30))
str(paises)
levels(paises$continente)

paises2 <- paises %>% filter(continente=="Oceanía")
paises2 <- paises %>% filter(continente!="Oceanía")
paises2 <- paises %>% filter(!(continente=="Oceanía"))

paises3 <- paises %>% filter(continente=="Oceanía" | anio %in% 1950:1970)
paises3 <- paises %>% filter(!(continente=="Oceanía" | anio %in% 1950:1970))


.
.
.
.
.




#------------ Funciones que acompañan el proceso de EDA #-------------------

encuesta <- datos::encuesta

# levels() para factores
# hmisc::describe()
# funModeling::df_status()
# funModeling::freq()
# summary()

# y un largo largo etc....

Hmisc::describe(encuesta)
View(funModeling::df_status(encuesta))
funModeling::freq(encuesta$edad)
summary(encuesta$edad)

#------------ discretizar variables numericas #-------------

unique(encuesta$edad)

# Quiero discretizar la edad, como lo hago?

# Podemos hacer diferentes tipos de clases o conjuntos
            # 1. Misma amplitud
            # 2. Misma frecuencia
            # 3. Amplitud especifica para cada clase
            # 4. Realizados a tu medida

# 1.
?cut_interval
edad_discreta1 <- cut_interval(encuesta$edad, 6 )
View(edad_discreta1)
class(edad_discreta1)
levels(edad_discreta1)
forcats::fct_count(edad_discreta1)

# 2.
?equal_freq
edad_discreta2 <- equal_freq(encuesta$edad, 6 )
View(edad_discreta2)
class(edad_discreta2)
levels(edad_discreta2)
forcats::fct_count(edad_discreta2)

# 3.
?cut_width
edad_discreta3 <- cut_width(encuesta$edad, 10 )
View(edad_discreta3)
class(edad_discreta3)
levels(edad_discreta3)
forcats::fct_count(edad_discreta3)

# 4.
?cut
breaks <- c(15,25,55,95)
edad_discreta4 <- cut(encuesta$edad, breaks = breaks )
View(edad_discreta4)
class(edad_discreta4)
levels(edad_discreta4)
forcats::fct_count(edad_discreta4)


#--------- Solución ejercicios propuestos #----------

# 4. (vehicles) Hallar los cuartiles de la variable cty. 
# Pista: Realizar un “resumen” del data set y utilizarla función quantiles().

vehicles <- fueleconomy::vehicles
df_status(vehicles)

vehicles %>% summarise("Q1"=quantile(cty, 0.25), "Q2"=quantile(cty, 0.5), 
                       "Q3"=quantile(cty, 0.75), "Q4"=quantile(cty, 1)) %>% View()

quantile(vehicles$cty)


# 5. (vehicles) Excluyendo los vehiculos de años anteriores a 2005, agrupar 
# dependiendo de la clase del vehiculo y del numero de cilindros. 
# Hallar el valor maximo de cilindraje (displ) para cada grupo y ordenar de manera descendente.


vehicles %>% filter(!(year<2005)) %>% group_by(class, cyl)%>% summarise("Cilindraje maximo"=max(displ)) %>% 
      arrange(desc(`Cilindraje maximo`))%>% View()


#--------- Ejercicio avanzado #----------


