install.packages("dplyr")
library(dplyr)
library(ggplot2)
setwd("/")
setwd("Users/junior mejia/Desktop/ser/")
survey <- read.csv("survey.csv",header = T,sep = ",", encoding = "UTF-8")
survey[,names(survey)]
names(survey)
str(survey)
summary(survey)
summary(mtcars)
data.frame(columna.name = names(survey))
df <- data.frame(columna.name = names(survey))
write.csv(df,"columna_name.csv",row.names = FALSE)
row.names(df)
colum_names <- read.csv("columna_name_tratados.csv", header = T, sep = ";")
colum_names
names(survey) <- colum_names$traslation
names(survey)
survey
summary(cars)


as.data.frame(prop.table(table(survey$Area_sistema_dificulta)))
df_p <- as.data.frame(prop.table(table(survey$Area_sistema_dominada)))
df_p2 <- as.data.frame(prop.table(table(survey$Area_sistema_dificulta)))

boxplot(df_p2$Freq)
hist(df_p$Freq)


df_p <- df_p %>% arrange(Freq)
df_p2 <- df_p2 %>% arrange(Freq)
qqnorm(df_p$Freq)
boxplot(df_p$Freq)
df_p2 [df_p2$Var1 %in% c("Base de datos"),"categoria"]<- "Bases"
df_p2 [df_p2$Var1 %in% c("Desarrollo"),"categoria"]<- "Desarrollo"
df_p2 [df_p2$Var1 %in% c("Infraestructura","Redes de datos"),"categoria"]<- "Infraestructura"
##df_p2 [df_p2$Var1 %in% c("Matemáticas"),"categoria"]<- "Otras areas"
##df_p2 [df_p2$Var1 %in% c("Matemáticas"),"categoria"]<- "otras"

df_p [df_p$Var1 %in% c("Desarrollo;Base de datos;Infraestructura","Desarrollo;Base de datos;Infraestructura;Análisis y diseño","Desarrollo;Base de datos;Infraestructura;Lo que se venga"), "categoria"]<- "todas las areas"
df_p [df_p$Var1 %in% c("Desarrollo;Infraestructura"),"categoria"]<- "desarrollo e infraestructura"
df_p [df_p$Var1 %in% c("Base de datos;Infraestructura"),"categoria"]<- "bases e infraestructura"
df_p [df_p$Var1 %in% c("Desarrollo;Base de datos"),"categoria"]<- "bases y desarrollo"
df_p [df_p$Var1 %in% c("Desarrollo"),"categoria"]<- "Desarrollo"
df_p [df_p$Var1 %in% c("Base de datos"),"categoria"]<- "Bases"
df_p [df_p$Var1 %in% c("Infraestructura"),"categoria"]<- "Infraestructura"

summary(df_p)
median(df_p2$Freq)

print("max")
max(df_p$Freq) 

prop.table(table(survey$Area_sistema_dificulta))


survey <- left_join(survey,df_p2,by=c("Area_sistema_dificulta"="Var1")) 
survey <- left_join(survey,df_p,by=c("Area_sistema_dominada"="Var1")) 
survey$Area_sistema_dominada
df_p <- df_p %>% select(Var1,categoria)
survey <- survey[,!(names(survey) %in% c("Freq"))] %>% names
prop.table(table(survey$categoria))
boxplot()

as.data.frame(prop.table(table(survey$categoria)))
survey <- survey[,!(names(survey) %in% c("Area_sistema_dificulta"))] %>% names
prop.table(table(survey$categoria))
boxplot()
as.data.frame(prop.table(table(survey$Area_sistema_dificulta)))
names(survey)[11] <- "rango_indice"


## VALORES NA
for ( myname in names(survey)){
  print(myname)
  print(prop.table(table(is.na(survey[,myname]))))
  
  sr <- as.data.frame(prop.table(table(is.na(survey[,myname]))))
  opr <- sr %>% filter(Var1 == TRUE) %>% select(Freq)
  
  length(opr$Freq)
  df_temporal <- data.frame(column.name=c(myname), na.porcentage = ifelse(length(opr$Freq) == 0, 0, opr$Freq[1]) )
  
  na.sumy <- rbind(na.sumy, df_temporal)
  
}
na.sumy <- c()
#sr <- as.data.frame(prop.table(table(is.na(survey[,myname]))))
#opr <- sr %>% filter(Var1 == TRUE) %>% select(Freq)

#length(opr$Freq)
#df_temporal <- data.frame(column.name=c(myname), na.porcentage = ifelse(length(opr$Freq) == 0, 0, opr$Freq[1]) )

## TRATAMIENTO de NA
na.sumy <- rbind(na.sumy, df_temporal)
na.sumy %>% arrange(-na.porcentage) %>% filter(na.porcentage > 0)
survey$Area_sistema_dificulta
pru <- as.data.frame(prop.table(table(survey$Area_sistema_dificulta)))
hist.default(pru$Freq)



x <- survey %>% filter(!is.na(survey$Area_sistema_dominada))
mda <- median(x$hora_investigacion)
table(is.na(survey$Area_sistema_dominada))
##los valores nulos de la variable Area_sistema_dominada fueron sustituidos en el promedio
survey[is.na(survey$Area_sistema_dominada), "Area_sistema_dominada"] <- mda
table(is.na(survey$Area_sistema_dominada))
boxplot()
prue

x <- survey$nuevas_habilidades_autodidacta %>% c("no") <- "si"
names(survey)

x2 <- survey %>% filter(!is.na(survey$Area_sistema_dificulta))
mda2 <- median(x2$Area_sistema_dominada)
table(is.na(survey$Area_sistema_dificulta))
##los valores nulos de la variable Area_sistema_dificulta fueron sustituidos por 0
survey[is.na(survey$Area_sistema_dificulta), "Area_sistema_dificulta"] <- mda2
survey[is.na(survey$Area_sistema_dificulta), "Area_sistema_dificulta"] <- "Desarrollo"
pp6<- prop.table(table(survey$nuevas_habilidades_autodidacta))
x$Area_sistema_dificulta
prue
na[is.na(na$categoria), "categoria"] <- "no"
ggplot(x2) + 
  aes(x= Area_sistema_dificulta, fill= )+
  geom_bar(position = "stack")+
  theme(axis.text = element_text(angle = 45))

dif <- as.data.frame(prop.table(table(survey$Area_sistema_dificulta)))
etr <- as.data.frame(prop.table(table(survey$Area_sistema_dominada)))
Hab <- as.data.frame(pp3)
qqnorm(etr$Freq)
boxplot(dom$Freq)
hist(dif$Freq)

###survey$Area_sistema_dificulta
grafica <- barplot(pp, las=1, main = "Areas de mayor dificultad segun los aspirantes") 
text(grafica,c(0,0), round(pp,3))
##
grafica2 <- barplot(pp2, las=1, main = "Areas de mejor dominio entre los aspirantes") 
text(grafica2,c(0,0), round(pp2,3))
##
grafica3 <- barplot(pp3, las=1, main = "Habilidades adquiridas de forma autodidacta") 
text(grafica3,c(0,0), round(pp3,3))
##
grafica5 <- barplot(pp5, main = "Entrevistas laborales realizadas por los aspirantes") 
text(grafica5,c(0,0), round(pp5,3))
##
grafica6 <- barplot(pp6, main = "Aspirantes rechasados") 
text(grafica6,c(0,0), round(pp6,3))


## correlaciones
ggplot(survey) +
  aes(x= survey$Area_sistema_dominada, fill= rechazo_en_entrevista)+
  geom_bar(position = "stack")+
  theme(axis.text = element_text(angle = 45))

chisq.test(table(survey$Area_sistema_dominada,survey$rechazo_en_entrevista))


