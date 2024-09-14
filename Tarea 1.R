#------------------------------------------------------------------------------#
library(readxl)
library(dplyr)
library(ggplot2)
library(officer)
library(flextable)
dt_cpi <- read_excel("tarea1TEFI/cpi_2015.xlsx")
dt  <- read_excel("tarea1TEFI/data_tarea_1.xlsx")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 1
#primero vamos a seleccionar las siguientes acciones por medio de su cusip
#------------------------------------------------------------------------------#
stock1<-"01623010"#alico
stock2<-"02358610"#Amerco
stock3<-"07745410"#BELDEN
#------------------------------------------------------------------------------#
dt_s1<- filter(dt, cusip == stock1)
dt_s2<- filter(dt, cusip == stock2)
dt_s3<- filter(dt, cusip == stock3)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 2 EN EL INFORME
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 3
#primero se pide calcular retornos simples considerando dividendos
#para ello se va a usar la fórmula (1+R_t)=(p_t+div_t)/p_t-1
# en la base de datos ya tengo div_t y p_t, tengo que crear una columna con los p_t-1
#------------------------------------------------------------------------------#
#primero hacer los cálculos mensuales
dt_s1$p_t_1<-dt_s1$prc
dt_s1<-dt_s1 %>%
  mutate(p_t_1 = lag(prc, n = 1)) %>%
  mutate(r_bruto=round((prc+divamt)/p_t_1,4)) %>%
  mutate(r_neto=(r_bruto-1)*100)
#AHORA anualizamos
dt_s1_anual<-dt_s1 %>%
  group_by(year) %>%
  summarise(r_bruto_anual=round((prod(r_bruto)),4))%>%
  mutate(r_neto_anual=(r_bruto_anual-1)*100)
  
tab_s1_3 <- dt_s1_anual %>%
  summarise(
    Media = round(mean(r_neto_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_neto_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_neto_anual))),2),
    Desviacion_Estandar = round(sd(r_neto_anual, na.rm = TRUE),2),
    Minimo = min(r_neto_anual, na.rm = TRUE),
    Maximo = max(r_neto_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_neto_anual))
  )
doc <- read_docx()#abriendo el doc
doc <- body_add_par(doc, 
                    value = "Tabla 1: Retorno nominal acción 1 ", 
                    style = "heading 1")
tabla1 <- qflextable(tab_s1_3)
doc <- body_add_flextable(doc, value = tabla1)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
dt_s2$p_t_1<-dt_s2$prc
dt_s2<-dt_s2 %>%
  mutate(p_t_1 = lag(prc, n = 1)) %>%
  mutate(r_bruto=round((prc+divamt)/p_t_1,4)) %>%
  mutate(r_neto=(r_bruto-1)*100)
dt_s2_anual<-dt_s2 %>%
  group_by(year) %>%
  summarise(r_bruto_anual=round((prod(r_bruto)),4))%>%
  mutate(r_neto_anual=(r_bruto_anual-1)*100)

tab_s2_3 <- dt_s2_anual %>%
  summarise(
    Media = round(mean(r_neto_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_neto_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_neto_anual))),2),
    Desviacion_Estandar = round(sd(r_neto_anual, na.rm = TRUE),2),
    Minimo = min(r_neto_anual, na.rm = TRUE),
    Maximo = max(r_neto_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_neto_anual))
  )

doc <- body_add_par(doc, 
                    value = "Tabla 2: Retorno nominal acción 2 ", 
                    style = "heading 1")
tabla2 <- qflextable(tab_s2_3)
doc <- body_add_flextable(doc, value = tabla2)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
dt_s3$p_t_1<-dt_s3$prc
dt_s3<-dt_s3 %>%
  mutate(p_t_1 = lag(prc, n = 1)) %>%
  mutate(r_bruto=round((prc+divamt)/p_t_1,4)) %>%
  mutate(r_neto=(r_bruto-1)*100)
dt_s3_anual<-dt_s3 %>%
  group_by(year) %>%
  summarise(r_bruto_anual=round((prod(r_bruto)),4))%>%
  mutate(r_neto_anual=(r_bruto_anual-1)*100)
tab_s3_3 <- dt_s3_anual %>%
  summarise(
    Media = round(mean(r_neto_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_neto_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_neto_anual))),2),
    Desviacion_Estandar = round(sd(r_neto_anual, na.rm = TRUE),2),
    Minimo = min(r_neto_anual, na.rm = TRUE),
    Maximo = max(r_neto_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_neto_anual))
  )

doc <- body_add_par(doc, 
                    value = "Tabla 3: Retorno nominal acción 3 ", 
                    style = "heading 1")
tabla3 <- qflextable(tab_s3_3)
doc <- body_add_flextable(doc, value = tabla3)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 4
#Realizar test de hipotesis media 0
t_test_s1 <- t.test(dt_s1_anual$r_neto_anual, mu = 0)
t_test_s2 <- t.test(dt_s2_anual$r_neto_anual, mu = 0)
t_test_s3 <- t.test(dt_s3_anual$r_neto_anual, mu = 0)
test_hipotesis<-data.frame(
  valor_p=round(c(t_test_s1$p.value,t_test_s2$p.value,t_test_s3$p.value),2)
  )
test_hipotesis<-test_hipotesis%>%
  mutate(alfa_10 = ifelse(valor_p<=0.1,"Se rechaza H0 con 10% de significancia","No se rechaza H0"))%>%
  mutate(alfa_05 = ifelse(valor_p<=0.05,"Se rechaza H0 con 5% de significancia","No se rechaza H0"))%>%
  mutate(alfa_01 = ifelse(valor_p<=0.01,"Se rechaza H0 con 1% de significancia","No se rechaza H0"))
doc <- body_add_par(doc, 
                    value = "Tabla 4: Resultado test de hipótesis H0: media==0", 
                    style = "heading 1")
tabla4 <- qflextable(test_hipotesis)
doc <- body_add_flextable(doc, value = tabla4)
print(doc, target = "tabla_documento.docx")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 5
#calculamos el retorno logaritmico como r_t=log(1+Rt), es decir logaritmo del 
#retorno bruto
dt_s1<-dt_s1 %>%
  mutate(r_t = round(log(r_bruto),4)*100)
dt_s1_anual_log<-dt_s1 %>%
  group_by(year) %>%
  summarise(r_log_anual=round((sum(r_t)),4))
tab_s1_log<- dt_s1_anual_log %>%
  summarise(
    Media = round(mean(r_log_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_log_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_log_anual))),2),
    Desviacion_Estandar = round(sd(r_log_anual, na.rm = TRUE),2),
    Minimo = min(r_log_anual, na.rm = TRUE),
    Maximo = max(r_log_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_log_anual))
  )

doc <- body_add_par(doc, 
                    value = "Tabla 5: Retorno logarítmico acción 1 ", 
                    style = "heading 1")
tabla5 <- qflextable(tab_s1_log)
doc <- body_add_flextable(doc, value = tabla5)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
dt_s2<-dt_s2 %>%
  mutate(r_t = round(log(r_bruto),4)*100)
dt_s2_anual_log<-dt_s2 %>%
  group_by(year) %>%
  summarise(r_log_anual=round((sum(r_t)),4))
tab_s2_log<- dt_s2_anual_log %>%
  summarise(
    Media = round(mean(r_log_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_log_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_log_anual))),2),
    Desviacion_Estandar = round(sd(r_log_anual, na.rm = TRUE),2),
    Minimo = min(r_log_anual, na.rm = TRUE),
    Maximo = max(r_log_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_log_anual))
  )
doc <- body_add_par(doc, 
                    value = "Tabla 6: Retorno logarítmico acción 2 ", 
                    style = "heading 1")
tabla6 <- qflextable(tab_s2_log)
doc <- body_add_flextable(doc, value = tabla6)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
dt_s3<-dt_s3 %>%
  mutate(r_t = round(log(r_bruto),4)*100)
dt_s3_anual_log<-dt_s3 %>%
  group_by(year) %>%
  summarise(r_log_anual=round((sum(r_t)),4))
tab_s3_log<- dt_s3_anual_log %>%
  summarise(
    Media = round(mean(r_log_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_log_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_log_anual))),2),
    Desviacion_Estandar = round(sd(r_log_anual, na.rm = TRUE),2),
    Minimo = min(r_log_anual, na.rm = TRUE),
    Maximo = max(r_log_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_log_anual))
  )
doc <- body_add_par(doc, 
                    value = "Tabla 7: Retorno logarítmico acción 3 ", 
                    style = "heading 1")
tabla7 <- qflextable(tab_s3_log)
doc <- body_add_flextable(doc, value = tabla7)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PARTE 6
#primero calculamos la inflacion bruta mensual
dt_cpi<-dt_cpi %>%
  mutate(ipc_t_1 = ifelse(year>1995,lag(cpi, n = 1),cpi)) %>%
  mutate(inflacion_bruta=round((cpi/ipc_t_1),4))
#luego agregamos a la base de datos de cada accion y calculamos el retorno bruto real y neto real
dt_s1<-dt_s1 %>%
  mutate(inflacion_bruta=dt_cpi$inflacion_bruta)%>%
  mutate(r_real_bruto=round(r_bruto/inflacion_bruta,4))%>%
  mutate(r_real_neto=(r_real_bruto-1)*100)
dt_s1_anual_real<-dt_s1 %>%
  group_by(year) %>%
  summarise(r_real_bruto_anual=round((prod(r_real_bruto)),4),
            inflacion_anual=(round((prod(inflacion_bruta)),4)-1)*100)%>%
  mutate(r_real_neto_anual=(r_real_bruto_anual-1)*100)
  
tab_s1_real <- dt_s1_anual_real %>%
  summarise(
    Media = round(mean(r_real_neto_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_real_neto_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_real_neto_anual))),2),
    Desviacion_Estandar = round(sd(r_real_neto_anual, na.rm = TRUE),2),
    Minimo = min(r_real_neto_anual, na.rm = TRUE),
    Maximo = max(r_real_neto_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_real_neto_anual))
  )
doc <- body_add_par(doc, 
                    value = "Tabla 8: Retorno real acción 1 ", 
                    style = "heading 1")
tabla8 <- qflextable(tab_s1_real)
doc <- body_add_flextable(doc, value = tabla8)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
dt_s2<-dt_s2 %>%
  mutate(inflacion_bruta=dt_cpi$inflacion_bruta)%>%
  mutate(r_real_bruto=round(r_bruto/inflacion_bruta,4))%>%
  mutate(r_real_neto=(r_real_bruto-1)*100)
dt_s2_anual_real<-dt_s2 %>%
  group_by(year) %>%
  summarise(r_real_bruto_anual=round((prod(r_real_bruto)),4),
            inflacion_anual=(round((prod(inflacion_bruta)),4)-1)*100)%>%
  mutate(r_real_neto_anual=(r_real_bruto_anual-1)*100)

tab_s2_real <- dt_s2_anual_real %>%
  summarise(
    Media = round(mean(r_real_neto_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_real_neto_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_real_neto_anual))),2),
    Desviacion_Estandar = round(sd(r_real_neto_anual, na.rm = TRUE),2),
    Minimo = min(r_real_neto_anual, na.rm = TRUE),
    Maximo = max(r_real_neto_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_real_neto_anual))
  )
doc <- body_add_par(doc, 
                    value = "Tabla 9: Retorno real acción 2 ", 
                    style = "heading 1")
tabla9 <- qflextable(tab_s2_real)
doc <- body_add_flextable(doc, value = tabla9)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
dt_s3<-dt_s3 %>%
  mutate(inflacion_bruta=dt_cpi$inflacion_bruta)%>%
  mutate(r_real_bruto=round(r_bruto/inflacion_bruta,4))%>%
  mutate(r_real_neto=(r_real_bruto-1)*100)
dt_s3_anual_real<-dt_s3 %>%
  group_by(year) %>%
  summarise(r_real_bruto_anual=round((prod(r_real_bruto)),4),
            inflacion_anual=(round((prod(inflacion_bruta)),4)-1)*100)%>%
  mutate(r_real_neto_anual=(r_real_bruto_anual-1)*100)

tab_s3_real <- dt_s3_anual_real %>%
  summarise(
    Media = round(mean(r_real_neto_anual, na.rm = TRUE),2),
    Error_Estandar = round(sd(r_real_neto_anual, na.rm = TRUE)/sqrt(sum(!is.na(r_real_neto_anual))),2),
    Desviacion_Estandar = round(sd(r_real_neto_anual, na.rm = TRUE),2),
    Minimo = min(r_real_neto_anual, na.rm = TRUE),
    Maximo = max(r_real_neto_anual, na.rm = TRUE),
    Numero_Datos = sum(!is.na(r_real_neto_anual))
  )
doc <- body_add_par(doc, 
                    value = "Tabla 10: Retorno real acción 3 ", 
                    style = "heading 1")
tabla10 <- qflextable(tab_s3_real)
doc <- body_add_flextable(doc, value = tabla10)
print(doc, target = "tabla_documento.docx")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#ñau