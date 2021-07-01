#Librerias necesarias en la APP

if(!requireNamespace("randtests", quietly = T)){
  install.packages("randtests")
}
if(!requireNamespace("shiny", quietly = T)){
  install.packages("shiny")
}
if(!requireNamespace("shinythemes", quietly = T)){
  install.packages("shinythemes")
}
if(!requireNamespace("readxl", quietly = T)){
  install.packages("readxl")
}
if(!requireNamespace("foreign", quietly = T)){
  install.packages("foreign")
}
if(!requireNamespace("car", quietly = T)){
  install.packages("car")
}

library(randtests) #rachas
library(shiny)
library(shinythemes)
library(readxl) #excel
library(foreign) #spss
library(car) #qqplots

### FUNCIONES PROPIAS

#Funcion que genera datos de n lanzamientos de una moneda
generar_moneda <- function(n){
  u <- runif(n)
  m <- ifelse(u>0.5,"C","X")
  return(m)
} 

#Funcion que genera n datos numericos entre 0 y 1
generar_numeros <- function(n){
  u <- runif(n,0,1)
  return(u)
}

#Funcion que transforma una variable discreta dicotomica (vector con 2 tipos de cadenas) en una variable con 1 y 0
rachas_discreta <- function(x){
  x <- ifelse(x == unique(x)[1], 1, 0)
  return(x)
}

# runs.test(rachas_discreta(x), threshold = 0.5, plot = T)

#Simular datos para bondad ajuste
sim_b_aj <- function(c="1", n1=round(runif(1,10,15),0), n2=round(runif(1,10,15),0)){
  u <- runif(1)
  landa <- round(runif(1,1,5),0)
  media <- round(runif(1,-50,50),0)
  desv <- round(runif(1,1,8),0)
  if(c=="1"){
    if(u<=0.25){
      return(list(datos=round(rexp(n1,landa),2), tipo="Exponencial", n=n1, landa=landa))
    } else if(u<=0.5){
      return(list(datos=round(rexp(n1,landa+3),2), tipo="Exponencial", n=n1, landa=landa))
    } else if(u<=0.75){
      return(list(datos=round(rnorm(n1,media,desv),2), tipo="Normal", n=n1, media=media, desv=desv))
    } else{
      return(list(datos=round(rnorm(n1,media+5,desv+2),2), tipo="Normal", n=n1, media=media, desv=desv))
    }
  } else if(c=="2"){
    if(u<=1/3){
      dist1 <- round(rnorm(n1,media,desv),2)
      dist2 <- round(rnorm(n2,media,desv),2)
      return(list(S=dist1,
                  G=dist2,
                  media=media, desv=desv, tipo="Normal"))
    } else if(u > 2/3){
      dist1 <- round(rexp(n1,landa),2)
      dist2 <- round(rexp(n2,landa),2)
      return(list(S=dist1,
                  G=dist2,
                  landa=landa, tipo="Exponencial"))
    } else {
      dist1 <- round(rnorm(n1,(media+5),(desv+2)),2)
      dist2 <- round(rnorm(n2,media,desv),2)
      return(list(S=dist1,
                  G=dist2,
                  landa=landa, media=media, desv=desv, tipo="Normal y Exponencial"))
    }
  } else {
    err <- "Tipo de ejercicio mal especificado, c != '1' , c != '2' "
    return(err)
  }
}

#funcion que devuelve la cantidad de valores que son menores o iguales que v (empirica F*)
cond <- function(xord,v){ 
  return(sum(xord<=v))
}
