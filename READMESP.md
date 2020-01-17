# swirl

[![Build Status](https://travis-ci.org/swirldev/swirl.png?branch=master)](https://travis-ci.org/swirldev/swirl)
[![CRAN version](http://www.r-pkg.org/badges/version/swirl?color=3399ff)](https://cran.r-project.org/package=swirl)
[![Downloads](http://cranlogs.r-pkg.org/badges/swirl?color=3399ff)](http://cran-logs.rstudio.com/)

### [http://swirlstats.com](http://swirlstats.com)

swirl es una plataforma para aprender (y enseñar) estadística y R simultánemamente 
e interactivamente. Este presenta una opción de lecciones e interactivamente  
tutorias a los estudiantes a través de los cursos. A los estudiantes se les puede pedir ver un video, 
responder preguntas de opción múltiple, llenar los espacios vacios de preguntas o ingresar un comando en  
la console R, precisamente como si el o ella estuvieran usando practicamente R. Se enfatiza en la  
en la interacción con la consola R. Las respuestas de los usuarios son verificadas y  
y se dan indicaciones en caso necesario. El progreso se guarda automáticamente de forma que el 
usuario puede salir en cualquier momento y retormar mas tarde sin perder el trabajo realizado.

swirl se apoya ampliamente en que los estudiantes practiquen utilizando la consola R. Un mecanismo 
de callback es usado para capturar la información ingresada por los esudiantes,  
y proporcionar retroalimentación inmediata y relevante al material del curso.


[swirlify](https://github.com/swirldev/swirlify) es un paquete separado que proporciona una 
herramienta comprensiva para los instructores de swirl. El contenido es creado en  
[YAML](http://en.wikipedia.org/wiki/YAML) usando las prácticas herramientas descritas en la pagina 
de [pagina para instructores](http://swirlstats.com/instructors.html) de nuestro sitio web.

El programa es iniciado con `swirl()`. Las funciones que controlan el comportamiento de swirl's 
incluyen `bye()` para salir, `skip()` para saltarse una pregunta, `main()` para 
regresar al menu principal, `play()` para experimentar en la consola de R sin la 
interferencia de swirl, `nxt()` para continuar con el uso de swirl, e 
`info()` para desplegar un menú de ayuda.


## Instalando swirl (desde CRAN)

La forma mas fácil para instalar y ejecutar swirl es tecleando las siguientes instrucciones desde la consola de R:

```
install.packages("swirl")
library(swirl)
swirl()
```

A medida que continuemos agregando nuevas funciones y contenido, continuaremos creando nuevas versiones 
disponibles en CRAN (mas probablemente cada 1 o 2 meses).

## Instalando la última versión de desarrollo (desde GitHub)

Para acceder a las funcionalidades y contenido mas reciente, se puede instalar y ejecutar la 
versión de desarrollo de swirl usando el paquete [devtools](https://github.com/hadley/devtools):

```
install.packages("devtools")
devtools::install_github("swirldev/swirl", ref = "dev")
library(swirl)
swirl()
```

## Contribuyendo con el desarrollo de swirl's

Se te gustaría estar involucrado, por favor haz 'fork' de este repositorio y envía una  
solicitud de 'pull' con tus cambios propuestos. Estaremos contentos de conversar si tu tienes alguna 
pregunta a cerca del código fuente.

## Usando swirl en el salon de clases

Instructores alrededor del mundo están usando swirl en su salón de clases. Nosotros pensamos que 
esto es asombroso. Si tu eres un instructor, por favor siéntete libre de hacer lo mismo -- libre de
cargos. Aunque tus estudiantes pueden estar pagando para tomar tu curso o por asistir a tu  
institución, lo único que te pedimos es que no le cobres a la gente *directamente* por el uso  
de nuestro software.

Si no estás seguro a cerca de un caso particular de uso, no dudes en enviarnos un correo 
electrónico a info@swirlstats.com.
