
Esta es una pequeña guía de uso para la aplicación NPS.
https://a-est-gal.shinyapps.io/npsapp/

################################################################################

INICIO

En esta sección se describe el funcionamiento general de la aplicación y se le da la bienvenida al usuario. 
También se muestran datos de interés como el contacto del desarrollador de la aplicación. 

################################################################################

APRENDE

El objetivo principal de esta sección es ofrecer al usuario un entorno en el que adquirir conocimientos acerca de las principales técnicas no paramétricas.

En cada uno de estos apartados se permite generar una o varias muestras aleatorias seleccionando algunas opciones en el panel lateral, como el tipo de variable o el tamaño muestral.

En el panel principal se puede ver la muestra aleatoria obtenida y el desarrollo de la prueba no paramétrica correspondiente paso a paso, con los resultados intermedios. Si se quisieran ocultar estos resultados para, por ejemplo, practicar resolviendo la técnica a mano, pueden deshabilitarse las casillas del panel lateral.

Además, en el desarrollo de cada técnica en el panel principal, se ofrecen hipervínculos con las tablas necesarias para el proceso de las pruebas y algunos gráficos que suelen acompañar a cada tipo de contraste.

Los apartados que se incluyen en esta sección son:

- Aleatoriedad: Rachas W-W 
Prueba de las rachas de Wald-Wolfowitz para contrastar la aleatoriedad de una muestra.
- Bondad de Ajuste: K-S (F=F0)
Prueba de Kolmogorov-Smirnov para contrastar la bondad de ajuste de una muestra auna distribución teórica conocida.
- Bondad de Ajuste: K-S (F=G)
Prueba de Kolmogorov-Smirnov para contrastar si dos muestras independientes proceden de la misma distribución de probabilidad.
- Localización de una muestra: W
Prueba de los rangos con signo de Wilcoxon para contrastar la localización de la mediana.
- Dos muestras independientes: U M-W
Prueba U de Mann-Whitney para comparar dos muestras independientes. 
- Dos muestras relacionadas: Apareados W
Prueba de los rangos con signo de Wilcoxon para comparar dos muestras relacionadas/pareadas.
- K muestras independientes: K-W
Prueba de Kruskal-Wallis para comparar k>2 muestras independientes.
- K muestras relacionadas: Friedman
Prueba de Friedman para comparar k>2 muestras relacionadas/pareadas.
- Independencia: Kendall
Prueba de Kendall para contrastar la concordancia de dos muestras.
- Independencia: Spearman
Prueba de Spearman para contrastar la independencia de dos muestras.

################################################################################

DATOS

El objetivo principal de la sección "Datos" es permitir al usuario seleccionar el conjunto de datos que quiere utilizar en la sección "Análisis". 

En el panel lateral el usuario puede seleccionar en el menú "Dataset" si quiere utilizar el conjunto de datos por defecto llamado "Empleados" o quiere importar un archivo desde su dispositivo.

La base de datos de "Empleados" consta de doce variables medidas en 64 individuos. Estos valores fueron obtenidos con una encuesta realizada por una empresa a sus trabajadores. Esta base de datos es muy variada y posibilita al usuario poder aplicar todas las técnicas de la sección "Análisis" a un conjunto de datos sin necesidad de importar uno propio.  

Si el usuario selecciona la opción de importar un archivo, debe incluir éste en el widget "Subir Archivo", clicando el botón "Examinar..." y seleccionando el archivo que quiera importar. 

La aplicación admite cuatro tipos de archivos (se selecciona en el menú "Tipo de Archivo"):
- .txt (TEXTO)
- .csv (CSV)
- .xls o .xlsx (Excel)
- .sav (SPSS)

Si los nombres de las variables se encuentran en la primera fila se debe marcar la casilla correspondiente en el panel lateral.

Para los archivos .txt se permite especificar el tipo de separados y el tipo de decimal utilizado.

Una vez seleccionados correctamente todos los parámetros necesarios en el panel lateral, se mostrará en el panel principal el conjunto de datos importado, ofreciendo al usuario la posibilidad de ordenar los datos por los valores de las distintas variables, aplicar algún filtro o limitar el número de entradas que se muestran en pantalla. Al final del panel principal se muestran las dimensiones del archivo importado.

################################################################################

ANÁLISIS

El objetivo principal de esta sección es ofrecer al usuario un entorno en el que poder aplicar las principales técnicas no paramétricas a un conjunto de datos propio o, en su defecto, al set de datos Empleados que ofrece NPS. 

Al acceder mediante la barra de navegación principal a la sección “Análisis” se despliega un menú en el que seleccionar a qué prueba/apartado acceder. En cada uno de estos apartados se ofrece al usuario la posibilidad de seleccionar, en el panel lateral izquierdo, la variable o variables a las que se quiere aplicar el test y variar otro tipo de valores del contraste específico como, por ejemplo, el tipo de contraste (Bilateral, Unilateral superior, Unilateral inferior), si se desea el estadístico y el p-valor exactos o si se quiere realizar una corrección por continuidad.

En el panel principal se muestra el nombre de la prueba/apartado en el que se encuentra el usuario, las hipótesis del contraste en cuestión, los valores elegidos por el usuario en el panel lateral y los resultados de la prueba, que principalmente son el valor del estadístico, el p-valor asociado y un gráfico que acompaña al test.

Las pruebas no paramétricas que se incluyen en esta sección son las mismas que se incluyen en la sección "Aprende".

################################################################################

Contacto:
Alfredo del Río
Universidad de Salamanca, España
a.est.gal@gmail.com
