###################################################################################################
#                                                                                                 #
#                       Shiny APP      Alfredo del Rio Moldes      USAL                           #
#                                                                                                 #
###################################################################################################

source('funciones.R')

empleados <- read.table("empleados2.txt",header = T,sep = ";",dec = ",")
log <- as.vector(unlist(lapply(empleados, "is.character")))
empleados[log] <- lapply(empleados[log], "as.factor")

options(shiny.sanitize.errors = TRUE) #evita mensajes de error particulares (son todos generales)

##########################################################################################################
#                                                    UI                                                  #
##########################################################################################################

ui <- navbarPage("NPS", theme = shinytheme("cosmo"),
  tabPanel("Inicio",
      withMathJax(),
      tags$img(src = "usal.png", width = '15%', align="right", offset="2px"),
      h1(tags$b("NPS")),     
      h4("¡Bienvenido a NPS!"),
      "Esta aplicación web ofrece herramientas para aprender sobre algunas técnicas no paramétricas
      así como la posibilidad de importar un set de datos y aplicar test no paramétricos a estos datos importados.",
      br(),
      br(),
      "A continuación se describe el funcionamiento general de cada uno de los apartados principales de la aplicación.",
      br(),
      br(),
      h3(tags$u("Aprende")),
      "En la sección Aprende se explica el proceso de cada una de las pruebas no paramétricas con un ejemplo práctico,
      incluyendo las fórmulas utilizadas y enlaces a las tablas de cuantiles necesarias en cada caso.
      Es posible practicar estos ejercicios las veces que se necesite, ocultando las soluciones y generando nuevos datos. 
      Después, se muestran las soluciones para comprobar que se han realizado las cuentas correctamente y que se ha comprendido
      el proceso del test no paramétrico en cuestión. Además, este apartado ofrece enlaces en los que podemos encontrar más información
      sobre las pruebas.",
      br(),
      br(),
      "Los tests no paramétricos que se incluyen en este apartado son:",
      br(),
      br(),
      "- ",
      strong("Aleatoriedad: Rachas W-W \\(\\rightarrow\\) "), 
      "Contraste de aleatoriedad: Prueba de las rachas de Wald-Wolfowitz.",
      br(),
      "- ",
      strong("Bondad de Ajuste: K-S \\(F = F_{0}\\) \\(\\rightarrow\\) "), 
      "Contraste de bondad de ajuste: Prueba de Kolmogorov-Smirnov \\(F = F_{0}\\).",
      br(),
      "- ",
      strong("Bondad de Ajuste: K-S \\(F = G\\) \\(\\rightarrow\\) "), 
      "Contraste de bondad de ajuste: Prueba de Kolmogorov-Smirnov \\(F = G\\).",
      br(),
      "- ",
      strong("Localización de una muestra: W \\(\\rightarrow\\) "), 
      "Contraste de localización de una muestra: Prueba de los rangos con signo de Wilcoxon.",
      br(),
      "- ",
      strong("Dos muestras independientes: U M-W \\(\\rightarrow\\) "), 
      "Contraste para dos muestras independientes: Prueba U de Mann-Whitney.",
      br(),
      "- ",
      strong("Dos muestras relacionadas: Apareados W \\(\\rightarrow\\) "), 
      "Contraste para dos muestras relacionadas: Prueba para datos apareados de Wilcoxon.",
      br(),
      "- ",
      strong("k muestras independientes: K-W \\(\\rightarrow\\) "), 
      "Contraste para k muestras independientes: Prueba de Kruskal-Wallis.",
      br(),
      "- ",
      strong("k muestras relacionadas: Friedman \\(\\rightarrow\\) "), 
      "Contraste para k muestras relacionadas: Prueba de Friedman.",
      br(),
      "- ",
      strong("Independencia: Kendall \\(\\rightarrow\\) "), 
      "Contraste de independencia: Prueba de Kendall.",
      br(),
      "- ",
      strong("Independencia: Spearman \\(\\rightarrow\\) "), 
      "Contraste de independencia: Prueba de Spearman.",
      br(),
      br(),
      h3(tags$u("Datos")),
      "En la sección Datos se ofrece la posibilidad de importar un conjunto de datos. Los tipos de archivo que admite son:",
      tags$i(".txt, .csv, .xlsx .xls (Excel) y .sav (SPSS)."),
      br(),
      br(),
      "Si no se dispone de un conjunto de datos, este apartado permite utilizar el set de datos siguiente: ",
      br(),
      br(),
      "- ",
      strong("Empleados:"), 
      "Doce variables medidas en 64 individuos. Contiene los resultados acerca de una encuesta realizada por una empresa a sus trabajadores.",
      br(),
      br(),
      "Una vez importados, estos datos pueden utilizarse en la sección Análisis.",
      br(),
      br(),
      h3(tags$u("Análisis")),
      "En la sección Análisis es posible aplicar una serie de tests no paramétricos a los datos importados. 
      A diferencia del apartado Aprende, este apartado no muestra los resultados paso a paso de las técnicas, ya que no es el objetivo del mismo. 
      En cada técnica se muestran los valores elegidos y los resultados finales del test, generalmente el estadístico y el p-valor correspondiente, así como algún gráfico que pueda ser de utilidad.",
      br(),
      br(),
      "Las pruebas no paramétricas incluidas en este apartado son las mismas que las incluidas en la sección Aprende.",
      br(),
      br(),
      hr(),
      h4(tags$b("Contacto:")),
      tags$b("Nombre:"), "Alfredo del Río Moldes",
      br(),
      tags$b("e-mail:"), "a.est.gal@usal.es / a.est.gal@gmail.com",
      br(),
      br()
    ),
                 
  tabPanel("Aprende",  
    sidebarLayout(
      sidebarPanel(
        conditionalPanel("input.conditionedpanels == 'Aleatoriedad: Rachas W-W' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable","Variable:", 
                                     c("Lanzar una Moneda" = "M", "Secuencia Numérica" = "S")),
                         sliderInput("cantidad_datos", "Tamaño de la muestra:", value = 15, min = 5, max = 25),
                         p(actionButton("generar_datos", "Nueva Muestra"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("n1","Mostrar \\(n\\), \\(n_{1}\\) y \\(n_{2}\\)", value = T),
                         checkboxInput("rachas","Mostrar \\(R_{obs}\\)", value = T),
                         checkboxInput("e_var1","Mostrar \\(E[R]\\) y \\(Var[R]\\)", value = T),
                         checkboxInput("estadistico_rachas","Mostrar \\(Z_{obs}\\)", value = T),
                         checkboxInput("graf1","Mostrar Gráfico", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Bondad de Ajuste: K-S (F=Fo)' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         sliderInput("cantidad_datos2", "Tamaño de la muestra:", value = 10, min = 6, max = 20),
                         p(actionButton("generar_datos2", "Nueva Muestra"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("ordenados2","Mostrar Datos Ordenados", value = T),
                         checkboxInput("empirica2","Mostrar Distribución Empírica \\(F^{*}\\)", value = T),
                         checkboxInput("conocida2","Mostrar Distribución Conocida \\(F_{0}\\)", value = T),
                         checkboxInput("diferencias2","Mostrar Diferencias en valor absoluto", value = T),
                         checkboxInput("D2","Mostrar \\(D_{obs}\\)", value = T),
                         checkboxInput("qqplot2", "Mostrar Gráfico Q-Q", value = T),
                         checkboxInput("hist2", "Mostrar Histograma y Densidad", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Bondad de Ajuste: K-S (F=G)' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         sliderInput("cantidad_datos3_1", "Tamaño de la muestra X:", value = 10, min = 6, max = 20),
                         sliderInput("cantidad_datos3_2", "Tamaño de la muestra Y:", value = 10, min = 6, max = 20),
                         p(actionButton("generar_datos3", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("ordenados3","Mostrar Datos Ordenados", value = T),
                         checkboxInput("empirica3_1","Mostrar Distribución Empírica de X \\(F^{*}\\)", value = T),
                         checkboxInput("empirica3_2","Mostrar Distribución Empírica de Y \\(G^{*}\\)", value = T),
                         checkboxInput("diferencias3","Mostrar Diferencias en valor absoluto", value = T),
                         checkboxInput("D3","Mostrar \\(D_{obs}\\)", value = T),
                         checkboxInput("qqplot3", "Mostrar Gráfico Q-Q", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Localización de una muestra: W' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         sliderInput("cantidad_datos4", "Tamaño de la muestra:", value = 15, min = 10, max = 20),
                         p(actionButton("generar_datos4", "Nueva Muestra"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("D","Mostrar \\(D\\)", value = T),
                         checkboxInput("D_Abs","Mostrar \\(|D|\\) y Rangos \\(|D|\\)", value = T),
                         checkboxInput("T_obs","Mostrar Estadístico \\(T^{+}_{obs}\\)", value = T),
                         checkboxInput("T_normal","Mostrar \\(E[T^{+}]\\) y \\(Var[T^{+}]\\)", value = T),
                         checkboxInput("Z_obs","Mostrar Estadístico tipificado \\(Z_{obs}\\)", value = T),
                         checkboxInput("graf4","Mostrar Box-Plot", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Dos muestras independientes: U M-W' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable5","Variable:", 
                                     c("Variables Cuantitativas" = "C", "Variables Ordinales" = "O")),
                         sliderInput("cantidad_datos5_1", "Tamaño de la muestra \\(X_{1}\\):", value = 10, min = 6, max = 20),
                         sliderInput("cantidad_datos5_2", "Tamaño de la muestra \\(X_{2}\\):", value = 10, min = 6, max = 20),
                         p(actionButton("generar_datos5", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("n5","Mostrar tamaño de las muestras", value = T),
                         checkboxInput("rXY5","Mostrar rangos de \\(X_{1}\\) y \\(X_{2}\\)", value = T),
                         checkboxInput("R5","Mostrar \\(R_{1}\\) y \\(R_{2}\\)", value = T),
                         checkboxInput("U5","Mostrar \\(U_{1}\\) , \\(U_{2}\\) y \\(U_{obs}\\)", value = T),
                         checkboxInput("tabla_U5","Mostrar \\(E[U]\\) y \\(Var[U]\\)", value = T),
                         checkboxInput("Z5","Mostrar \\(Z_{obs}\\)", value = T),
                         checkboxInput("graf5","Mostrar Box-Plots", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Dos muestras relacionadas: Apareados W' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable6","Variable:", 
                                     c("Variables Cuantitativas" = "C", "Variables Ordinales" = "O")),
                         sliderInput("cantidad_datos6", "Tamaño de la muestra:", value = 10, min = 6, max = 20),
                         p(actionButton("generar_datos6", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("Di6","Mostrar Diferencias \\(Y_{i} - X_{i}\\)", value = T),
                         checkboxInput("absDi6","Mostrar \\(|Y_{i} - X_{i}|\\)", value = T),
                         checkboxInput("rangDi6","Mostrar Rangos", value = T),
                         checkboxInput("estT6","Mostrar \\(T^{+}_{obs}\\)", value = T),
                         checkboxInput("T_normal6","Mostrar \\(E[T^{+}]\\) y \\(Var[T^{+}]\\)", value = T),
                         checkboxInput("Z_obs6","Mostrar Estadístico tipificado \\(Z_{obs}\\)", value = T),
                         checkboxInput("graf6","Mostrar Box-Plots", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'k muestras independientes: K-W' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable9","Variable:", 
                                     c("Variables Cuantitativas" = "C", "Variables Ordinales" = "O")),
                         sliderInput("cantidad_datos9_1", "Tamaño de la muestra \\(X_{1}\\):", value = 10, min = 3, max = 20),
                         sliderInput("cantidad_datos9_2", "Tamaño de la muestra \\(X_{2}\\):", value = 10, min = 3, max = 20),
                         sliderInput("cantidad_datos9_3", "Tamaño de la muestra \\(X_{3}\\):", value = 10, min = 3, max = 20),
                         p(actionButton("generar_datos9", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("n9","Mostrar tamaño de las muestras", value = T),
                         checkboxInput("rXYZ9","Mostrar rangos de \\(X_{1}\\), \\(X_{2}\\), \\(X_{3}\\) (\\(R_{ij}\\))", value = T),
                         checkboxInput("R9","Mostrar \\(R_{1·}\\), \\(R_{2·}\\) y \\(R_{3·}\\)", value = T),
                         checkboxInput("H9", "Mostrar Estadístico \\(H_{obs}\\)", value = T),
                         checkboxInput("graf9","Mostrar Box-Plots", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'k muestras relacionadas: Friedman' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable10","Variable:", 
                                     c("Variables Cuantitativas" = "C", "Variables Ordinales" = "O")),
                         sliderInput("cantidad_datos10", "Tamaño de la muestra:", value = 10, min = 3, max = 20),
                         p(actionButton("generar_datos10", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("n10","Mostrar tamaño muestral", value = T),
                         checkboxInput("r10","Mostrar rangos individuos", value = T),
                         checkboxInput("Ri10","Mostrar \\(R_{1·}\\), \\(R_{2·}\\) y \\(R_{3·}\\)", value = T),
                         checkboxInput("S10","Mostrar Estadístico \\(S_{obs}\\)", value = T),
                         checkboxInput("graf10","Mostrar Box-Plots", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Independencia: Kendall' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable7","Variable:", 
                                     c("Variables Cuantitativas" = "C", "Variable Cuantitativa y Ordinal" = "CO", "Variables Ordinales" = "O")),
                         sliderInput("cantidad_datos7", "Tamaño de la muestra:", value = 10, min = 6, max = 15),
                         p(actionButton("generar_datos7", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("tabla7","Mostrar Tabla de concordancias", value = T),
                         checkboxInput("conc_disc","Mostrar Parejas Concordantes y Discordantes", value = T),
                         checkboxInput("k7","Mostrar \\(k\\)", value = T),
                         checkboxInput("tao7","Mostrar \\(\\tau_{obs}\\)", value = T),
                         checkboxInput("Tao_normal","Mostrar \\(E[\\tau]\\) y \\(Var[\\tau]\\)", value = T),
                         checkboxInput("Z_obs7","Mostrar Estadístico tipificado \\(Z_{obs}\\)", value = T),
                         checkboxInput("graf7","Mostrar Gráfico Dispersión", value = T)
        ),
        conditionalPanel("input.conditionedpanels == 'Independencia: Spearman' ",
                         h4(strong("Generar Datos:")),
                         br(),
                         selectInput("variable8","Variable:", 
                                     c("Variables Cuantitativas" = "C", "Variable Cuantitativa y Ordinal" = "CO", "Variables Ordinales" = "O")),
                         sliderInput("cantidad_datos8", "Tamaño de la muestra:", value = 15, min = 6, max = 25),
                         p(actionButton("generar_datos8", "Nuevas Muestras"), align = "left"),
                         hr(),
                         h4(strong("Mostrar Soluciones:")),
                         checkboxInput("rangos8","Mostrar Rangos (\\(R_{i}\\), \\(S_{i}\\))", value = T),
                         checkboxInput("r_s8","Mostrar Estadístico \\(r_{obs}\\)", value = T),
                         checkboxInput("tobs8","Mostrar \\(t_{obs}\\) y grados libertad \\(t\\)", value = T),
                         checkboxInput("graf8","Mostrar Gráfico Dispersión", value = T)
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Aleatoriedad: Rachas W-W",
                   br(),
                   "Las pruebas de aleatoriedad se suelen utilizar para estudiar la aleatoriedad de una distribución, tomando los datos de la muestra en el orden dado.",
                   br(),
                   "En este apartado se utiliza la prueba de las rachas de Wald-Wolfowitz para estudiar la aleatoriedad de una muestra cualitativa (Moneda) o cuantitativa (Secuencia Numérica).",
                   br(),
                   h3("Hipótesis:"),
                   h2("\\(\\left\\{  
                      H_{0}: \\enspace Muestra \\enspace Aleatoria \\atop
                      H_{1}: \\enspace Muestra \\enspace No \\enspace Aleatoria
                   \\right.\\)"),
                   br(),
                   uiOutput("link1"), #enlace a pagina
                   br(),
                   h3("Funcionamiento:"),
                   "En la sección lateral se puede seleccionar el tipo de variable y el tamaño muestral.",
                   br(),
                   "Al clicar en 'Nueva Muestra' se genera una muestra con los valores elegidos.",
                   br(),
                   "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                   hr(),
                   h2("Datos:"),
                   h3(strong(verbatimTextOutput("datos1"))),
                   br(),
                   "Se contabiliza el número de símbolos del primer tipo \\(n_{1}\\), del segundo tipo \\(n_{2}\\) y el tamaño 
                   total de la muestra \\(n\\) (si la variable es cuantitativa se dicotomiza con la mediana)*:",
                   br(),
                   p("* Los valores que coinciden con la mediana no se tienen en cuenta en el test.", style = "color:red"),
                   tableOutput("med1"),
                   tableOutput("n1"),
                   br(),
                   "Se calcula el número de rachas de la muestra \\(R_{obs}\\) (número de sucesiones de símbolos iguales):",
                   br(),
                   tableOutput("rachas"),
                   br(),
                   "Si \\(n_{1} \\leq 20\\) y \\(n_{2} \\leq 20\\) se busca en la ", 
                   a("Tabla 7 (R)", target="_blank", href="Tabla_7_Rachas.pdf"), 
                   "el intervalo \\([r_{low}, r_{up}]\\) siguiendo la notación de la tabla (\\(n = max(n_{1},n_{2}), m = min(n_{1},n_{2}) \\)) para ", tags$b("\\(\\alpha \\)"), " = 0.05.",
                   br(),
                   br(),
                   "Si \\(R_{obs} \\in [r_{low}, r_{up}] \\Rightarrow \\) se acepta \\(H_{0}\\), es decir, no hay evidencias estadísticas de que la muestra no sea aleatoria.",
                   br(),
                   "Si \\(R_{obs} \\notin [r_{low}, r_{up}] \\Rightarrow \\) se rechaza \\(H_{0}\\) y se concluye que la muestra no presenta aleatoriedad.",
                   br(),
                   br(),
                   "Si \\(n_{1} > 20 \\; \\) o \\( \\; n_{2} > 20\\) se utiliza la distribución asintótica de \\(R\\) (Distribución Normal) obteniendo el valor del estadístico \\(Z_{obs}\\) y comparándolo con el cuantil correspondiente:",
                   h4("\\( E[R] = \\frac{(2n_{1}n_{2})}{n}+1 \\)",
                   "\\( \\qquad \\)",
                   " \\(Var[R] = \\frac{2n_{1}n_{2}·(2n_{1}n_{2}-n)}{n^{2}(n-1)} \\)"),
                   br(),
                   tableOutput("e_var1"),
                   br(),
                   h4("\\( Z_{obs} = \\frac{(R_{obs} - E[R])}{\\sqrt{Var[R]}} \\approx N(0,1) \\)"),
                   br(),
                   tableOutput("estadistico_rachas"),
                   br(),
                   "Se compara \\(|Z_{obs}|\\) con el cuantil \\(z_{ 1-\\frac{\\alpha}{2} }\\):",
                   br(),
                   br(),
                   "Si \\(|Z_{obs}| < z_{ 1-\\frac{\\alpha}{2} } \\Rightarrow \\) Se acepta \\(H_{0}\\)",
                   br(),
                   "Si \\(|Z_{obs}| \\geq z_{ 1-\\frac{\\alpha}{2} } \\Rightarrow \\) Se rechaza \\(H_{0}\\)",
                   br(),
                   br(),
                   "Para \\( \\alpha \\) = 5% :",
                   h5("\\( z_{ 1-\\frac{\\alpha}{2} } = 1,96 \\)" ),
                   br(),
                   "Más cuantiles \\(z_{ 1-\\frac{\\alpha}{2} }\\) : ",
                   a("Tabla 1,2,3 (N)", target="_blank", href="Tabla_1_2_3_Normal.pdf"),
                   br(),
                   br(),
                   "Si queremos visualizar las rachas de la muestra representamos los valores de la muestra en el orden dado (Para las variables cuantitativas se dibuja una línea roja que representa la mediana muestral):",
                   plotOutput("graf1", width = "80%")
          ),
          tabPanel(title = "Bondad de Ajuste: K-S (F=Fo)",
                  br(),
                  "Las pruebas de bondad de ajuste sirven para contrastar si un conjunto de observaciones
                  se ajusta a una distribución de probabilidad. En este apartado se utiliza el test de Kolmogorov-Smirnov
                  para el caso de una distribución teórica conocida \\(F_{0}\\), que se compara con la distribución empírica de la muestra \\(F^{*}\\)",
                  br(),
                  h3("Distribución empírica:"),
                  h3("\\(F_{n}^{*}(x) = \\frac{ \\# \\{ x_{i} \\leq x \\} }{n}\\)"),
                  br(),
                  h3("Hipótesis:"),
                  h2("\\(\\left\\{  
                     H_{0}: \\enspace F \\enspace = \\enspace F_{0} \\atop
                     H_{1}: \\enspace F \\enspace \\neq \\enspace F_{0}
                     \\right.\\)"),
                  br(),
                  uiOutput("link2_1"),
                  uiOutput("link2_2"),
                  br(),
                  h3("Funcionamiento:"),
                  "En la sección lateral se puede seleccionar el tamaño muestral.",
                  br(),
                  "Al clicar en 'Nueva Muestra' se genera una muestra del tamaño elegido y un nuevo enunciado.",
                  br(),
                  "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                  hr(),
                  h2("Datos:"),
                  h3(strong(verbatimTextOutput("datos2"))),
                  br(),
                  uiOutput("enunciado2", container = tags$h3),
                  h3(textOutput("parametros2")),
                  "Primero se ordenan los datos de la muestra:",
                  verbatimTextOutput("ordenados2"),
                  br(),
                  "Se calcula la distribución empírica \\(F^{*}\\) de la variable:",
                  verbatimTextOutput("empirica2"),
                  br(),
                  "Se obtienen las probabilidades de la distribución conocida \\(F_{0}\\) para los valores de la muestra ordenada:",
                  verbatimTextOutput("conocida2"),
                  br(),
                  "Se calculan las diferencias en valor absoluto entre ambas distribuciones:",
                  verbatimTextOutput("diferencias2"),
                  br(),
                  "Se obtiene el estadístico \\(D_{obs}\\) como el valor máximo de estas diferencias:",
                  tableOutput("D2"),
                  br(),
                  "Teniendo en cuenta \\(n\\) y \\(\\alpha\\) se busca en la ",
                  a("Tabla 6 (KS)", target="_blank", href="Tabla_6_D.pdf"),
                  "el valor crítico \\(D_{crit}\\) y se compara con \\(D_{obs}\\):",
                  br(),
                  br(),
                  "Si \\(D_{obs} < D_{crit}\\) se acepta \\(H_{0}\\). No hay evidencias estadísticas de que la muestra no provenga de \\(F_{0}\\)",
                  br(),
                  "Si \\(D_{obs} \\geq D_{crit}\\) se rechaza \\(H_{0}\\). La distribución de la que proviene la muestra no es \\(F_{0}\\)",
                  br(),
                  br(),
                  "Es habitual observar el gráfico Q-Q para comprobar si los cuantiles de la distribución empírica \\(F^{*}\\) y los cuantiles teóricos conocidos de \\(F_{0}\\) son similares:",
                  splitLayout(
                    plotOutput("qqplot2", width = "90%"),
                    plotOutput("hist2", width = "90%")
                  )
          ),
          tabPanel(title = "Bondad de Ajuste: K-S (F=G)",
                   br(),
                   "Las pruebas de bondad de ajuste también sirven para contrastar si dos conjuntos de observaciones
                   provienen de la misma distribución de probabilidad. En este apartado se utiliza el test de Kolmogorov-Smirnov
                   para el caso de dos muestras. ",
                   br(),
                   h3("Distribución empírica:"),
                   h3("\\(F_{n}^{*}(x) = \\frac{ \\# \\{ x_{i} \\leq x \\} }{n}\\)"),
                   br(),
                   h3("Hipótesis:"),
                   h2("\\(\\left\\{  
                      H_{0}: \\enspace F \\enspace = \\enspace G \\atop
                      H_{1}: \\enspace F \\enspace \\neq \\enspace G
                      \\right.\\)"),
                   br(),
                   uiOutput("link3_1"),
                   uiOutput("link3_2"),
                   br(),
                   h3("Funcionamiento:"),
                   "En la sección lateral se puede seleccionar el tamaño de ambas muestras.",
                   br(),
                   "Al clicar en 'Nuevas Muestras' se generan dos muestras del tamaño elegido.",
                   br(),
                   "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                   hr(),
                   h2("Datos:"),
                   h3(strong(verbatimTextOutput("datos3_1"))),
                   h3(strong(verbatimTextOutput("datos3_2"))),
                   br(),
                   h3(textOutput("enunciado3")),
                   "Primero se ordenan de menor a mayor todas las observaciones unidas en una sola muestra:",
                   verbatimTextOutput("ordenados3"),
                   br(),
                   "Se calcula la distribución empírica de ambas variables \\(F^{*}\\) y \\(G^{*}\\) teniendo en cuenta la muestra de la que provenía cada dato:",
                   br(),
                   br(),
                   "Distribución Empírica \\(F^{*}\\):",
                   verbatimTextOutput("empirica3_1"),
                   "Distribución Empírica \\(G^{*}\\):",
                   verbatimTextOutput("empirica3_2"),
                   br(),
                   "Se calculan las diferencias en valor absoluto entre \\(F^{*}\\) y \\(G^{*}\\):",
                   verbatimTextOutput("diferencias3"),
                   br(),
                   "Se obtiene el estadístico \\(D_{obs}\\) como el valor máximo de estas diferencias:",
                   tableOutput("D3"),
                   br(),
                   "Teniendo en cuenta \\(n = n_{1}+n_{2}\\) y \\(\\alpha\\) se busca en la ",
                   a("Tabla 6 (KS)", target="_blank", href="Tabla_6_D.pdf"),
                   "el valor crítico \\(D_{crit}\\) y se compara con \\(D_{obs}\\):",
                   br(),
                   br(),
                   "Si \\(D_{obs} < D_{crit}\\) se acepta \\(H_{0}\\). No hay evidencias estadísticas de que las muestras no provengan de la misma distribución",
                   br(),
                   "Si \\(D_{obs} \\geq D_{crit}\\) se rechaza \\(H_{0}\\). La distribución de la que provienen las muestras no es la misma",
                   br(),
                   br(),
                   "Es habitual observar el gráfico Q-Q para comprobar si los cuantiles de las distribuciones empíricas \\(F^{*}\\) y \\(G^{*}\\) son similares:",
                   plotOutput("qqplot3", width = "70%")
          ),
          tabPanel(title = "Localización de una muestra: W",
                  br(),
                  "Las pruebas no paramétricas de localización de una muestra consisten en estudiar 
                  el valor de la mediana de la población de la que proviene la muestra. En este apartado
                  se aplica la prueba de los rangos con signo de Wilcoxon a muestras cuantitativas.",
                  br(),
                  h3("Hipótesis:"),
                  h2("\\(\\left\\{  
                     H_{0}: \\enspace \\theta \\enspace = \\enspace \\theta_{0} \\atop
                     H_{1}: \\enspace \\theta \\enspace \\neq \\enspace \\theta_{0}
                     \\right.\\)",
                     "\\(\\qquad\\)",      # ESPACIO
                     "\\(\\left\\{  
                     H_{0}: \\enspace \\theta \\enspace \\leq \\enspace \\theta_{0} \\atop
                     H_{1}: \\enspace \\theta \\enspace > \\enspace \\theta_{0}
                     \\right.\\)",
                     "\\(\\qquad\\)",      # ESPACIO
                     "\\(\\left\\{  
                     H_{0}: \\enspace \\theta \\enspace \\geq \\enspace \\theta_{0} \\atop
                     H_{1}: \\enspace \\theta \\enspace < \\enspace \\theta_{0}
                     \\right.\\)"),
                  br(),
                  uiOutput("link4"),
                  br(),
                  h3("Funcionamiento:"),
                  "En la sección lateral se puede elegir el tamaño muestral.",
                  br(),
                  "Al clicar en 'Nueva Muestra' se genera una muestra del tamaño elegido y un nuevo enunciado.",
                  br(),
                  "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                  hr(),
                  h2("Datos:"),
                  "\\(X\\):",
                  h3(strong(verbatimTextOutput("datos4"))),
                  br(),
                  h3(textOutput("enunciado4")),
                  "Primero se calcula una nueva variable \\(D = X - \\theta_{0}\\) :",
                  br(),
                  verbatimTextOutput("D"),
                  br(),
                  "Se aplica el valor absoluto a los valores de la nueva variable \\(D\\) y se calculan sus rangos:",
                  br(),
                  verbatimTextOutput("D_Abs"),
                  verbatimTextOutput("D_Ranks"),
                  br(),
                  "Se obtiene el estadístico de los rangos con signo de Wilcoxon \\(T^{+}_{obs}\\) sumando 
                  los rangos de los valores de \\(D\\) positivos (Los rangos asociados con valores de X menores que la mediana no se suman):",
                  tableOutput("T_obs"),
                  br(),
                  "Si \\(n \\leq 12\\) se busca en la ",
                  a("Tabla 8 (T+)", target="_blank", href="Tabla_8_T.pdf"),
                  "la significación muestral correspondiente a cada hipótesis (Fondo Gris: \\(P_{I}\\), Fondo Blanco: \\(P_{S}\\)):",
                  br(),
                  br(),
                  "\\(H_{1}: \\theta < \\theta_{0} \\Rightarrow P_{I} = P(T^{+} \\leq T^{+}_{obs}) \\Rightarrow\\) Si \\(P_{I} \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                  br(),
                  "\\(H_{1}: \\theta > \\theta_{0} \\Rightarrow P_{S} = P(T^{+} \\geq T^{+}_{obs}) \\Rightarrow\\) Si \\(P_{S} \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                  br(),
                  "\\(H_{1}: \\theta \\neq \\theta_{0} \\Rightarrow P = 2·min\\{P_{I},P_{S}\\} \\Rightarrow\\) Si \\(P \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                  br(),
                  br(),
                  "Si el valor del estadístico no se encuentra en las tablas utilizar una de las siguientes propiedades (\\(t \\in \\mathbb{N}\\)):",
                  br(),
                  "\\(P(T^{+} \\leq t) = 1 - P(T^{+} > t) = 1 - P(T^{+} \\geq t+1)\\)",
                  br(),
                  "\\(P(T^{+} \\geq t) = 1 - P(T^{+} < t) = 1 - P(T^{+} \\leq t-1)\\)",
                  br(),
                  br(),
                  "Si el valor de \\(T^{+}_{obs}\\) tiene decimales se procede de la siguiente manera:",
                  br(),
                  "Para los valores de \\(P_{I}\\) se redondea al anterior natural. Ejemplo: \\(T^{+}_{obs} = 3.5 \\rightarrow P_{I} = P(T^{+} \\leq 3.5) = P(T^{+} \\leq 3)\\)",
                  br(),
                  "Para los valores de \\(P_{S}\\) se redondea al posterior natural. Ejemplo: \\(T^{+}_{obs} = 3.5 \\rightarrow P_{S} = P(T^{+} \\geq 3.5) = P(T^{+} \\geq 4)\\)",
                  br(),
                  br(),
                  "Si \\(n > 12\\) se utiliza la distribución asintótica de \\(T^{+}\\) (Distribución Normal) y se compara el estadístico \\(Z_{obs}\\) 
                  con el cuantil que corresponda segun el tipo de contraste.",
                  br(),
                  br(),
                  "Se calcula \\(E[T^{+}]\\) y \\(Var[T^{+}]\\) siendo \\(n\\) el tamaño muestral:",
                  h4("\\( E[T^{+}] = \\frac{n(n+1)}{4} \\)",
                     "\\( \\qquad \\)",
                     " \\(Var[T^{+}] = \\frac{n(n+1)(2n+1)}{24} \\)"),
                  br(),
                  tableOutput("T_normal"), #media y varianza
                  br(),
                  "Se obtiene el valor de \\(Z_{obs}\\):",
                  h4("\\( Z_{obs} = \\frac{(T^{+}_{obs} - E[T^{+}])}{\\sqrt{Var[T^{+}]}} \\approx N(0,1) \\)"),
                  tableOutput("Z_obs"),
                  br(),
                  "Se compara el valor de \\(Z_{obs}\\) con el cuantil de \\(Z\\) correspondiente.",
                  br(),
                  br(),
                  "Para \\( \\alpha \\) = 5% :",
                  h5("\\( z_{\\alpha} = -1.6449 \\)  \\( \\qquad z_{1-\\alpha} = 1.6449 \\enspace \\)  \\( \\qquad z_{ 1-{\\alpha}/{2} } = 1,96 \\)" ),
                  br(),
                  "Más cuantiles \\(z\\) : ",
                  a("Tabla 1,2,3 (N)", target="_blank", href="Tabla_1_2_3_Normal.pdf"),
                  br(),
                  br(),
                  "Es habitual realizar un box-plot de los datos para localizar la mediana muestral visualmente (La línea roja representa el valor de \\(\\theta_{0}\\)): ",
                  plotOutput("graf4", width = "80%")
          ),
          tabPanel(title = "Dos muestras independientes: U M-W",
                   br(),
                   "Si se quiere comparar la distribución de una variable \\(X\\) entre dos muestras independientes \\(X_{1},X_{2}\\), se puede utilizar la prueba para muestras independientes de Mann Whitney",
                   br(),
                   h3("Hipótesis:"),
                   h2("\\(\\left\\{  
                      H_{0}: \\enspace X_{1} \\enspace = \\enspace X_{2} \\atop
                      H_{1}: \\enspace X_{1} \\enspace \\neq \\enspace X_{2}
                      \\right.\\)",
                      "\\(\\qquad\\)",      # ESPACIO
                      "\\(\\left\\{  
                      H_{0}: \\enspace X_{1} \\enspace \\leq \\enspace X_{2} \\atop
                      H_{1}: \\enspace X_{1} \\enspace > \\enspace X_{2}
                      \\right.\\)",
                      "\\(\\qquad\\)",      # ESPACIO
                      "\\(\\left\\{  
                      H_{0}: \\enspace X_{1} \\enspace \\geq \\enspace X_{2} \\atop
                      H_{1}: \\enspace X_{1} \\enspace < \\enspace X_{2}
                      \\right.\\)"),
                   br(),
                   uiOutput("link5"),
                   br(),
                   h3("Funcionamiento:"),
                   "En la sección lateral se puede seleccionar el tipo de variable y el tamaño muestral de ambas muestras.",
                   br(),
                   "Al clicar en 'Nuevas Muestras' se generan dos muestras con los valores elegidos.",
                   br(),
                   "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                   hr(),
                   h2("Datos:"),
                   p("*Las categorías de las variables ordinales están expresadas como números enteros
                     (Ej: 'Mal'= 1, 'Regular'= 2, 'Bien'= 3)", style = "color:red"),
                   br(),
                   "\\(X_{1}\\):",
                   h3(strong(verbatimTextOutput("datos5_1"))),
                   "\\(X_{2}\\):",
                   h3(strong(verbatimTextOutput("datos5_2"))),
                   br(),
                   "Se anota el tamaño de las muestras \\(n_{1}, n_{2}\\) y el tamaño muestral total \\(n\\), que serán útiles en los cálculos:",
                   tableOutput("n5"),
                   br(),
                   "El primer paso es calcular los rangos de ambas muestras de forma conjunta:",
                   verbatimTextOutput("rX5"),
                   verbatimTextOutput("rY5"),
                   br(),
                   "Después se calcula el valor de \\(R_{1}\\) y \\(R_{2}\\) sumando los rangos de cada variable:",
                   tableOutput("R5"),
                   br(),
                   "Se calculan los valores de \\(U_{1}\\) y \\(U_{2}\\):",
                   h4("\\( U_{1} = R_{1} - \\frac{n_{1}(n_{1}+1)}{2} \\)",
                      "\\( \\qquad \\)",
                      " \\(U_{2} = R_{2} - \\frac{n_{2}(n_{2}+1)}{2} \\)"),
                   br(),
                   tableOutput("U5"),
                   br(),
                   "Se calcula el estadístico de Mann-Whitney \\(U_{obs}\\) como el valor mínimo entre \\(U_{1}\\) y \\(U_{2}\\):",
                   tableOutput("Uobs5"),
                   br(),
                   "Si \\(n_{1} \\leq 8\\) y \\(n_{2} \\leq 8\\), se busca en la ",
                   a("Tabla 9 (U)", target="_blank", href="Tabla_9_U.pdf"),
                   "el valor de \\(P(U \\leq u)\\) teniendo en cuenta \\(m = max(n_{1},n_{2})\\), \\(n = min(n_{1},n_{2})\\), \\(u = U_{obs}\\).",
                   br(),
                   br(),
                   "Se procede según el tipo de contraste:",
                   br(),
                   "\\(H_{1}: X_{1} < X_{2} \\Rightarrow P_{I} = P(U \\leq U_{obs}) \\Rightarrow\\) Si \\(P_{I} \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                   br(),
                   "\\(H_{1}: X_{1} > X_{2} \\Rightarrow P_{S} = P(U \\geq U_{obs}) \\Rightarrow\\) Si \\(P_{S} \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                   br(),
                   "\\(H_{1}: X_{1} \\neq X_{2} \\Rightarrow P = 2·min\\{P_{I},P_{S}\\} \\Rightarrow\\) Si \\(P \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                   br(),
                   br(),
                   "Para los valores de \\(P(U \\geq u)\\) utilizar la siguiente propiedad (\\(u \\in \\mathbb{N} \\)):",
                   br(),
                   "* \\(P(U \\geq u) = 1 - P(U < u) = 1 - P(U \\leq u-1)\\)",
                   br(),
                   br(),
                   "Si \\(U_{obs}\\) tiene decimales, se procede de la siguiente manera:",
                   br(),
                   "Para los valores de \\(P_{I}\\) se redondea al anterior natural. Ejemplo: \\(U_{obs} = 3.5 \\rightarrow P_{I} = P(U \\leq 3.5) = P(U \\leq 3)\\)",
                   br(),
                   "Para los valores de \\(P_{S}\\) se redondea al posterior natural. Ejemplo: \\(U_{obs} = 3.5 \\rightarrow P_{S} = P(U \\geq 3.5) = P(U \\geq 4)\\)",
                   br(),
                   br(),
                   "Por el contrario, si \\(n_{1} > 8\\) o \\(n_{2} > 8\\) se utiliza la distribución asintótica de \\(U\\) (Distribución Normal) y se compara el estadístico \\(Z_{obs}\\) 
                   con el cuantil que corresponda segun el tipo de contraste.",
                   br(),
                   br(),
                   "Se calcula \\(E[U]\\) y \\(Var[U]\\):",
                   h4("\\( E[U] = \\frac{n_{1}n_{2}}{2} \\)",
                      "\\( \\qquad \\)",
                      " \\(Var[U] = \\frac{n_{1}n_{2}(n_{1}+n_{2}+1)}{12} \\)"),
                   br(),
                   tableOutput("tabla_U5"),
                   br(),
                   "Se obtiene el valor de \\(Z_{obs}\\):",
                   h4("\\( Z_{obs} = \\frac{(U_{obs} - E[U])}{\\sqrt{Var[U]}} \\approx N(0,1) \\)"),
                   tableOutput("Z5"),
                   br(),
                   "Se compara el valor de \\(Z_{obs}\\) con el cuantil de \\(Z\\) correspondiente.",
                   br(),
                   br(),
                   "Para \\( \\alpha \\) = 5% :",
                   h5("\\( z_{\\alpha} = -1.6449 \\)  \\( \\qquad z_{1-\\alpha} = 1.6449 \\enspace \\)  \\( \\qquad z_{ 1-{\\alpha}/{2} } = 1,96 \\)" ),
                   br(),
                   "Más cuantiles \\(z\\) : ",
                   a("Tabla 1,2,3 (N)", target="_blank", href="Tabla_1_2_3_Normal.pdf"),
                   br(),
                   br(),
                   "Es habitual realizar los gráficos Box-Plot para comparar visualmente las muestras: ",
                   plotOutput("graf5", width = "80%")
          ),
          tabPanel(title = "Dos muestras relacionadas: Apareados W",
                   br(),
                   "Si se necesita comparar dos variables que están relacionadas se utiliza la prueba para datos apareados de Wilcoxon.
                    Esta prueba es una alternativa al test t-student para datos relacionados. Es necesario que las variables estén medidas en los mismos individuos y tengan las mismas unidades.",
                   br(),
                   "Este tipo de contraste es muy utilizado cuando se compara una variable antes y después de un tratamiento, evento o experimento.",
                   br(),
                   h3("Hipótesis:"),
                   h2("\\(\\left\\{  
                      H_{0}: \\enspace X \\enspace = \\enspace Y \\atop
                      H_{1}: \\enspace X \\enspace \\neq \\enspace Y
                      \\right.\\)",
                      "\\(\\qquad\\)",      # ESPACIO
                      "\\(\\left\\{  
                      H_{0}: \\enspace X \\enspace \\leq \\enspace Y \\atop
                      H_{1}: \\enspace X \\enspace > \\enspace Y
                      \\right.\\)",
                      "\\(\\qquad\\)",      # ESPACIO
                      "\\(\\left\\{  
                      H_{0}: \\enspace X \\enspace \\geq \\enspace Y \\atop
                      H_{1}: \\enspace X \\enspace < \\enspace Y
                      \\right.\\)"),
                   br(),
                   uiOutput("link6"),
                   br(),
                   h3("Funcionamiento:"),
                   "En la sección lateral se puede seleccionar el tipo de variables y el tamaño de las muestras.",
                   br(),
                   "Al clicar en 'Nuevas Muestras' se generan dos muestras con los valores elegidos.",
                   br(),
                   "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                   hr(),
                   h2("Datos:"),
                   p("*Las categorías de las variables ordinales están expresadas como números enteros
                    (Ej: 'Mal'= 1, 'Regular'= 2, 'Bien'= 3)", style = "color:red"),
                   br(),
                   "\\(X\\):",
                   h3(strong(verbatimTextOutput("datos6_1"))),
                   "\\(Y\\):",
                   h3(strong(verbatimTextOutput("datos6_2"))),
                   br(),
                   "El primer paso es obtener las diferencias entre los pares de valores relacionados \\(Y_{i}\\) - \\(X_{i}\\) :",
                   verbatimTextOutput("Di6"),
                   br(),
                   "Posteriormente se calcula el valor absoluto de estas diferencias \\(|Y_{i} - X_{i}|\\) y se obtienen los rangos de estos valores: ",
                   verbatimTextOutput("absDi6"),
                   verbatimTextOutput("rangDi6"),
                   br(),
                   "El estadístico \\(T^{+}_{obs}\\) se calcula sumando los rangos asociados a valores de diferencias positivas \\(Y_{i}\\) - \\(X_{i} > 0\\) :",
                   tableOutput("estT6"),
                   br(),
                   "Si \\(n \\leq 12\\) se busca en la ",
                   a("Tabla 8 (T+)", target="_blank", href="Tabla_8_T.pdf"),
                   "la significación muestral correspondiente a cada hipótesis (Fondo Gris: \\(P_{I}\\), Fondo Blanco: \\(P_{S}\\)):",
                   br(),
                   br(),
                   "\\(H_{1}: \\theta < \\theta_{0} \\Rightarrow P_{I} = P(T^{+} \\leq T^{+}_{obs}) \\Rightarrow\\) Si \\(P_{I} \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                   br(),
                   "\\(H_{1}: \\theta > \\theta_{0} \\Rightarrow P_{S} = P(T^{+} \\geq T^{+}_{obs}) \\Rightarrow\\) Si \\(P_{S} \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                   br(),
                   "\\(H_{1}: \\theta \\neq \\theta_{0} \\Rightarrow P = 2·min\\{P_{I},P_{S}\\} \\Rightarrow\\) Si \\(P \\leq \\alpha \\Rightarrow\\) Se Rechaza \\(H_{0}\\) ",
                   br(),
                   br(),
                   "Si el valor del estadístico no se encuentra en las tablas utilizar una de las siguientes propiedades (\\(t \\in \\mathbb{N})\\):",
                   br(),
                   "\\(P(T^{+} \\leq t) = 1 - P(T^{+} > t) = 1 - P(T^{+} \\geq t+1)\\)",
                   br(),
                   "\\(P(T^{+} \\geq t) = 1 - P(T^{+} < t) = 1 - P(T^{+} \\leq t-1)\\)",
                   br(),
                   br(),
                   "Si el valor de \\(T^{+}_{obs}\\) tiene decimales se procede de la siguiente manera:",
                   br(),
                   "Para los valores de \\(P_{I}\\) se redondea al anterior natural. Ejemplo: \\(T^{+}_{obs} = 3.5 \\rightarrow P_{I} = P(T^{+} \\leq 3.5) = P(T^{+} \\leq 3)\\)",
                   br(),
                   "Para los valores de \\(P_{S}\\) se redondea al posterior natural. Ejemplo: \\(T^{+}_{obs} = 3.5 \\rightarrow P_{S} = P(T^{+} \\geq 3.5) = P(T^{+} \\geq 4)\\)",
                   br(),
                   br(),
                   "Si \\(n > 12\\) se utiliza la distribución asintótica de \\(T^{+}\\) (Distribución Normal) y se compara el estadístico \\(Z_{obs}\\) 
                   con el cuantil que corresponda segun el tipo de contraste.",
                   br(),
                   br(),
                   "Se calcula \\(E[T^{+}]\\) y \\(Var[T^{+}]\\) siendo \\(n\\) el tamaño muestral:",
                   h4("\\( E[T^{+}] = \\frac{n(n+1)}{4} \\)",
                      "\\( \\qquad \\)",
                      " \\(Var[T^{+}] = \\frac{n(n+1)(2n+1)}{24} \\)"),
                   br(),
                   tableOutput("T_normal6"),
                   br(),
                   "Se tipifica el estadístico \\(T^{+}_{obs}\\) obteniendo \\(Z_{obs}\\) :",
                   h4("\\( Z_{obs} = \\frac{(T^{+}_{obs} - E[T^{+}])}{\\sqrt{Var[T^{+}]}} \\approx N(0,1) \\)"),
                   tableOutput("Z_obs6"),
                   br(),
                   "Se compara \\(Z_{obs}\\) con el cuantil de \\(Z\\) correspondiente.",
                   br(),
                   br(),
                   "Para \\( \\alpha \\) = 5% :",
                   h5("\\( z_{\\alpha} = -1.6449 \\)  \\( \\qquad z_{1-\\alpha} = 1.6449 \\enspace \\)  \\( \\qquad z_{ 1-\\frac{\\alpha}{2} } = 1,96 \\)" ),
                   br(),
                   "Más cuantiles \\(z\\) : ",
                   a("Tabla 1,2,3 (N)", target="_blank", href="Tabla_1_2_3_Normal.pdf"),
                   br(),
                   br(),
                   "Es habitual realizar los gráficos Box-Plot para comparar visualmente las muestras: ",
                   plotOutput("graf6", width = "80%")
          ),
          tabPanel(title = "k muestras independientes: K-W",
                   br(),
                   "Si se quieren comparar \\(k>2\\) muestras independientes del mismo o distinto tamaño de una variable se utiliza el test de Kruskal-Wallis.
                   Esta prueba es equivalente al ANOVA paramétrico de 1 factor.",
                   br(),
                   h3("Hipótesis:"),
                   h2("\\(\\left\\{  
                           H_{0}: \\enspace \\tau_{1} \\enspace = \\enspace \\tau_{2} \\enspace = \\enspace \\cdots \\enspace = \\enspace \\tau_{k} \\atop
                           H_{1}: \\enspace \\tau_{i} \\enspace \\neq \\enspace \\tau_{j} \\enspace para \\enspace algun \\enspace i \\neq j 
                           \\right.\\)"),
                   h2("\\(\\left\\{  
                           H_{0}:  \\enspace No \\: hay \\: diferencias \\: entre \\: las \\: k \\: muestras \\atop
                           H_{1}:  \\enspace Al \\: menos \\: dos \\: muestras \\: son \\: distintas
                           \\right.\\)"),
                   br(),
                   uiOutput("link9"),
                   br(),
                   h3("Funcionamiento:"),
                   "En la sección lateral se puede seleccionar el tipo de variables y el tamaño de las muestras.",
                   br(),
                   "Al clicar en 'Nuevas Muestras' se generan tres muestras con los valores elegidos.",
                   br(),
                   "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                   hr(),
                   h2("Datos:"),
                   p("*Las categorías de las variables ordinales están expresadas como números enteros
                    (Ej: 'Mal'= 1, 'Regular'= 2, 'Bien'= 3)", style = "color:red"),
                   br(),
                   "\\(X_{1}\\):",
                   h3(strong(verbatimTextOutput("datos9_1"))),
                   "\\(X_{2}\\):",
                   h3(strong(verbatimTextOutput("datos9_2"))),
                   "\\(X_{3}\\):",
                   h3(strong(verbatimTextOutput("datos9_3"))),
                   br(),
                   "Se anota el tamaño de las muestras \\(n_{1}, n_{2}, n_{3}\\) y el tamaño muestral total \\(N\\), que serán útiles en los cálculos:",
                   tableOutput("n9"),
                   br(),
                   "El primer paso es calcular los rangos de las muestras de forma conjunta \\(R_{ij}\\):",
                   verbatimTextOutput("rX9"),
                   verbatimTextOutput("rY9"),
                   verbatimTextOutput("rZ9"),
                   br(),
                   "Después se calcula el valor de \\(R_{1·}\\) \\(R_{2·}\\) y \\(R_{3·}\\) sumando los rangos de cada muestra:",
                   tableOutput("R9"),
                   br(),
                   "Se obtiene el estadístico \\(H_{obs}\\) siendo \\(k\\) el número de muestras:",
                   h4("\\( H_{obs} = \\left( \\frac{12}{N(N+1)}·\\sum_{i=1}^{k} \\frac{(R_{i·})^{2}}{n_{i}} \\right) - 3(N+1) \\)"),
                   tableOutput("H9"),
                   br(),
                   "Si \\(n_{1} \\leq 8\\), \\(n_{2} \\leq 8\\) y \\(n_{3} \\leq 8\\), se busca en la ",
                   a("Tabla 10 (H)", target="_blank", href="Tabla_10_H.pdf"),
                   "el valor de \\(H_{crit}\\) teniendo en cuenta \\(n_{1},n_{2},n_{3}\\) y \\(\\alpha\\).",
                   br(),
                   br(),
                   "Se compara \\(H_{obs}\\) con \\(H_{crit}\\):",
                   br(),
                   br(),
                   "Si \\(H_{obs} < H_{crit} \\Rightarrow\\) Se Acepta \\(H_{0}\\)",
                   br(),
                   "Si \\(H_{obs} \\geq H_{crit} \\Rightarrow\\) Se Rechaza \\(H_{0}\\)",
                   br(),
                   br(),
                   "Por el contrario si \\(n_{1} > 8\\) o \\(n_{2} > 8\\) o \\(n_{3} > 8\\) se utiliza la distribución asintótica de \\(H\\):",
                   br(),
                   h4("\\(H \\approx \\chi^{2}_{k-1} \\)"),
                   br(),
                   "Se compara el estadístico \\(H_{obs}\\) con el cuantil de \\(\\chi^{2}_{k-1}\\):",
                   br(),
                   br(),
                   "Si \\(H_{obs} < \\chi^{2}_{k-1;1-\\alpha} \\Rightarrow\\) Se Acepta \\(H_{0}\\)",
                   br(),
                   "Si \\(H_{obs} \\geq \\chi^{2}_{k-1;1-\\alpha} \\Rightarrow\\) Se Rechaza \\(H_{0}\\)",
                   br(),
                   br(),
                   "Para \\(k=3\\) y \\(\\alpha = 0.05\\):",
                   br(),
                   br(),
                   "\\(\\chi^{2}_{k-1;1-\\alpha} = 5.991\\)",
                   br(),
                   br(),
                   "Más cuantiles \\(\\chi^{2}_{k-1;1-\\alpha}\\) : ",
                   a("Tabla 4 (Chi2)", target="_blank", href="Tabla_4_Chi.pdf"),
                   br(),
                   br(),
                   "Si se rechaza \\(H_{0}\\) se realiza un análisis post-hoc comparando las muestras dos a dos con la prueba para dos muestras independientes \\(U\\) de Mann-Whitney.",
                   br(),
                   br(),
                   "Es habitual realizar los gráficos Box-Plot para comparar visualmente las muestras:",
                   plotOutput("graf9", width = "80%")
          ),
          tabPanel(title = "k muestras relacionadas: Friedman",
                   br(),
                   "Si se quieren comparar \\(k>2\\) muestras relacionadas se utiliza la prueba de Friedman.",
                   br(),
                   h3("Hipótesis:"),
                   h2("\\(\\left\\{  
                           H_{0}: \\enspace \\tau_{1} \\enspace = \\enspace \\tau_{2} \\enspace = \\enspace \\cdots \\enspace = \\enspace \\tau_{k} \\atop
                           H_{1}: \\enspace \\tau_{i} \\enspace \\neq \\enspace \\tau_{j} \\enspace para \\enspace algun \\enspace i \\neq j 
                           \\right.\\)"),
                   h2("\\(\\left\\{  
                           H_{0}:  \\enspace No \\: hay \\: diferencias \\: entre \\: las \\: k \\: muestras \\atop
                           H_{1}:  \\enspace Al \\: menos \\: dos \\: muestras \\: son \\: distintas
                           \\right.\\)"),
                   br(),
                   uiOutput("link10"),
                   br(),
                   h3("Funcionamiento:"),
                   "En la sección lateral se puede seleccionar el tipo de variables y el tamaño de las muestras.",
                   br(),
                   "Al clicar en 'Nuevas Muestras' se generan tres muestras con los valores elegidos.",
                   br(),
                   "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                   hr(),
                   h2("Datos:"),
                   p("*Las categorías de las variables ordinales están expresadas como números enteros
                    (Ej: 'Mal'= 1, 'Regular'= 2, 'Bien'= 3)", style = "color:red"),
                   br(),
                   tableOutput("datos10"),
                   br(),
                   "Se anota el tamaño de las muestras \\(n\\) y el total \\(N\\):",
                   tableOutput("n10"),
                   br(),
                   "El primer paso es calcular los rangos de las variables en cada individuo \\(R_{ij}\\):",
                   tableOutput("r10"),
                   br(),
                   "Se calcula \\(R_{i·}\\) sumando los rangos de cada variable en todos los individuos:",
                   tableOutput("Ri10"),
                   br(),
                   "Se obtiene el estadistico \\(S_{obs}\\) siendo \\(k\\) el número de muestras:",
                   h4("\\( S_{obs} = \\left( \\frac{12}{n·k(k+1)}·\\sum_{i=1}^{k} R_{i·}^{2} \\right) - 3n(k+1) \\)"),
                   tableOutput("S10"),
                   br(),
                   "Si \\(n \\leq 15\\) y \\(k \\leq 5\\) se busca en la ",
                   a("Tabla 11 (S)", target="_blank", href="Tabla_11_S.pdf"),
                   "el valor de \\(S_{crit}\\) teniendo en cuenta el valor de \\(k\\), \\(n\\) y \\(\\alpha\\).",
                   br(),
                   br(),
                   "Se compara \\(S_{obs}\\) con \\(S_{crit}\\):",
                   br(),
                   br(),
                   "Si \\(S_{obs} < S_{crit} \\Rightarrow\\) Se Acepta \\(H_{0}\\)",
                   br(),
                   "Si \\(S_{obs} \\geq S_{crit} \\Rightarrow\\) Se Rechaza \\(H_{0}\\)",
                   br(),
                   br(),
                   "Por el contrario si \\(n > 15\\) o \\(k > 5\\) se utiliza la distribución asintótica de \\(S\\):",
                   h4("\\(S \\approx \\chi^{2}_{k-1} \\)"),
                   br(),
                   "Se compara el estadístico \\(S_{obs}\\) con el cuantil de \\(\\chi^{2}_{k-1}\\):",
                   br(),
                   br(),
                   "Si \\(S_{obs} < \\chi^{2}_{k-1;1-\\alpha} \\Rightarrow\\) Se Acepta \\(H_{0}\\)",
                   br(),
                   "Si \\(S_{obs} \\geq \\chi^{2}_{k-1;1-\\alpha} \\Rightarrow\\) Se Rechaza \\(H_{0}\\)",
                   br(),
                   br(),
                   "Para \\(k=3\\) y \\(\\alpha = 0.05\\):",
                   br(),
                   br(),
                   "\\(\\chi^{2}_{k-1;1-\\alpha} = 5.991\\)",
                   br(),
                   br(),
                   "Más cuantiles \\(\\chi^{2}_{k-1;1-\\alpha}\\) : ",
                   a("Tabla 4 (Chi2)", target="_blank", href="Tabla_4_Chi.pdf"),
                   br(),
                   br(),
                   "Si se rechaza \\(H_{0}\\) se realiza un análisis post-hoc comparando las muestras dos a dos con la prueba para dos muestras relacionadas: Rangos con signo de Wilcoxon.",
                   br(),
                   br(),
                   "Es habitual realizar los gráficos Box-Plot para comparar visualmente las muestras:",
                   plotOutput("graf10", width = "80%")
          ),
          tabPanel(title = "Independencia: Kendall",
                  br(),
                  "Si se quiere estudiar la relación entre dos variables pareadas con unidades distintas sólo podemos compararlas dos a dos.
                   En este apartado se utiliza la prueba de Kendall para contrastar la concordancia de dos muestras.",
                  br(),
                  h3("Hipótesis:"),
                  h2("\\(\\left\\{  
                     H_{0}: \\enspace Variables \\enspace Independientes \\atop
                     H_{1}: \\enspace Variables \\enspace Dependientes
                     \\right.\\)"),
                  br(),
                  uiOutput("link7"),
                  br(),
                  h3("Funcionamiento:"),
                  "En la sección lateral se puede seleccionar el tipo de variables y el tamaño de las muestras.",
                  br(),
                  "Al clicar en 'Nuevas Muestras' se generan dos muestras con los valores elegidos.",
                  br(),
                  "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                  hr(),
                  h2("Datos:"),
                  p("*Las categorías de las variables ordinales están expresadas como números enteros
                  (Ej: 'Mal'= 1, 'Regular'= 2, 'Bien'= 3)", style = "color:red"),
                  br(),
                  "\\(X\\):",
                  h3(strong(verbatimTextOutput("datos7_1"))),
                  "\\(Y\\):",
                  h3(strong(verbatimTextOutput("datos7_2"))),
                  br(),
                  "Primero se elabora la tabla de concordancias: (1 = Pareja Concordante, -1 = Pareja Discordante)",
                  tableOutput("tabla7"),
                  br(),
                  "Se calcula el número de parejas concordantes y discordantes:",
                  tableOutput("conc_disc"),
                  br(),
                  "El valor del estadístico de Kendall \\(k\\) se obtiene como la diferencia entre el número de parejas concordantes y discordantes:",
                  tableOutput("k7"),
                  "Se calcula el coeficiente de correlación \\( \\tau-a \\) de Kendall \\(\\tau_{obs}\\) (siendo \\(n\\) el tamaño muestral):",
                  h4("\\( \\tau_{obs} = \\frac{k}{\\frac{n(n-1)}{2}} \\)"),
                  tableOutput("tao7"),
                  "Si \\(n \\leq 30\\) se busca en la ",
                  a("Tabla 12 (Tau)", target="_blank", href="Tabla_12_13_K_S.pdf"),
                  "el valor de \\(n\\) y \\( \\alpha \\) y se obtiene el cuantil de rechazo \\(\\tau_{crit}\\).",
                  br(),
                  br(),
                  "\\(|\\tau_{obs}| < \\tau_{crit} \\Rightarrow\\) Se acepta \\(H_{0}\\)",
                  br(),
                  "\\(|\\tau_{obs}| \\geq \\tau_{crit} \\Rightarrow\\) Se rechaza \\(H_{0}\\)",
                  br(),
                  br(),
                  "Si \\(n > 30\\) se utiliza la distribución asintótica (Distribución Normal) y se compara el estadístico \\(Z_{obs}\\) 
                  con el cuantil que corresponda segun el tipo de contraste.",
                  br(),
                  br(),
                  "Se calcula \\(E[\\tau]\\) y \\(Var[\\tau]\\):",
                  h4("\\( E[\\tau] = 0 \\)",
                     "\\( \\qquad \\)",
                     " \\(Var[\\tau] = \\frac{2(2n+5)}{9n(n-1)} \\)"),
                  br(),
                  tableOutput("Tao_normal"), #media y varianza
                  br(),
                  "Se tipifica el estadístico \\(\\tau_{obs}\\) obteniendo \\(Z_{obs}\\) :",
                  h4("\\( Z_{obs} = \\frac{(\\tau_{obs} - E[\\tau])}{\\sqrt{Var[\\tau]}} \\approx N(0,1) \\)"),
                  tableOutput("Z_obs7"),
                  br(),
                  "Se compara \\(|Z_{obs}|\\) con el cuantil \\(z_{ 1-\\frac{\\alpha}{2} }\\):",
                  br(),
                  br(),
                  "Si \\(|Z_{obs}| < z_{ 1-\\frac{\\alpha}{2} } \\Rightarrow \\) Se acepta \\(H_{0}\\)",
                  br(),
                  "Si \\(|Z_{obs}| \\geq z_{ 1-\\frac{\\alpha}{2} } \\Rightarrow \\) Se rechaza \\(H_{0}\\)",
                  br(),
                  br(),
                  "Para \\( \\alpha \\) = 5% :",
                  h5("\\( z_{ 1-\\frac{\\alpha}{2} } = 1,96 \\)" ),
                  br(),
                  "Más cuantiles \\(z_{ 1-\\frac{\\alpha}{2} }\\) : ",
                  a("Tabla 1,2,3 (N)", target="_blank", href="Tabla_1_2_3_Normal.pdf"),
                  br(),
                  br(),
                  "Es habitual realizar un gráfico de dispersión para analizar visualmente si existe algun tipo de asociación entre las variables: ",
                  plotOutput("graf7", width = "80%")
          ),
          tabPanel(title = "Independencia: Spearman",
                  br(),
                  "Si se quiere estudiar la relación entre dos variables pareadas con unidades distintas sólo podemos compararlas dos a dos.
                  En este apartado se utiliza la prueba de Spearman para contrastar la asociación de dos muestras. (Las variables no necesitan
                  seguir una distribución normal como en el cálculo de la correlación de Pearson.)",
                  br(),
                  h3("Hipótesis:"),
                  h2("\\(\\left\\{  
                           H_{0}: \\enspace r_{xy} \\enspace = \\enspace 0 \\atop
                           H_{1}: \\enspace r_{xy} \\enspace \\neq \\enspace 0
                      \\right.\\)"),
                  br(),
                  uiOutput("link8"),
                  br(),
                  h3("Funcionamiento:"),
                  "En la sección lateral se puede seleccionar el tipo de variables y el tamaño muestral.",
                  br(),
                  "Al clicar en 'Nuevas Muestras' se generan dos muestras con los valores elegidos.",
                  br(),
                  "Es posible ocultar los resultados deshabilitando las casillas en 'Mostrar Soluciones:'",
                  hr(),
                  h2("Datos:"),
                  p("*Las categorías de las variables ordinales están expresadas como números enteros
                  (Ej: 'Mal'= 1, 'Regular'= 2, 'Bien'= 3)", style = "color:red"),
                  br(),
                  "\\(X\\):",
                  h3(strong(verbatimTextOutput("datos8_1"))),
                  "\\(Y\\):",
                  h3(strong(verbatimTextOutput("datos8_2"))),
                  br(),
                  "Primero se calculan los rangos de las dos variables por separado \\(R_{i}\\), \\(S_{i}\\):",
                  verbatimTextOutput("rangos8_1"),
                  verbatimTextOutput("rangos8_2"),
                  br(),
                  "Se obtiene el estadístico \\(r_{obs}\\) (\\(n\\) = tamaño muestral):",
                  h4("\\( r_{obs} = \\frac{12}{(n^{3}-n)}·\\sum_{i=1}^{n} (R_{i}-\\frac{n+1}{2})·(S_{i}-\\frac{n+1}{2}) \\)"),
                  tableOutput("r_s8"),
                  "Si \\(n \\leq 30\\) se busca en la ",
                  a("Tabla 13 (RS)", target="_blank", href="Tabla_12_13_K_S.pdf"),
                  "el valor de \\(n\\) y \\( \\alpha \\) y se obtiene el cuantil de rechazo \\(r_{crit}\\).",
                  br(),
                  br(),
                  "\\(|r_{obs}| < r_{crit} \\Rightarrow\\) Se acepta \\(H_{0}\\)",
                  br(),
                  "\\(|r_{obs}| \\geq r_{crit} \\Rightarrow\\) Se rechaza \\(H_{0}\\)",
                  br(),
                  br(),
                  "Si \\(n > 30\\) se utiliza la distribución t-Student (El estadístico \\(r_{s}\\) no sigue una distribución asintótica conocida):",
                  h4( "\\( t_{obs} = r_{obs}·\\sqrt{ \\frac{(n-2)}{(1-r_{obs}^{2})} } \\approx t_{n-2} \\)" ),
                  tableOutput("tobs8"),
                  br(),
                  "Comparamos el valor de \\(|t_{obs}|\\) con el cuantil \\(t_{n-2;1-\\frac{\\alpha}{2}}\\):",
                  br(),
                  br(),
                  "Si \\(|t_{obs}| < t_{n-2;1-\\frac{\\alpha}{2}} \\Rightarrow \\) Se acepta \\(H_{0}\\)",
                  br(),
                  "Si \\(|t_{obs}| \\geq t_{n-2;1-\\frac{\\alpha}{2}} \\Rightarrow \\) Se rechaza \\(H_{0}\\)",
                  br(),
                  br(),
                  "Para \\( \\alpha \\) = 5% :",
                  uiOutput("cuantilt8",container = tags$h5),
                  br(),
                  "Más cuantiles \\(t_{ n-2;1-\\frac{\\alpha}{2} }\\) : ",
                  a("Tabla 14 (t)", target="_blank", href="Tabla_14_t.pdf"),
                  br(),
                  "\\(n-2 \\rightarrow \\) grados libertad (filas)",
                  br(),
                  "\\(1-\\frac{\\alpha}{2} \\rightarrow \\) columnas",
                  br(),
                  br(),
                  "Es habitual realizar un gráfico de dispersión para analizar visualmente si existe algun tipo de asociación entre las variables: ",
                  plotOutput("graf8", width = "80%")
          ),
          id = "conditionedpanels"
        )
      )
    )
  ),
  tabPanel("Datos",
           sidebarLayout(
             sidebarPanel(
               "¿No tiene datos? Puede utilizar el dataset de ejemplo 'Empleados':",
               br(),
               br(),
               selectInput("dataset", label = "Dataset:", choices = c("Empleados"="empleados","Archivo Importado"="imp")),
               hr(),
               h2("Importar Datos:"),
               br(),
               fileInput(inputId = "file", "Subir Archivo:", accept = c(".csv", ".txt", ".xls",".xlsx"), buttonLabel = "Examinar...", placeholder = "Sin Datos"),
               selectInput("tipo_archivo","Tipo de Archivo:", c("CSV" = "csv", "TXT" = "txt", "EXCEL" = "excel", "SPSS"="spss")),
               checkboxInput("header", "¿Están en la primera fila los nombres de las variables?", TRUE),
               radioButtons("sep","Tipo de Separador:*", c("Coma: ','" = ",","Tabulador" = "\t", "Punto y Coma: ';'" = ";")),
               radioButtons("dec", "Tipo de Decimal:*", c("Coma: ','" = ",", "Punto: '.'" = ".")),
               br(),
               p("* El tipo de separador y el tipo de decimal sólo funcionan con archivos .txt", style = "color:red"),
               p("* Los archivos csv utilizan la coma ',' como separador y el punto '.' para identificar decimales.", style = "color:red")
             ),
             mainPanel(
               h3("Archivo importado:"),
               br(),
               dataTableOutput("h"),
               p(textOutput("errorDatos")),
               textOutput("dim")
             )
           )
  ),
  navbarMenu("Análisis",
           tabPanel("Aleatoriedad: W-W",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var1","Variable",""),
                 selectInput("corte1","Umbral:", 
                             c("Mediana"="median", "Media"="mean", "Valor"="v")),
                 numericInput("value1","Valor:",1),
                 checkboxInput("exact1", "¿Estadístico y p-valor exacto?*", FALSE),
                 p(actionButton("calcular1", "Realizar Contraste"), align = "left"),
                 p("*Los valores que coincidan con el umbral no serán tenidos en cuenta", style = "color:red"),
                 p("*Las variables cualitativas no utilizan un umbral", style = "color:red"),
                 p("*Si la variable cualitativa tiene más de dos categorías se enfrentará la categoría del primer elemento al resto de categorías", style = "color:red")
               ),
               mainPanel(
                 h1("Aleatoriedad: Prueba de las rachas de Wald-Wolfowitz"),
                 br(),
                 h3("Hipótesis:"),
                 h2("\\(\\left\\{  
                      H_{0}: \\enspace Muestra \\enspace Aleatoria \\atop
                      H_{1}: \\enspace Muestra \\enspace No \\enspace Aleatoria
                   \\right.\\)"),
                 hr(),
                 h3("Valores elegidos:"),
                 textOutput("elegida1"),
                 textOutput("corte1"),
                 textOutput("value1"),
                 textOutput("exact1"),
                 hr(),
                 h3("Resultados:"),
                 uiOutput("runs1"),
                 uiOutput("estadistico1"),
                 textOutput("p_valor1"),
                 br(),
                 h4("Gráfico:"),
                 plotOutput("grafico1", width = "80%")
               )
             )
           ),
           tabPanel("Bondad de Ajuste: K-S (F=Fo)",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var2","Variable",""),
                        selectInput("con2", "Distribución Conocida:",
                                    c("Normal"="pnorm", "Exponencial"="pexp", "Chi-Cuadrado"="pchisq",
                                      "t-Student"="pt")),
                        numericInput("par2_1","Parámetro 1:",1),
                        numericInput("par2_2","Parámetro 2:",1),
                        checkboxInput("summ2", "¿Calcular Media, Mediana y Desviación Típica?", TRUE),
                        checkboxInput("exact2", "¿Estadístico y p-valor exacto?*", FALSE),
                        p("*Si hay empates no se podrán obtener los valores exactos", style = "color:red"),
                        p(actionButton("calcular2", "Realizar Contraste"), align = "left"),
                        br(),
                        p("*Parámetro 1: Media/Landa/Grados Libertad", style = "color:red"),
                        p("*Parámetro 2: Desviación Estándar", style = "color:red")
                      ),
                      mainPanel(
                        h1("Bondad de Ajuste: Prueba de Kolmogorov-Smirnov  \\(F=F_{0}\\) "),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                             H_{0}: \\enspace F \\enspace = \\enspace F_{0} \\atop
                             H_{1}: \\enspace F \\enspace \\neq \\enspace F_{0}
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("elegida2"),
                        textOutput("distribucion2"),
                        textOutput("param_eleg_1"),
                        textOutput("param_eleg_2"),
                        textOutput("summ2"),
                        textOutput("exact2"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico2"),
                        textOutput("p_valor2"),
                        br(),
                        tableOutput("summary2"),
                        br(),
                        h4("Gráfico:"),
                        splitLayout(
                          plotOutput("grafico2", width = "90%"),
                          plotOutput("grafico2_2", width = "90%")
                        )
                      )
                    )
            ),
           tabPanel("Bondad de Ajuste: K-S (F=G)",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var3_1","Variable 1:",""),
                        selectInput("var3_2","Variable 2:",""),
                        checkboxInput("exact3", "¿Estadístico y p-valor exacto?*", FALSE),
                        p("*Si hay empates no se podrán obtener los valores exactos", style = "color:red"),
                        p(actionButton("calcular3", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("Bondad de Ajuste: Prueba de Kolmogorov-Smirnov \\(F=G\\)"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                           H_{0}: \\enspace F \\enspace = \\enspace G \\atop
                           H_{1}: \\enspace F \\enspace \\neq \\enspace G
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("elegida3"),
                        textOutput("exact3"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico3"),
                        textOutput("p_valor3"),
                        br(),
                        h4("Gráfico:"),
                        plotOutput("grafico3", width = "70%")
                      )
                    )
           ),
           tabPanel("Localización Muestra: W",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var4","Variable",""),
                        selectInput("contraste4", "Tipo de contraste:",
                                    c("Bilateral (H0: = )" = "t", "Unilateral inferior (H1: < )" = "l", "Unilateral superior (H1: > )" = "g")),
                        numericInput("mu4","\\(\\theta_{0}\\)",0),
                        checkboxInput("summ4", "¿Calcular Media, Mediana y Desviación Típica?", TRUE),
                        p(actionButton("calcular4", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("Localización Muestra: Rangos con signo de Wilcoxon"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                           H_{0}: \\enspace \\theta \\enspace = \\enspace \\theta_{0} \\atop
                           H_{1}: \\enspace \\theta \\enspace \\neq \\enspace \\theta_{0}
                           \\right.\\)",
                           "\\(\\qquad\\)",      # ESPACIO
                           "\\(\\left\\{  
                           H_{0}: \\enspace \\theta \\enspace \\leq \\enspace \\theta_{0} \\atop
                           H_{1}: \\enspace \\theta \\enspace > \\enspace \\theta_{0}
                           \\right.\\)",
                           "\\(\\qquad\\)",      # ESPACIO
                           "\\(\\left\\{  
                           H_{0}: \\enspace \\theta \\enspace \\geq \\enspace \\theta_{0} \\atop
                           H_{1}: \\enspace \\theta \\enspace < \\enspace \\theta_{0}
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("elegida4"),
                        textOutput("contraste_eleg4"),
                        uiOutput("mu_eleg4"),
                        textOutput("summ4"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico4"),
                        textOutput("p_valor4"),
                        br(),
                        tableOutput("summary4"),
                        br(),
                        h4("Gráfico:"),
                        p("En rojo aparece el valor de la mediana en las hipótesis (\\(\\theta_{0}\\))", style = "color:red"),
                        plotOutput("grafico4", width = "80%")
                      )
                    )
           ),
           tabPanel("Dos muestras independientes: U M-W",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var5_1","Variable Agrupación:",""),
                        selectInput("var5_2","Variable X:",""),
                        selectInput("contraste5", "Tipo de contraste:",
                                    c("Bilateral (H0: X1=X2)" = "t", "Unilateral inferior (H1: X1>X2)" = "g", "Unilateral superior (H1: X1<X2)" = "l")),
                        checkboxInput("exact5", "¿Estadístico y p-valor exacto?*", FALSE),
                        p("*Si hay empates no se podrán obtener los valores exactos", style = "color:red"),
                        checkboxInput("correct5", "¿Corrección por continuidad?", FALSE),
                        p(actionButton("calcular5", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("Dos muestras independientes: U de Mann-Whitney"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                           H_{0}: \\enspace X_{1} \\enspace = \\enspace X_{2} \\atop
                           H_{1}: \\enspace X_{1} \\enspace \\neq \\enspace X_{2}
                           \\right.\\)",
                           "\\(\\qquad\\)",      # ESPACIO
                           "\\(\\left\\{  
                           H_{0}: \\enspace X_{1} \\enspace \\leq \\enspace X_{2} \\atop
                           H_{1}: \\enspace X_{1} \\enspace > \\enspace X_{2}
                           \\right.\\)",
                           "\\(\\qquad\\)",      # ESPACIO
                           "\\(\\left\\{  
                           H_{0}: \\enspace X_{1} \\enspace \\geq \\enspace X_{2} \\atop
                           H_{1}: \\enspace X_{1} \\enspace < \\enspace X_{2}
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("elegida5"),
                        textOutput("contraste_eleg5"),
                        textOutput("exact5"),
                        textOutput("correct5"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico5"),
                        textOutput("p_valor5"),
                        br(),
                        h4("Gráfico:"),
                        plotOutput("grafico5", width = "80%")
                      )
                    )
           ),
           tabPanel("Dos muestras relacionadas: Apareados W",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var6_1","Variable 1:",""),
                        selectInput("var6_2","Variable 2:",""),
                        selectInput("contraste6", "Tipo de contraste:",
                                    c("Bilateral (H0: X=Y)" = "t", "Unilateral inferior (H1: X>Y)" = "g", "Unilateral superior (H1: X<Y)" = "l")),
                        checkboxInput("exact6", "¿Estadístico y p-valor exacto?*", FALSE),
                        p("*Si hay empates no se podrán obtener los valores exactos", style = "color:red"),
                        checkboxInput("correct6", "¿Corrección por continuidad?", FALSE),
                        p(actionButton("calcular6", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("Dos muestras relacionadas: Datos apareados de Wilcoxon"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                           H_{0}: \\enspace X \\enspace = \\enspace Y \\atop
                           H_{1}: \\enspace X \\enspace \\neq \\enspace Y
                           \\right.\\)",
                           "\\(\\qquad\\)",      # ESPACIO
                           "\\(\\left\\{  
                           H_{0}: \\enspace X \\enspace \\leq \\enspace Y \\atop
                           H_{1}: \\enspace X \\enspace > \\enspace Y
                           \\right.\\)",
                           "\\(\\qquad\\)",      # ESPACIO
                           "\\(\\left\\{  
                           H_{0}: \\enspace X \\enspace \\geq \\enspace Y \\atop
                           H_{1}: \\enspace X \\enspace < \\enspace Y
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("elegida6"),
                        textOutput("contraste_eleg6"),
                        textOutput("exact6"),
                        textOutput("correct6"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico6"),
                        textOutput("p_valor6"),
                        br(),
                        h4("Gráfico:"),
                        plotOutput("grafico6", width = "80%")
                      )
                    )
           ),
           tabPanel("k muestras independientes: K-W",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var7_1","Variable Agrupación:",""),
                        selectInput("var7_2","Variable X:",""),
                        checkboxInput("posthoc_ind7","¿Incluir Análisis post-hoc?", value = F),
                        p(actionButton("calcular7", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("k muestras independientes: Prueba de Kruskal-Wallis"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                           H_{0}: \\enspace \\tau_{1} \\enspace = \\enspace \\tau_{2} \\enspace = \\enspace \\cdots \\enspace = \\enspace \\tau_{k} \\atop
                           H_{1}: \\enspace \\tau_{i} \\enspace \\neq \\enspace \\tau_{j} \\enspace para \\enspace algun \\enspace i \\neq j
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("nvars7"),
                        textOutput("elegida7"),
                        textOutput("posthoc7"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico7"),
                        textOutput("p_valor7"),
                        br(),
                        h4("Gráfico:"),
                        plotOutput("grafico7", width = "90%"),
                        br(),
                        h4("Post-hoc, Tabla de p-valores:"),
                        tableOutput("posthoc_ind7")
                      )
                    )
           ),
           tabPanel("k muestras relacionadas: Friedman",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("nvars8","Número de Variables:",min = 3, max = 6, value = 3),
                        selectInput("var8_1","Variable 1:",""),
                        selectInput("var8_2","Variable 2:",""),
                        selectInput("var8_3","Variable 3:",""),
                        conditionalPanel(
                          condition = "input.nvars8 > 3",
                          selectInput("var8_4","Variable 4:","")
                        ),
                        conditionalPanel(
                          condition = "input.nvars8 > 4",
                          selectInput("var8_5","Variable 5:","")
                        ),
                        conditionalPanel(
                          condition = "input.nvars8 > 5",
                          selectInput("var8_6","Variable 6:","")
                        ),
                        checkboxInput("posthoc_rel8","¿Incluir Análisis post-hoc?", value = F),
                        p(actionButton("calcular8", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("k muestras relacionadas: Prueba de Friedman"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                           H_{0}: \\enspace \\tau_{1} \\enspace = \\enspace \\tau_{2} \\enspace = \\enspace \\cdots \\enspace = \\enspace \\tau_{k} \\atop
                           H_{1}: \\enspace \\tau_{i} \\enspace \\neq \\enspace \\tau_{j} \\enspace para \\enspace algun \\enspace i \\neq j
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("nvars8"),
                        textOutput("elegida8"),
                        textOutput("posthoc8"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("estadistico8"),
                        textOutput("p_valor8"),
                        br(),
                        h4("Gráfico:"),
                        plotOutput("grafico8", width = "90%"),
                        br(),
                        h4("Post-hoc, Tabla de p-valores:"),
                        tableOutput("posthoc_rel8")
                      )
                    )
           ),
           tabPanel("Independencia: Kendall y Spearman",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("var9_1","Variable 1",""),
                        selectInput("var9_2","Variable 2",""),
                        selectInput("metodo9","Método:",
                                    c("Kendall"="kendall", "Spearman"="spearman")),
                        checkboxInput("exact9", "¿Estadístico y p-valor exacto?*", FALSE),
                        p("*Si hay empates no se podrán obtener los valores exactos", style = "color:red"),
                        checkboxInput("continuity9", "¿Corrección por continuidad?", FALSE),
                        p(actionButton("calcular9", "Realizar Contraste"), align = "left")
                      ),
                      mainPanel(
                        h1("Independencia: Pruebas de Kendall y Spearman"),
                        br(),
                        h3("Hipótesis:"),
                        h2("\\(\\left\\{  
                          H_{0}: \\enspace Variables \\enspace Independientes \\atop
                          H_{1}: \\enspace Variables \\enspace Dependientes
                        \\right.\\)",
                           "\\(\\qquad\\)",
                           "\\(\\left\\{  
                           H_{0}: \\enspace r_{xy} \\enspace = \\enspace 0 \\atop
                           H_{1}: \\enspace r_{xy} \\enspace \\neq \\enspace 0
                           \\right.\\)"),
                        hr(),
                        h3("Valores elegidos:"),
                        textOutput("elegida9"),
                        textOutput("metodo9"),
                        textOutput("exact9"),
                        textOutput("continuity9"),
                        hr(),
                        h3("Resultados:"),
                        uiOutput("asociacion9"),
                        uiOutput("estadistico9"),
                        textOutput("p_valor9"),
                        br(),
                        h4("Gráfico:"),
                        plotOutput("grafico9", width = "80%")
                      )
                    )
           )
  )
)

##########################################################################################################
#                                                 Server                                                 #
##########################################################################################################

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  ##### Aprende: Aleatoriedad #####
  
  datos1 <- reactive({
    input$generar_datos
    isolate({
      if(input$variable == "M"){
        return(generar_moneda(input$cantidad_datos))
      }else{
        return(round(generar_numeros(input$cantidad_datos)*90-45,0))
      }
    })
  })
  
  test_rachas <- reactive({
    if(typeof(datos1()[1]) == "double"){
      return(runs.test(datos1()))
    } else {
      return(runs.test(rachas_discreta(datos1()), threshold = 0.5))
    }
  })
  
  output$med1 <- renderTable({
    if(input$n1 == T & is.numeric(datos1()) == T){
      med <- as.data.frame(matrix(c(median(datos1())), ncol=1))
      colnames(med) <- c("Me")
      rownames(med) <- c("Valor")
      med
    } else{
      NULL
    }
  }, digits = 1)
  
  output$n1 <- renderTable({
    if(input$n1 == T){
      n1 <- as.data.frame(matrix(test_rachas()$parameter[2:4], ncol=3))
      colnames(n1) <- c("n1", "n2", "n")
      rownames(n1) <- c("Valores")
      n1
    }
  }, digits = 3)
  
  output$rachas <- renderTable({
    if(input$rachas == T){
      rachas <- as.data.frame(c(test_rachas()$runs))
      colnames(rachas) <- c("Rachas Observadas")
      rownames(rachas) <- c("Valor")
      rachas
    }
  }, digits = 1)
  
  output$estadistico_rachas <- renderTable({
    if(input$estadistico_rachas == T){
      estadistico_rachas <- as.data.frame(matrix(c(test_rachas()$statistic), ncol=1))
      colnames(estadistico_rachas) <- c("Zobs")
      rownames(estadistico_rachas) <- c("Valor")
      estadistico_rachas
    }
  }, digits = 4)
  
  output$e_var1 <- renderTable({
    if(input$e_var1 == T){
      e_var <- as.data.frame(matrix(c(test_rachas()$mu, test_rachas()$var), ncol=2))
      colnames(e_var) <- c("E[R]", "Var[R]")
      rownames(e_var) <- c("Valores")
      e_var
    }
  }, digits = 3)
  
  output$datos1 <- renderText({
    datos1()
  })
  
  output$link1 <- renderUI({
    url1 <- a("Test de las Rachas de Wald-Wolfowitz", href="https://en.wikipedia.org/wiki/Wald-Wolfowitz_runs_test")
    tagList("+ Info: ", url1)
  })
  
  output$graf1 <- renderPlot({
    if(input$graf1 == T){
      datos <- datos1()
      if(is.numeric(datos) == T){
        plot(datos, xlab = "Orden", ylab = "X")
        abline(median(datos),0, col="red")
      } else{
        plot(rachas_discreta(datos), xlab = "Orden", ylab = "X")
      }
    } else{
      NULL
    }
  })
  
  ##### Aprende: B.Ajuste (F=Fo) #####
  
  sim_baj1 <- reactive({
    input$generar_datos2
    isolate({
      return(sim_b_aj(n1=input$cantidad_datos2))
    })
  })
  
  ajuste2 <- reactive({
    lista <- sim_baj1()
    xord <- sort(lista$datos)
    dist_emp <- round(sapply(xord, cond, xord=xord)/length(xord), 4) #distribucion empirica de x
    if(lista$tipo == "Normal"){
      dist_con <- round(sapply(xord, pnorm, mean=lista$media, sd=lista$desv), 4) #distribucion conocida normal
    } else{
      dist_con <- round(sapply(xord, pexp, rate=lista$landa), 4) #distribucion conocida exponencial
    }
    diferencias <- abs(dist_emp-dist_con)
    D <- max(diferencias)
    return(list(xord=xord,dist_emp=dist_emp,dist_con=dist_con,diferencias=diferencias,D=D))
  })
  
  output$ordenados2 <- renderText({
    if(input$ordenados2 == T){
      return(ajuste2()$xord)
    }
  }) 
  
  output$empirica2 <- renderText({
    if(input$empirica2 == T){
      return(ajuste2()$dist_emp)
    }
  }) 
  
  output$conocida2 <- renderText({
    if(input$conocida2 == T){
      return(ajuste2()$dist_con)
    }
  }) 
  
  output$diferencias2 <- renderText({
    if(input$diferencias2 == T){
      return(ajuste2()$diferencias)
    }
  }) 
  
  output$D2 <- renderTable({
    if(input$D2 == T){
      D2 <- as.data.frame(c(ajuste2()$D))
      colnames(D2) <- c("D obs")
      rownames(D2) <- c("Valor")
      return(D2)
    }
  }, digits = 4) 
  
  output$qqplot2 <- renderPlot({
    if(input$qqplot2 == T){
      if(sim_baj1()$tipo == "Normal"){
        qqPlot(sim_baj1()$datos, distribution = "norm", mean=sim_baj1()$media, sd=sim_baj1()$desv, id=F, line = "none", envelope = F, 
               xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestra", main = "Gráfico Q-Q Normal")
        abline(0,1, col="blue", lwd=2)
      } else{
        qqPlot(sim_baj1()$datos, distribution = "exp", rate=sim_baj1()$landa, id=F, line = "none", envelope = F,
               xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestra", main = "Gráfico Q-Q Exponencial")
        abline(0,1, col="blue", lwd=2)
      }
    }
  })
  
  output$hist2 <- renderPlot({
    if(input$hist2 == T){
      if(sim_baj1()$tipo == "Normal"){
        hist(sim_baj1()$datos, freq = F,col="lightblue", main = "Histograma + Densidad", xlab = "X")
        curve(dnorm(x,sim_baj1()$media,sim_baj1()$desv), col="red", lwd=2, add = T)
      } else{
        hist(sim_baj1()$datos, freq = F, col="lightblue", main = "Histograma + Densidad", xlab = "X")
        curve(dexp(x,sim_baj1()$landa), col="red", lwd=2, add = T)
      }
    }
  })
  
  output$datos2 <- renderText({
      return(sim_baj1()$datos)
  })
  
  output$enunciado2 <- renderUI({
    if(sim_baj1()$tipo == "Normal"){
      withMathJax(paste0("Comprueba si los datos provienen de una distribución ", sim_baj1()$tipo, " con parámetros:  \\(\\mu\\) = ", 
                         sim_baj1()$media, " y \\(\\sigma\\) = ", sim_baj1()$desv))
    } else{
      withMathJax(paste0("Comprueba si los datos provienen de una distribución ", sim_baj1()$tipo, " con parámetros:  \\(\\lambda\\) = ", sim_baj1()$landa)) 
    }
  })
  
  output$link2_1 <- renderUI({
    url2_1 <- a("Distribución Empírica", href="https://en.wikipedia.org/wiki/Empirical_distribution_function")
    tagList("+ Info: ", url2_1)
  })
  
  output$link2_2 <- renderUI({
    url2_2 <- a("Prueba de Kolmogorov-Smirnov", href="https://en.wikipedia.org/wiki/Kolmogorov-Smirnov_test")
    tagList("+ Info: ", url2_2)
  })
  
  ##### Aprende: B.Ajuste (F=G) #####
  
  sim_baj2 <- reactive({
    input$generar_datos3
    isolate({
      return(sim_b_aj(c="2",n1=input$cantidad_datos3_1, n2=input$cantidad_datos3_2))
    })
  })
  
  ajuste3 <- reactive({
    lista <- sim_baj2()
    x <- lista$S
    y <- lista$G
    xord <- sort(x)
    yord <- sort(y)
    m <- rbind(c(x,y),c(rep(1,times=length(x)),rep(2,times=length(y))))
    row.names(m) <- c("X","Y")
    mord <- m[,order(m["X",])] #matriz con primera fila valores x,y ordenados, segunda fila indica a que muestra pertenece el valor (1 o 2)
    fx <- c(0)
    fy <- c(0)
    for(i in 1:length(m[1,])) {
      if(mord[2,i] == 1){
        fx <- c(fx, sum(xord<=mord[1,i]))
        fy <- c(fy,tail(fy,1))
      } else{
        fx <- c(fx,tail(fx,1))
        fy <- c(fy, sum(yord<=mord[1,i]))
      }
    }
    fx <- fx[-1] ; fy <- fy[-1] #eliminamos el primer 0 auxiliar de ambos vectores
    fx <- fx/length(x) ; fy <- fy/length(y)
    diferencias <- abs(fx-fy)
    D <- max(diferencias)
    return(list(xyord=sort(c(x,y)),fx=round(fx,4),fy=round(fy,4),diferencias=round(diferencias,4),D=D))
  })
  
  output$ordenados3 <- renderText({
    if(input$ordenados3 == T){
      return(ajuste3()$xyord)
    }
  }) 
  
  output$empirica3_1 <- renderText({
    if(input$empirica3_1 == T){
      return(ajuste3()$fx)
    }
  }) 
  
  output$empirica3_2 <- renderText({
    if(input$empirica3_2 == T){
      return(ajuste3()$fy)
    }
  }) 
  
  output$diferencias3 <- renderText({
    if(input$diferencias3 == T){
      return(ajuste3()$diferencias)
    }
  }) 
  
  output$D3 <- renderTable({
    if(input$D3 == T){
      D3 <- as.data.frame(c(ajuste3()$D))
      colnames(D3) <- c("D obs")
      rownames(D3) <- c("Valor")
      return(D3)
    }
  }, digits = 4) 
  
  output$qqplot3 <- renderPlot({
    if(input$qqplot3 == T){
        qqplot(sim_baj2()$S, sim_baj2()$G, xlab = "Cuantiles Muestra X", ylab = "Cuantiles Muestra Y", main = "Gráfico Q-Q")
        abline(0,1,col="red")
    }
  })
  
  output$datos3_1 <- renderText({
      return(sim_baj2()$S)
  })
  
  output$datos3_2 <- renderText({
    return(sim_baj2()$G)
  })
  
  output$enunciado3 <- renderText({
    "Comprueba si los datos de ambas muestras provienen de la misma distribución de probabilidad"
  })

  output$link3_1 <- renderUI({
    url3_1 <- a("Distribución Empírica", href="https://en.wikipedia.org/wiki/Empirical_distribution_function")
    tagList("+ Info: ", url3_1)
  })
  
  output$link3_2 <- renderUI({
    url3_2 <- a("Prueba de Kolmogorov-Smirnov", href="https://en.wikipedia.org/wiki/Kolmogorov-Smirnov_test#Two-sample_Kolmogorov-Smirnov_test")
    tagList("+ Info: ", url3_2)
  })
  
  ##### Aprende: Localizacion muestra (Me) #####
  
  datos4 <- reactive({
    input$generar_datos4
    isolate({
      return(round(generar_numeros(input$cantidad_datos4)*90-45,2))
    })
  })
  
  contraste_D <- reactive({
    mu0 <- round(median(datos4()),0)+round(runif(1)*20-10,0)
    D <- datos4()-mu0
    D_Abs <- abs(D)
    D_Ranks <- rank(D_Abs)
    T_obs <- sum(D_Ranks*(D>0))
    n <- length(D)
    T_normal <- list(E=(n*(n+1))/4,Var=n*(n+1)*(2*n+1)/24) #varianza sin empates
    Z_obs <- (T_obs - T_normal$E)/sqrt(T_normal$Var)
    return(list(D=D,D_Abs=D_Abs,D_Ranks=D_Ranks,T_obs=T_obs,n=n,T_normal=T_normal,Z_obs=Z_obs, mu0=mu0))
  })
  
  output$D <- renderText({
    if(input$D == T){
      return(contraste_D()$D)
    }
  }) 
  
  output$D_Abs <- renderText({
    if(input$D_Abs == T){
      return(contraste_D()$D_Abs)
    }
  }) 
  
  output$D_Ranks <- renderText({
    if(input$D_Abs == T){
      return(contraste_D()$D_Ranks)
    }
  }) 
  
  output$T_obs <- renderTable({
    if(input$T_obs == T){
      T_obs <- as.data.frame(c(contraste_D()$T_obs))
      colnames(T_obs) <- c("T+ obs")
      rownames(T_obs) <- c("Valor")
      return(T_obs)
    }
  }, digits = 2)
  
  output$T_normal <- renderTable({
    if(input$T_normal == T){
      T_normal <- as.data.frame(matrix(c(contraste_D()$T_normal[1], contraste_D()$T_normal[2]), ncol=2))
      colnames(T_normal) <- c("E[T+]", "Var[T+]")
      rownames(T_normal) <- c("Valores")
      return(T_normal)
    }
  }, digits = 3)
  
  output$Z_obs <- renderTable({
    if(input$Z_obs == T){
      Z_obs <- as.data.frame(c(contraste_D()$Z_obs))
      colnames(Z_obs) <- c("Zobs")
      rownames(Z_obs) <- c("Valor")
      return(Z_obs)
    }
  }, digits = 4)
  
  output$enunciado4 <- renderText({
    u <- runif(1)
    if(u <= (1/3)){
      return(paste0("Contrasta si la mediana de la distribución es igual a ", contraste_D()$mu0))
    } else if(u > (2/3)){
      return(paste0("Contrasta si la mediana de la distribución es menor o igual a ", contraste_D()$mu0))
    } else {
      return(paste0("Contrasta si la mediana de la distribución es mayor o igual a ", contraste_D()$mu0))
    }
  })
  
  output$datos4 <- renderText({
    datos4()
  })
  
  output$link4 <- renderUI({
    url4 <- a("Prueba de los rangos con signo de Wilcoxon", href="https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test")
    tagList("+ Info: ", url4)
  })
  
  output$graf4 <- renderPlot({
    if(input$graf4 == T){
      datos <- datos4()
      boxplot(datos, xlab = "X", col = "lightblue")
      abline(contraste_D()$mu0,0, col="red")
    } else{
      NULL
    }
  })
  
  ##### Aprende: Independientes U M-W #####
  
  datos5 <- reactive({
    input$generar_datos5
    isolate({
      if(input$variable5 == "C"){
        return(list(X=round(generar_numeros(input$cantidad_datos5_1)*17+8,1), #U(8,25)
                    Y=round(generar_numeros(input$cantidad_datos5_2)*15+6,1))) #U(6,21)
      } else {
        return(list(X=round(generar_numeros(input$cantidad_datos5_1)*3+1,0), #
                    Y=round(generar_numeros(input$cantidad_datos5_2)*6+1,0))) #
      } 
    })
  })
  
  umw5 <- reactive({
    x <- datos5()$X
    y <- datos5()$Y
    n1 <- length(x)
    n2 <- length(y)
    n <- n1+n2
    rangos <- rank(c(x,y)) #rangos X e Y
    rangX <- rangos[1:n1] #rangos X
    rangY <- rangos[(n1+1):n] #rangos Y
    R <- list(rangX=sum(rangX), rangY=sum(rangY))
    U <- list(U1=R$rangX-(n1*(n1+1))/2, U2= R$rangY-(n2*(n2+1))/2)
    Uobs <- min(R$rangX-(n1*(n1+1))/2, R$rangY-(n2*(n2+1))/2)
    E <- n1*n2/2
    VAR <- n1*n2*(n1+n2+1)/12 #varianza sin empates
    Z <- (U$U1 - E)/sqrt(VAR)
    return(list(n1=n1,n2=n2,n=n,rangX=rangX,rangY=rangY,R=R,U=U,Uobs=Uobs,E=E,VAR=VAR,Z=Z))
  })
  
  output$n5 <- renderTable({
    if(input$n5 == T){
      n5 <- as.data.frame(matrix(c(umw5()$n1, umw5()$n2, umw5()$n), ncol=3))
      colnames(n5) <- c("n1", "n2", "n")
      rownames(n5) <- c("Valores")
      return(n5)
    }
  }, digits = 0)
  
  output$rX5 <- renderText({
    if(input$rXY5 == T){
      return(umw5()$rangX)
    }
  })
  
  output$rY5 <- renderText({
    if(input$rXY5 == T){
      return(umw5()$rangY)
    }
  })
  
  output$R5 <- renderTable({
    if(input$R5 == T){
      R5 <- as.data.frame(matrix(c(umw5()$R[1], umw5()$R[2]), ncol=2))
      colnames(R5) <- c("R1", "R2")
      rownames(R5) <- c("Valores")
      return(R5)
    }
  }, digits = 1)
  
  output$U5 <- renderTable({
    if(input$U5 == T){
      U5 <- as.data.frame(matrix(c(umw5()$U[1], umw5()$U[2]), ncol=2))
      colnames(U5) <- c("U1", "U2")
      rownames(U5) <- c("Valores")
      return(U5)
    }
  }, digits = 1)
  
  output$Uobs5 <- renderTable({
    if(input$U5 == T){
      Uobs5 <- as.data.frame(c(umw5()$Uobs))
      colnames(Uobs5) <- c("Uobs")
      rownames(Uobs5) <- c("Valores")
      return(Uobs5)
    }
  }, digits = 1)
  
  output$tabla_U5 <- renderTable({
    if(input$tabla_U5 == T){
      tabla_U5 <- as.data.frame(matrix(c(umw5()$E, umw5()$VAR), ncol=2))
      colnames(tabla_U5) <- c("E[U]", "Var[U]")
      rownames(tabla_U5) <- c("Valores")
      return(tabla_U5)
    }
  }, digits = 3)
  
  output$Z5 <- renderTable({
    if(input$Z5 == T){
      Z5 <- as.data.frame(c(umw5()$Z))
      colnames(Z5) <- c("Zobs")
      rownames(Z5) <- c("Valor")
      return(Z5)
    }
  }, digits = 4)
  
  output$datos5_1 <- renderText({
    return(datos5()$X)
  })
  
  output$datos5_2 <- renderText({
    return(datos5()$Y)
  })
  
  output$link5 <- renderUI({
    url5 <- a("Prueba para dos muestras independientes de Mann-Whitney", href="https://en.wikipedia.org/wiki/Mann-Whitney_U_test")
    tagList("+ Info: ", url5)
  })
  
  output$graf5 <- renderPlot({
    if(input$graf5 == T){
      X <- datos5()$X
      Y <- datos5()$Y
      boxplot(list("X1"=X,"X2"=Y), col = c("lightblue", "lightgreen"), main="Box-Plots de X1 y X2")
    } else{
      NULL
    }
  })
  
  ##### Aprende: Apareados Wilcoxon #####
  
  datos6 <- reactive({
    input$generar_datos6
    isolate({
      if(input$variable6 == "C"){
        return(list(X=round(generar_numeros(input$cantidad_datos6)*17+8,1), #U(8,25)
                    Y=round(generar_numeros(input$cantidad_datos6)*15+6,1))) #U(6,21)
      } else {
        return(list(X=round(generar_numeros(input$cantidad_datos6)*3+1,0), #
                    Y=round(generar_numeros(input$cantidad_datos6)*6+1,0))) #
      } 
    })
  })
  
  wilcox6 <- reactive({
    x <- datos6()$X
    y <- datos6()$Y
    n <- length(x)
    Di <- y-x
    absDi <- abs(Di)
    rangDi <- rank(absDi)
    estT <- sum(rangDi[Di>0])
    T_normal <- list(E=(n*(n+1))/4,Var=n*(n+1)*(2*n+1)/24) #varianza sin empates
    Zobs <- (estT - T_normal$E)/sqrt(T_normal$Var)
    return(list(n=n, Di=Di,absDi=absDi,rangDi=rangDi, estT=estT, Zobs=Zobs, T_normal=T_normal))
  })
  
  output$Di6 <- renderText({
    if(input$Di6 == T){
      return(wilcox6()$Di)
    }
  })
  
  output$absDi6 <- renderText({
    if(input$absDi6 == T){
      return(wilcox6()$absDi)
    }
  })
  
  output$rangDi6 <- renderText({
    if(input$rangDi6 == T){
      return(wilcox6()$rangDi)
    }
  })
  
  output$estT6 <- renderTable({
    if(input$estT6 == T){
      estT <- as.data.frame(matrix(c(wilcox6()$estT), ncol=1))
      colnames(estT) <- c("T+ obs")
      rownames(estT) <- c("Valor")
      return(estT)
    }
  }, digits = 2)
  
  output$T_normal6 <- renderTable({
    if(input$T_normal6 == T){
      T_normal6 <- as.data.frame(matrix(c(wilcox6()$T_normal[1], wilcox6()$T_normal[2]), ncol=2))
      colnames(T_normal6) <- c("E[T+]", "Var[T+]")
      rownames(T_normal6) <- c("Valores")
      return(T_normal6)
    }
  }, digits = 3)
  
  output$Z_obs6 <- renderTable({
    if(input$Z_obs6 == T){
      Z_obs6 <- as.data.frame(c(wilcox6()$Zobs))
      colnames(Z_obs6) <- c("Zobs")
      rownames(Z_obs6) <- c("Valor")
      return(Z_obs6)
    }
  }, digits = 4)
  
  output$datos6_1 <- renderText({
    return(datos6()$X)
  })
  
  output$datos6_2 <- renderText({
    return(datos6()$Y)
  })
  
  output$link6 <- renderUI({
    url6 <- a("Datos apareados de Wilcoxon", href="https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test")
    tagList("+ Info: ", url6)
  })
  
  output$graf6 <- renderPlot({
    if(input$graf6 == T){
      X <- datos6()$X
      Y <- datos6()$Y
      boxplot(list("X"=X,"Y"=Y), col = c("lightblue", "lightgreen"), main="Box-Plots de X e Y")
    } else{
      NULL
    }
  })
  
  
  ##### Aprende: k independientes #####
  
  datos9 <- reactive({
    input$generar_datos9
    isolate({
      if(input$variable9 == "C"){
        return(list(X=round(generar_numeros(input$cantidad_datos9_1)*17+8,1), #U(8,25)
                    Y=round(generar_numeros(input$cantidad_datos9_2)*15+6,1), #U(6,21)
                    Z=round(generar_numeros(input$cantidad_datos9_3)*16+11,1))) #U(11,27)
      } else {
        return(list(X=round(generar_numeros(input$cantidad_datos9_1)*3+1,0), # 1,2,3,4
                    Y=round(generar_numeros(input$cantidad_datos9_2)*6+1,0), # 1,2,3,4,5,6,7
                    Z=round(generar_numeros(input$cantidad_datos9_3)*4+1,0))) # 1,2,3,4,5
      } 
    })
  })
  
  kw9 <- reactive({
    x <- datos9()$X
    y <- datos9()$Y
    z <- datos9()$Z
    n1 <- length(x)
    n2 <- length(y)
    n3 <- length(z)
    n <- n1+n2+n3
    rangos <- rank(c(x,y,z)) #rangos X, Y, Z
    rangX <- rangos[1:n1] #rangos X
    rangY <- rangos[(n1+1):(n1+n2)] #rangos Y
    rangZ <- rangos[(n1+n2+1):n] #rangos Z
    R <- list(rangX=sum(rangX), rangY=sum(rangY), rangZ=sum(rangZ))
    Ri <- as.vector(unlist(R))
    H <- 12/(n*(n+1))*sum(Ri*Ri/c(n1,n2,n3))-3*(n+1)   
    return(list(n1=n1,n2=n2,n3=n3,n=n,rangX=rangX,rangY=rangY,rangZ=rangZ,R=R,H=H))
  })
  
  output$n9 <- renderTable({
    if(input$n9 == T){
      n9 <- as.data.frame(matrix(c(kw9()$n1, kw9()$n2, kw9()$n3, kw9()$n), ncol=4))
      colnames(n9) <- c("n1", "n2", "n3", "N")
      rownames(n9) <- c("Valores")
      return(n9)
    }
  }, digits = 0)
  
  output$rX9 <- renderText({
    if(input$rXYZ9 == T){
      return(kw9()$rangX)
    }
  })
  
  output$rY9 <- renderText({
    if(input$rXYZ9 == T){
      return(kw9()$rangY)
    }
  })
  
  output$rZ9 <- renderText({
    if(input$rXYZ9 == T){
      return(kw9()$rangZ)
    }
  })
  
  output$R9 <- renderTable({
    if(input$R9 == T){
      R9 <- as.data.frame(matrix(c(kw9()$R[1], kw9()$R[2], kw9()$R[3]), ncol=3))
      colnames(R9) <- c("R1.", "R2.", "R3.")
      rownames(R9) <- c("Valores")
      return(R9)
    }
  }, digits = 1)
  
  output$H9 <- renderTable({
    if(input$H9 == T){
      H9 <- as.data.frame(matrix(c(kw9()$H), ncol=1))
      colnames(H9) <- c("H obs")
      rownames(H9) <- c("Valores")
      return(H9)
    }
  }, digits = 3)
  
  output$datos9_1 <- renderText({
    return(datos9()$X)
  })
  
  output$datos9_2 <- renderText({
    return(datos9()$Y)
  })
  
  output$datos9_3 <- renderText({
    return(datos9()$Z)
  })
  
  output$link9 <- renderUI({
    url9 <- a("Prueba de Kruskal-Wallis", href="https://en.wikipedia.org/wiki/Kruskal-Wallis_one-way_analysis_of_variance")
    tagList("+ Info: ", url9)
  })
  
  output$graf9 <- renderPlot({
    if(input$graf9 == T){
      X <- datos9()$X
      Y <- datos9()$Y
      Z <- datos9()$Z
      boxplot(list("X1"=X,"X2"=Y,"X3"=Z), col = c("lightblue", "lightgreen", "orange"), main="Box-Plot de X1, X2 y X3")
    } else{
      NULL
    }
  })
  
  ##### Aprende: k relacionadas #####
  
  datos10 <- reactive({
    input$generar_datos10
    isolate({
      if(input$variable10 == "C"){
        return(list(X=round(generar_numeros(input$cantidad_datos10)*17+8,1), #U(8,25)
                    Y=round(generar_numeros(input$cantidad_datos10)*15+6,1), #U(6,21)
                    Z=round(generar_numeros(input$cantidad_datos10)*16+11,1))) #U(11,27)
      } else {
        return(list(X=round(generar_numeros(input$cantidad_datos10)*3+1,0), # 1,2,3,4
                    Y=round(generar_numeros(input$cantidad_datos10)*6+1,0), # 1,2,3,4,5,6,7
                    Z=round(generar_numeros(input$cantidad_datos10)*4+1,0))) # 1,2,3,4,5
      } 
    })
  })
  
  fr10 <- reactive({
    X <- datos10()$X
    Y <- datos10()$Y
    Z <- datos10()$Z
    n <- length(X)
    N <- 3*n
    m <- rbind(X,Y,Z)
    rangos <- apply(m,2,"rank") #rangos
    colnames(rangos) <- paste0("I",1:n) #I1 I2, ...
    Ri <- apply(rangos,1,"sum")
    S <- 1/(n)*sum(Ri*Ri)-12*n   
    return(list(N=N,n=n,rangos=rangos,Ri=Ri,S=S))
  })
  
  output$n10 <- renderTable({
    if(input$n10 == T){
      n10 <- as.data.frame(matrix(c(fr10()$n, fr10()$N), ncol=2))
      colnames(n10) <- c("n", "N")
      rownames(n10) <- c("Valores")
      return(n10)
    }
  }, digits = 0)
  
  output$r10 <- renderTable({
    if(input$r10 == T){
      return(fr10()$rangos)
    }
  }, rownames = T)
  
  output$Ri10 <- renderTable({
    if(input$Ri10 == T){
      Ri10 <- as.data.frame(matrix(c(fr10()$Ri[[1]], fr10()$Ri[[2]], fr10()$Ri[[3]]), ncol=3))
      colnames(Ri10) <- c("R1·", "R2.", "R3.")
      rownames(Ri10) <- c("Valores")
      return(Ri10)
    }
  }, digits = 1)
  
  output$S10 <- renderTable({
    if(input$S10 == T){
      S10 <- as.data.frame(matrix(c(fr10()$S), ncol=1))
      colnames(S10) <- c("S obs")
      rownames(S10) <- c("Valores")
      return(S10)
    }
  }, digits = 3)
  
  output$datos10 <- renderTable({
    m <- rbind(datos10()$X,datos10()$Y,datos10()$Z)
    rownames(m) <- c("X","Y","Z")
    colnames(m) <- paste0("I",1:length(datos10()$X))
    return(m)
  }, rownames = T, digits = 1)
  
  output$link10 <- renderUI({
    url10 <- a("Prueba de Friedman", href="https://en.wikipedia.org/wiki/Friedman_test")
    tagList("+ Info: ", url10)
  })
  
  output$graf10 <- renderPlot({
    if(input$graf10 == T){
      X <- datos10()$X
      Y <- datos10()$Y
      Z <- datos10()$Z
      boxplot(list("X"=X,"Y"=Y, "Z"=Z), col = c("lightblue", "lightgreen", "orange"), main="Box-Plots de X, Y y Z")
    } else{
      NULL
    }
  })
  
  ##### Aprende: Kendall #####
  
  
  datos7 <- reactive({
    input$generar_datos7
    isolate({
      u <- runif(1)
      if(u<0.5){ #aleatorias
        if(input$variable7 == "C"){
          return(list(X=round(generar_numeros(input$cantidad_datos7)*15+5,1), #U(5,20)
                      Y=round(generar_numeros(input$cantidad_datos7)*4+3,1))) #U(3,7)
        } else if(input$variable7 == "O"){
          return(list(X=round(generar_numeros(input$cantidad_datos7)*3+1,0), #1,2,3,4
                      Y=round(generar_numeros(input$cantidad_datos7)*6+1,0))) #1,2,3,4,5,6,7
        } else{
          return(list(X=round(generar_numeros(input$cantidad_datos7)*15+5,1), #U(5,20)
                      Y=round(generar_numeros(input$cantidad_datos7)*3+1,0))) #1,2,3,4 
        }
      } else{ #se fuerza la correlacion
        if(input$variable7 == "C"){
          X <- round(generar_numeros(input$cantidad_datos7)*11+1,1) #U(1,12)
          return(list(X=X, 
                      Y=round((0.2*X*X)+runif(length(X),-3,3),1))) #0.2*x^2+e
        } else if(input$variable7 == "O"){
          X <- round(generar_numeros(input$cantidad_datos7)*3+1,0)
          return(list(X=X,
                      Y=X+1+round(runif(length(X),-1,1),0))) #
        } else{
          X <- round(generar_numeros(input$cantidad_datos7)*15+5,1) #U(5,20)
          return(list(X=X,
                      Y=round(X/5+1+runif(length(X),-1,1),0))) 
        }
      }
      
    })
  })
  
  kendall <- reactive({
    x <- datos7()$X
    y <- datos7()$Y
    n <- length(x)
    #matriz logica
    i <- matrix(rep(1:(n-1), times = n-1), nrow = (n-1))
    j <- matrix(rep(2:n, times = n-1), nrow = (n-1), byrow = T)
    mat_log <- (i-j)<0
    #xi,xj,yi,yj
    xi <- matrix(rep(x[1:(n-1)], times = n-1), nrow = (n-1))
    xj <- matrix(rep(x[2:(n)], times = n-1), nrow = (n-1), byrow = T)
    yi <- matrix(rep(y[1:(n-1)], times = n-1), nrow = (n-1))
    yj <- matrix(rep(y[2:(n)], times = n-1), nrow = (n-1), byrow = T)
    #matriz final
    mat_f <- mat_log*((xj-xi)*(yj-yi))
    mat_f[mat_f>0] <- 1
    mat_f[mat_f<0] <- -1
    row.names(mat_f) <- 1:(n-1)
    colnames(mat_f) <- 2:(n)
    mases <- length(mat_f[mat_f>0])
    menos <- length(mat_f[mat_f<0])
    k <- mases - menos
    tao <- (2*k)/(n*(n-1))
    varz <- 2*(2*n+5)/(9*n*(n-1))
    z <- tao/sqrt(varz)
    return(list(mat_f=mat_f, n=n, mases=mases, menos=menos, k=k, tao=tao, varz=varz, z=z))
  })
  
  output$tabla7 <- renderTable({
    if(input$tabla7 == T){
      return(kendall()$mat_f)
    }
  }, rownames = T)
  
  output$conc_disc <- renderTable({
    if(input$conc_disc == T){
      conc_disc <- as.data.frame(matrix(c(kendall()$mases, kendall()$menos), ncol=2))
      colnames(conc_disc) <- c("Concordantes", "Discordantes")
      rownames(conc_disc) <- c("Valores")
      return(conc_disc)
    }
  }, digits = 1)
  
  output$k7 <- renderTable({
    if(input$k7 == T){
      k7 <- as.data.frame(matrix(c(kendall()$k), ncol=1))
      colnames(k7) <- c("k")
      rownames(k7) <- c("Valores")
      return(k7)
    }
  }, digits = 1)
  
  output$tao7 <- renderTable({
    if(input$tao7 == T){
      tao7 <- as.data.frame(matrix(c(kendall()$tao), ncol=1))
      colnames(tao7) <- c("Tau obs")
      rownames(tao7) <- c("Valores")
      return(tao7)
    }
  }, digits = 4)
  
  output$Tao_normal <- renderTable({
    if(input$Tao_normal == T){
      Tao_normal <- as.data.frame(matrix(c(0, kendall()$varz), ncol=2))
      colnames(Tao_normal) <- c("E[Tau]", "Var[Tau]")
      rownames(Tao_normal) <- c("Valores")
      return(Tao_normal)
    }
  }, digits = 4)
  
  output$Z_obs7 <- renderTable({
    if(input$Z_obs7 == T){
      Z_obs7 <- as.data.frame(c(kendall()$z))
      colnames(Z_obs7) <- c("Zobs")
      rownames(Z_obs7) <- c("Valor")
      return(Z_obs7)
    }
  }, digits = 4)
  
  output$datos7_1 <- renderText({
    return(datos7()$X)
  })
  
  output$datos7_2 <- renderText({
    return(datos7()$Y)
  })
  
  output$link7 <- renderUI({
    url7 <- a("Coeficiente de Kendall", href="https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient")
    tagList("+ Info: ", url7)
  })
  
  output$graf7 <- renderPlot({
    if(input$graf7 == T){
      X <- datos7()$X
      Y <- datos7()$Y
      plot(X,Y,xlab = "X", ylab = "Y", main = "Gráfico de Dispersión de X e Y")
    } else{
      NULL
    }
  })
  
  ##### Aprende: Spearman #####
  
  datos8 <- reactive({
    input$generar_datos8
    isolate({
      u <- runif(1)
      if(u <= 0.5){ #aleatorias
        if(input$variable8 == "C"){
          return(list(X=round(generar_numeros(input$cantidad_datos8)*15+5,1), #U(5,20)
                      Y=round(generar_numeros(input$cantidad_datos8)*4+3,1))) #U(3,7)
        } else if(input$variable8 == "O"){
          return(list(X=round(generar_numeros(input$cantidad_datos8)*3+1,0), #1,2,3,4
                      Y=round(generar_numeros(input$cantidad_datos8)*6+1,0))) #1,2,3,4,5,6,7
        } else{
          return(list(X=round(generar_numeros(input$cantidad_datos8)*15+5,1), #U(5,20)
                      Y=round(generar_numeros(input$cantidad_datos8)*6+1,0))) #1,2,3,4,5,6,7
        }
      } else { #se fuerza corrrelacion
        if(input$variable8 == "C"){
          X <- round(generar_numeros(input$cantidad_datos8)*15+5,1) #U(5,20)
          return(list(X=X, 
                      Y=round((-14*cos(X)+3*X)+runif(length(X),-5,5),1))) #-14*cos(x)+3x+e
        } else if(input$variable8 == "O"){
          X <- round(generar_numeros(input$cantidad_datos8)*3+1,0) #1,2,3,4
          return(list(X=X,
                      Y=X+1+round(runif(length(X),-1,1),0))) #x+1+e
        } else{
          X <- round(generar_numeros(input$cantidad_datos8)*15+5,1) #U(5,20)
          return(list(X=X,
                      Y=round(X/5+1+runif(length(X),-1,1),0))) #x/5 + 1 + e
        }
      }
    })
  })
  
  spearman <- reactive({
    x <- datos8()$X
    y <- datos8()$Y
    n <- length(x)
    r1 <- rank(x)
    r2 <- rank(y)
    r_s <- 1-6/(n*n*n-n)*sum((r1-r2)*(r1-r2))
    Tobs <- r_s*sqrt((n-2)/(1-r_s*r_s))
    return(list(n=n, r1=r1, r2=r2, r_s=r_s, Tobs=Tobs, glt=(n-2)))
  })
  
  output$rangos8_1 <- renderText({
    if(input$rangos8 == T){
      return(spearman()$r1)
    }
  })
  
  output$rangos8_2 <- renderText({
    if(input$rangos8 == T){
      return(spearman()$r2)
    }
  })
  
  output$r_s8 <- renderTable({
    if(input$r_s8 == T){
      r_s8 <- as.data.frame(matrix(c(spearman()$r_s), ncol=1))
      colnames(r_s8) <- c("r obs")
      rownames(r_s8) <- c("Valores")
      return(r_s8)
    }
  }, digits = 4)
  
  output$tobs8 <- renderTable({
    if(input$tobs8 == T){
      tobs8 <- as.data.frame(matrix(c(spearman()$Tobs, spearman()$glt), ncol=2))
      colnames(tobs8) <- c("t_obs", "g.l.")
      rownames(tobs8) <- c("Valores")
      return(tobs8)
    }
  }, digits = 4)
  
  output$cuantilt8 <- renderUI({
    input$generar_datos8
    isolate({
      withMathJax(paste0("\\( t_{n-2;1-\\frac{\\alpha}{2}} = \\) ", round(qt(df = input$cantidad_datos8-2,0.975), 4)))
    })
  })
  
  output$datos8_1 <- renderText({
    return(datos8()$X)
  })
  
  output$datos8_2 <- renderText({
    return(datos8()$Y)
  })
  
  output$link8 <- renderUI({
    url8 <- a("Coeficiente de Spearman", href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient")
    tagList("+ Info: ", url8)
  })
  
  output$graf8 <- renderPlot({
    if(input$graf8 == T){
      X <- datos8()$X
      Y <- datos8()$Y
      plot(X,Y,xlab = "X", ylab = "Y", main = "Gráfico de Dispersión de X e Y")
    } else{
      NULL
    }
  })
  
  ##### DATOS #####
  
  data <- reactive({
    archivo <- input$file
    if(is.null(archivo) | input$dataset == "empleados"){
      empleados
    } else if(input$tipo_archivo == "csv"){
      tryCatch(
        expr = {
          datos <- read.csv(archivo$datapath, header = input$header, row.names = NULL)
          log <- as.vector(unlist(lapply(datos, "is.character")))
          datos[log] <- lapply(datos[log], "as.factor")
          datos
        },
        error = function(error){
          "Se produjo un error al importar los datos. Compruebe que los parámetros son correctos."
        },
        warning = function(adv){
          "Se produjo un 'warning' al importar los datos."
        }
      )
    } else if(input$tipo_archivo == "txt"){
      tryCatch(
        expr = {
          datos <- read.table(archivo$datapath, header = input$header, sep = input$sep, dec = input$dec, row.names = NULL)
          log <- as.vector(unlist(lapply(datos, "is.character")))
          datos[log] <- lapply(datos[log], "as.factor")
          datos
        },
        error = function(error){
          "Se produjo un error al importar los datos. Compruebe que los parámetros son correctos."
        },
        warning = function(adv){
          "Se produjo un 'warning' al importar los datos."
        }
      )
    } else if(input$tipo_archivo == "excel"){
      tryCatch(
        expr = {
          datos <- as.data.frame(read_excel(archivo$datapath, col_names = input$header))
          log <- as.vector(unlist(lapply(datos, "is.character")))
          datos[log] <- lapply(datos[log], "as.factor")
          datos
        },
        error = function(error){
          "Se produjo un error al importar los datos. Compruebe que los parámetros son correctos."
        },
        warning = function(adv){
          "Se produjo un 'warning' al importar los datos."
        }
      )
    } else if(input$tipo_archivo == "spss"){
      tryCatch(
        expr = {
          datos <- read.spss(archivo$datapath, use.value.labels = T, to.data.frame = T)
          log <- as.vector(unlist(lapply(datos, "is.character")))
          datos[log] <- lapply(datos[log], "as.factor")
          datos
        },
        error = function(error){
          "Se produjo un error al importar los datos. Compruebe que los parámetros son correctos."
        },
        warning = function(adv){
          "Se produjo un 'warning' al importar los datos."
        }
      )
    }
  })
  
  output$h <- renderDataTable({
    if(is.data.frame(data()) == T){
      data()
    } else{
      NULL
    }
  }, options = list(pageLength = 10, 
                    lengthMenu = c(10,15,20,50),
                    autoWidth = T)) 
  
  output$errorDatos <- renderText({
    if(is.data.frame(data()) == F){
      data()
    } else{
      NULL
    }
  })

  output$dim <- renderText({
    if(is.null(data()) == T | is.data.frame(data()) == F){
      return("")
    } else{
      paste0("Las dimensiones del archivo importado son: ", dim(data())[1], " x ", dim(data())[2])
    }
  })
  
  ##### ANALISIS: Aleatoriedad #####
  
  observe({
    updateSelectInput(session, "var1",
                      label = "Variable:",
                      choices = names(data()),
                      selected = names(data())[1])
  })
  
  output$elegida1 <- renderText({
    if(is.null(contraste_1()) == T){
      "Ha elegido la variable:"
    } else{
      paste0("Ha elegido la variable: ", contraste_1()$var)
    }
  })
  
  contraste_1 <- reactive({
    a <- input$calcular1
    isolate({
      datos <- data()[,input$var1]
      if(is.numeric(datos) == T & a>0){
        if(input$corte1 == "median"){
          if(input$exact1 == T){
            r <- runs.test(datos,"t",threshold = median(datos, na.rm = T),"exact")
            list(r=r,datos=datos, umbral=median(datos, na.rm = T), var=input$var1, corte=input$corte1, exacto=input$exact1)
          } else{
            r <- runs.test(datos,"t",threshold = median(datos, na.rm = T))
            list(r=r,datos=datos,umbral=median(datos, na.rm = T), var=input$var1, corte=input$corte1, exacto=input$exact1)
          }
        } else if(input$corte1 == "mean"){
          if(input$exact1 == T){
            r <- runs.test(datos,"t",threshold = mean(datos, na.rm = T),"exact")
            list(r=r,datos=datos,umbral=mean(datos, na.rm = T), var=input$var1, corte=input$corte1, exacto=input$exact1)
          } else{
            r <- runs.test(datos,"t",threshold = mean(datos, na.rm = T))
            list(r=r,datos=datos,umbral=mean(datos, na.rm = T), var=input$var1, corte=input$corte1, exacto=input$exact1)
          }
        } else if(input$corte1 == "v"){
          if(input$exact1 == T){
            r <- runs.test(datos,"t",threshold = input$value1,"exact")
            list(r=r,datos=datos,umbral=input$value1, var=input$var1, corte=input$corte1, exacto=input$exact1, valor=input$value1)
          } else{
            r <- runs.test(datos,"t",threshold = input$value1)
            list(r=r,datos=datos,umbral=input$value1, var=input$var1, corte=input$corte1, exacto=input$exact1, valor=input$value1)
          }
        }
      } else if(is.numeric(datos) == F & a>0){
        if(input$exact1 == T){
          r <- runs.test(rachas_discreta(datos),"t",threshold = 0.5,"exact")
          list(r=r,datos=datos,umbral=0.5, var=input$var1, corte=input$corte1, exacto=input$exact1)
        } else{
          r <- runs.test(rachas_discreta(datos),"t",threshold = 0.5)
          list(r=r,datos=datos,umbral=0.5, var=input$var1, corte=input$corte1, exacto=input$exact1)
        }
      }
    })
  })
  
  output$corte1 <- renderText({
    if(is.null(contraste_1()) == T){
      "Ha elegido el tipo de umbral:"
    }else{
      if(is.numeric(contraste_1()$datos) == T){
        switch(contraste_1()$corte,
               median = "Ha elegido el tipo de umbral: Mediana",
               mean = "Ha elegido el tipo de umbral: Media",
               v = "Ha elegido el tipo de umbral: Valor")
      }else{
        "Ha elegido el tipo de umbral: NA"
      }
    }
  })
  
  output$value1 <- renderText({
    if(is.null(contraste_1()) == T){
      "Valor del umbral:"
    } else{
      if(is.numeric(contraste_1()$datos) == T){
        switch(contraste_1()$corte,
               v = paste0("Valor del umbral: ", contraste_1()$valor),
               median = paste0("Valor del umbral: ", round(median(contraste_1()$datos, na.rm = T), 4)),
               mean = paste0("Valor del umbral: ", round(mean(contraste_1()$datos, na.rm = T), 4)))
      } else{
        "Valor del umbral: NA"
      }
    }    
  })
  
  output$exact1 <- renderText({
    if(is.null(contraste_1()) == T){
      "¿Estadístico y p-valor exacto?: "
    } else{
      paste0("¿Estadístico y p-valor exacto?: ", contraste_1()$exacto)
    }
  })
  
  output$runs1 <- renderUI({
    if(is.null(contraste_1()) == T){
      return(withMathJax("Número de rachas \\(R_{obs}\\):"))
    } else{
      r <- contraste_1()$r
      withMathJax(paste0("Número de rachas \\(R_{obs}\\): ", r$runs)) 
    }
  })
  
  output$estadistico1 <- renderUI({
    if(is.null(contraste_1()) == T){
      return("Valor del estadístico normalizado \\(Z_{obs}\\): ")
    } else{
      r <- contraste_1()$r
      paste0("Valor del estadístico normalizado \\(Z_{obs}\\): ", round(r$statistic,3))
    }
  })
  
  output$p_valor1 <- renderText({
    if(is.null(contraste_1()) == T){
      return("p-valor:")
    } else{
      r <- contraste_1()$r
      return(paste0("p-valor: ", round(r$p.value, 6)))
    }
  })
  
  output$grafico1 <- renderPlot({
    if(is.null(contraste_1()) == T){
      NULL
    } else{
      datos <- contraste_1()$datos
      if(is.numeric(datos) == T){
        plot(datos, xlab = "Orden", ylab = contraste_1()$var)
        abline(contraste_1()$umbral,0, col="red")
      } else{
        plot(rachas_discreta(datos), xlab = "Orden", ylab = contraste_1()$var)
      }
    }
  })
  
  ##### ANALISIS: B. Ajuste (F=Fo) #####
  
  observe({
    updateSelectInput(session, "var2",
                      label = "Variable:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  output$elegida2 <- renderText({
    paste0("Ha elegido la variable: ", contraste_2()$var)
  })
  
  contraste_2 <- reactive({
    a <- input$calcular2
    isolate({
      datos <- data()[,input$var2]
      if(is.numeric(datos) == T & a>0){
        if(input$con2 == "pnorm"){
          k <- ks.test(datos, input$con2, mean = input$par2_1, sd = input$par2_2, exact = input$exact2)
          return(list(k=k, datos=datos, var=input$var2, exact=input$exact2,conocida=input$con2, param1=input$par2_1, sd=input$par2_2, 
                      tabla=input$summ2, media=mean(datos, na.rm = T), mediana=median(datos, na.rm = T), desv=sd(datos, na.rm = T)))
        } else {
          k <- ks.test(datos, input$con2, input$par2_1, exact = input$exact2)
          return(list(k=k, datos=datos, var=input$var2, exact=input$exact2, conocida=input$con2, param1=input$par2_1, 
                      tabla=input$summ2, media=mean(datos, na.rm = T), mediana=median(datos, na.rm = T), desv=sd(datos, na.rm = T)))
        }
      }
    })
  })
  
  output$distribucion2 <- renderText({
    if(is.null(contraste_2()) == T){
      return("La distribución elegida es:")
    } else{
      con <- contraste_2()$conocida
      switch(con,
             pnorm = "La distribución elegida es: Normal",
             pexp = "La distribución elegida es: Exponencial",
             pchisq = "La distribución elegida es: Chi-Cuadrado",
             pt = "La distribución elegida es: t Student")
    }
  })
  
  output$exact2 <- renderText({
    paste0("¿Estadístico y p-valor exacto?: ", contraste_2()$exact)
  })
  
  output$param_eleg_1 <- renderText({
    paste0("Valor del primer parámetro: ", contraste_2()$param1)
  })
  
  output$param_eleg_2 <- renderText({
    if(contraste_2()$conocida == "pnorm"){
      paste0("Valor del segundo parámetro: ", contraste_2()$sd)
    } else{
      "Valor del segundo parámetro: NA"
    }
  })
  
  output$summ2 <- renderText({
    if(is.null(contraste_2()) == T){
      "¿Calcular Media, Mediana y Desviación Típica?: "
    } else{
      paste0("¿Calcular Media, Mediana y Desviación Típica?: ", contraste_2()$tabla)
    }
  })
  
  output$estadistico2 <- renderUI({
    if(is.null(contraste_2()) == T){
      return(withMathJax("Valor del estadístico \\(D_{obs}\\): "))
    } else{
      k <- contraste_2()$k
      withMathJax(paste0("Valor del estadístico \\(D_{obs}\\): ", round(k$statistic,3))) 
    }
  })
  
  output$p_valor2 <- renderText({
    if(is.null(contraste_2()) == T){
      return("p-valor:")
    } else{
      k <- contraste_2()$k
      return(paste0("p-valor: ", round(k$p.value, 6)))
    }
  })
  
  output$summary2 <- renderTable({
    if(is.null(contraste_2()) == T){
      return(NULL)
    } else if(contraste_2()$tabla == T){
      summ <- as.data.frame(matrix(c(contraste_2()$media, contraste_2()$mediana, contraste_2()$desv), ncol=3))
      colnames(summ) <- c("Media", "Mediana", "Desviación")
      rownames(summ) <- c("Valores")
      return(summ)
    }
  }, digits = 4)
  
  output$grafico2 <- renderPlot({
    if(is.null(contraste_2()) == T){
      return(NULL)
    }else{
      X <- contraste_2()$datos
      conocida <- contraste_2()$conocida
      if(conocida == "pnorm"){
        qqPlot(X, distribution = "norm", mean=contraste_2()$param1, sd=contraste_2()$sd, id=F, line="none", envelope = F,
                xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestra", main = "Gráfico Q-Q Normal")
        abline(0,1,col="blue", lwd=2)
      } else if(conocida == "pexp"){
        qqPlot(X, distribution = "exp", rate=contraste_2()$param1, id=F, line="none", envelope = F,
                xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestra", main = "Gráfico Q-Q Exponencial")
        abline(0,1,col="blue", lwd=2)
      } else if(conocida == "pchisq"){
        qqPlot(X, distribution = "chisq", df=contraste_2()$param1, id=F, line="none", envelope = F,
                xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestra", main = "Gráfico Q-Q Chi-2")
        abline(0,1,col="blue", lwd=2)
      } else if(conocida == "pt"){
        qqPlot(X, distribution = "t", df=contraste_2()$param1, id=F, line="none", envelope = F,
                xlab = "Cuantiles Teóricos", ylab = "Cuantiles Muestra", main = "Gráfico Q-Q t-Student")
        abline(0,1,col="blue", lwd=2)
      }
    }
  })
  
  output$grafico2_2 <- renderPlot({
    if(is.null(contraste_2()) == T){
      return(NULL)
    }else{
      X <- contraste_2()$datos
      conocida <- contraste_2()$conocida
      if(conocida == "pnorm"){
        hist(X, freq = F,col="lightblue", main = "Histograma + Densidad", xlab = contraste_2()$var)
        curve(dnorm(x,contraste_2()$param1,contraste_2()$sd), col="red", lwd=2, add = T)
      } else if(conocida == "pexp"){
        hist(X, freq = F, col="lightblue", main = "Histograma + Densidad", xlab = contraste_2()$var)
        curve(dexp(x,contraste_2()$param1), col="red", lwd=2, add = T)
      } else if(conocida == "pchisq"){
        hist(X, freq = F, col="lightblue", main = "Histograma + Densidad", xlab = contraste_2()$var)
        curve(dchisq(x,contraste_2()$param1), col="red", lwd=2, add = T)
      } else{
        hist(X, freq = F, col="lightblue", main = "Histograma + Densidad", xlab = contraste_2()$var)
        curve(dt(x,contraste_2()$param1), col="red", lwd=2, add = T)
      }
    }
  })
  
  ##### ANALISIS: B. Ajuste (F=G) #####
  
  observe({
    updateSelectInput(session, "var3_1",
                      label = "Variable Agrupación:",
                      choices = names(data())[as.vector(unlist(lapply(sapply(data(), "levels"), "length"))==2)],
                      selected = names(data())[as.vector(unlist(lapply(sapply(data(), "levels"), "length"))==2)][1])
  })
  
  observe({
    updateSelectInput(session, "var3_2",
                      label = "Variable X:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][2])
  })
  
  output$elegida3 <- renderText({
    paste0("Ha elegido la variable de agrupación: ", contraste_3()$var1, ". Variable X: ", contraste_3()$var2)
  })
  
  contraste_3 <- reactive({
    a <- input$calcular3
    isolate({
      lvl <- levels(data()[,input$var3_1]) 
      x <- data()[,input$var3_1]
      y1 <- data()[x==lvl[1],input$var3_2]
      y2 <- data()[x==lvl[2],input$var3_2]
      if(is.numeric(y1) == T & is.numeric(y2) == T & a>0){
        k <- ks.test(y1, y2, exact = input$exact3)
        return(list(k=k, lvl=lvl, y1=y1, y2=y2, var1=input$var3_1, var2=input$var3_2, exact=input$exact3))
      }
    })
  })
  
  output$exact3 <- renderText({
    paste0("¿Estadístico y p-valor exacto?: ", contraste_3()$exact)
  })
  
  output$estadistico3 <- renderUI({
    if(is.null(contraste_3()) == T){
      return(withMathJax("Valor del estadístico \\(D_{obs}\\): "))
    } else{
      k <- contraste_3()$k
      withMathJax(paste0("Valor del estadístico \\(D_{obs}\\): ", round(k$statistic,2))) 
    }
  })
  
  output$p_valor3 <- renderText({
    if(is.null(contraste_3()) == T){
      return("p-valor:")
    } else{
      k <- contraste_3()$k
      return(paste0("p-valor: ", round(k$p.value, 6)))
    }
  })
  
  output$grafico3 <- renderPlot({
    if(is.null(contraste_3()) == T){
      return(NULL)
    }else{
      lvl <-  contraste_3()$lvl
      y1 <- contraste_3()$y1
      y2 <- contraste_3()$y2
      qqplot(y1,y2, xlab = paste0("Cuantiles ", lvl[1]), ylab = paste0("Cuantiles ", lvl[2]), main = "Gráfico Q-Q")
      abline(0,1, col="red", lwd=2)
    }  
  })
    
  ##### ANALISIS: Loc. Muestra (Me) #####
  
  observe({
    updateSelectInput(session, "var4",
                      label = "Variable:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  output$elegida4 <- renderText({
    paste0("Ha elegido la variable: ", contraste_4()$var)
  })
  
  output$contraste_eleg4 <- renderText({
    if(is.null(contraste_4()) == T){
      return("El tipo de contraste elegido es:")
    } else{
      alt <- contraste_4()$alternative
      switch(alt,
             t = "El tipo de contraste elegido es: Bilateral (H0: =)",
             g = "El tipo de contraste elegido es: Unilateral Superior (H1: >)",
             l = "El tipo de contraste elegido es: Unilateral Inferior (H1: <)")
    }
  })
  
  output$mu_eleg4 <- renderUI({
    withMathJax(paste0("El valor elegido de la mediana en las hipótesis \\(\\theta_{0}\\) es: ", contraste_4()$mu))
  })
  
  output$summ4 <- renderText({
    if(is.null(contraste_4()) == T){
      "¿Calcular Media, Mediana y Desviación Típica?: "
    } else{
      paste0("¿Calcular Media, Mediana y Desviación Típica?: ", contraste_4()$tabla)
    }
  })
  
  contraste_4 <- reactive({
    a <- input$calcular4
    isolate({
      datos <- data()[,input$var4]
      if(is.numeric(datos) == T & a>0){
        W <- wilcox.test(datos, alternative = input$contraste4, mu = input$mu4)
        return(list(W=W, datos=datos, var=input$var4, alternative=input$contraste4, mu=input$mu4, tabla=input$summ4,
                    media=mean(datos, na.rm = T), mediana=median(datos, na.rm = T), desv=sd(datos, na.rm = T)))
      }
    })
  })
  
  output$estadistico4 <- renderUI({
    if(is.null(contraste_4()) == T){
      return(withMathJax("Valor del estadístico \\(T^{+}_{obs}\\): "))
    } else{
      W <- contraste_4()$W
      withMathJax(paste0("Valor del estadístico \\(T^{+}_{obs}\\): ", round(W$statistic,2)))
    }
  })
  
  output$p_valor4 <- renderText({
    if(is.null(contraste_4()) == T){
      return("p-valor:")
    } else{
      W <- contraste_4()$W
      return(paste0("p-valor: ", round(W$p.value, 6)))
    }
  })
  
  output$summary4 <- renderTable({
    if(is.null(contraste_4()) == T){
      return(NULL)
    } else if(contraste_4()$tabla == T){
      summ <- as.data.frame(matrix(c(contraste_4()$media, contraste_4()$mediana, contraste_4()$desv), ncol=3))
      colnames(summ) <- c("Media", "Mediana", "Desviación")
      rownames(summ) <- c("Valores")
      return(summ)
    }
  }, digits = 4)
  
  output$grafico4 <- renderPlot({
    if(is.null(contraste_4()) == T){
      return(NULL)
    }else{
      boxplot(contraste_4()$datos, col = "lightblue", main = paste0("Box-Plot de ", contraste_4()$var))
      abline(h=contraste_4()$mu, col="red")
    }
  })
  
  ##### ANALISIS: 2 muestras indep #####
  
  observe({
    updateSelectInput(session, "var5_1",
                      label = "Variable Agrupación:",
                      choices = names(data())[as.vector(unlist(lapply(sapply(data(), "levels"), "length"))==2)],
                      selected = names(data())[as.vector(unlist(lapply(sapply(data(), "levels"), "length"))==2)][1])
  })
  
  observe({
    updateSelectInput(session, "var5_2",
                      label = "Variable X:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  output$elegida5 <- renderText({
    paste0("Ha elegido como variable de agrupación: ", contraste_5()$var1, ", y  como variable X: ", contraste_5()$var2)
  })
  
  contraste_5 <- reactive({
    a <- input$calcular5
    isolate({
      x <- data()[,input$var5_1]
      y <- data()[,input$var5_2]
      if(is.numeric(y) == T & is.factor(x) == T & a>0){
        u <- wilcox.test(y~x, alternative = input$contraste5, exact = input$exact5, correct = input$correct5, paired = F)
        return(list(u=u, x=x, y=y, var1=input$var5_1, var2=input$var5_2, alt=input$contraste5, exact=input$exact5, correct=input$correct5))
      }
    })
  })
  
  output$contraste_eleg5 <- renderText({
    if(is.null(contraste_5()) == T){
      return("El tipo de contraste elegido es:")
    } else{
      alt <- contraste_5()$alt
      switch(alt,
             t = "El tipo de contraste elegido es: Bilateral (H0: X=Y)",
             l = "El tipo de contraste elegido es: Unilateral Superior (H1: X<Y)",
             g = "El tipo de contraste elegido es: Unilateral Inferior (H1: X>Y)")
    }
  })
  
  output$exact5 <- renderText({
    paste0("¿Estadístico y p-valor exacto?: ", contraste_5()$exact)
  })
  
  output$correct5 <- renderText({
    paste0("¿Corrección por continuidad?: ", contraste_5()$correct)
  })
  
  output$estadistico5 <- renderUI({
    if(is.null(contraste_5()) == T){
      return(withMathJax("Valor del estadístico \\(U_{obs}\\): "))
    } else{
      u <- contraste_5()$u
      withMathJax(paste0("Valor del estadístico \\(U_{obs}\\): ", round(u$statistic,2)))
    }
  })
  
  output$p_valor5 <- renderText({
    if(is.null(contraste_5()) == T){
      return("p-valor:")
    } else{
      u <- contraste_5()$u
      return(paste0("p-valor: ", round(u$p.value, 6)))
    }
  })
  
  output$grafico5 <- renderPlot({
    if(is.null(contraste_5()) == T){
      return(NULL)
    }else{
      boxplot(contraste_5()$y~contraste_5()$x, col = c("lightgreen", "lightblue"), 
              ylab = "Valores", main = "Box-Plots", xlab = "")
    }
  })
  
  ##### ANALISIS: 2 muestras relacionadas #####
  
  observe({
    updateSelectInput(session, "var6_1",
                      label = "Variable X:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  observe({
    updateSelectInput(session, "var6_2",
                      label = "Variable Y:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var6_1],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var6_1][1])
  })
  
  output$elegida6 <- renderText({
    paste0("Ha elegido las variables: ", contraste_6()$var1, " y ", contraste_6()$var2)
  })
  
  contraste_6 <- reactive({
    a <- input$calcular6
    isolate({
      x <- data()[,input$var6_1]
      y <- data()[,input$var6_2]
      if(is.numeric(x) == T & is.numeric(y) == T & a>0){
        W <- wilcox.test(x, y, alternative = input$contraste6, exact = input$exact6, correct = input$correct6, paired = T)
        return(list(W=W, x=x, y=y, var1=input$var6_1, var2=input$var6_2, alt=input$contraste6, exact=input$exact6, correct=input$correct6))
      }
    })
  })
  
  output$contraste_eleg6 <- renderText({
    if(is.null(contraste_6()) == T){
      return("El tipo de contraste elegido es:")
    } else{
      alt <- contraste_6()$alt
      switch(alt,
             t = "El tipo de contraste elegido es: Bilateral (H0: X=Y)",
             l = "El tipo de contraste elegido es: Unilateral Superior (H1: X<Y)",
             g = "El tipo de contraste elegido es: Unilateral Inferior (H1: X>Y)"
      )
    }
  })
  
  output$exact6 <- renderText({
    paste0("¿Estadístico y p-valor exacto?: ", contraste_6()$exact)
  })
  
  output$correct6 <- renderText({
    paste0("¿Corrección por continuidad?: ", contraste_6()$correct)
  })
  
  output$estadistico6 <- renderUI({
    if(is.null(contraste_6()) == T){
      return(withMathJax("Valor del estadístico \\(T^{+}_{obs}\\): "))
    } else{
      W <- contraste_6()$W
      withMathJax(paste0("Valor del estadístico \\(T^{+}_{obs}\\): ", round(W$statistic,2)))
    }
  })
  
  output$p_valor6 <- renderText({
    if(is.null(contraste_6()) == T){
      return("p-valor:")
    } else{
      W <- contraste_6()$W
      return(paste0("p-valor: ", round(W$p.value, 6)))
    }
  })
  
  output$grafico6 <- renderPlot({
    if(is.null(contraste_6()) == T){
      return(NULL)
    }else{
      m <- matrix(c(contraste_6()$x,contraste_6()$y), ncol = 2)
      colnames(m) <- c(contraste_6()$var1, contraste_6()$var2)
      boxplot(m, col = c("lightgreen", "lightblue"), 
              ylab = "Valores", main = "Box-Plots")
    }
  })
  
  ##### ANALISIS: k muestras indep #####
  
  observe({
    updateSelectInput(session, "var7_1",
                      label = "Variable Agrupación:",
                      choices = names(data())[as.vector(sapply(data(), "is.factor"))],
                      selected = names(data())[as.vector(sapply(data(), "is.factor"))][1])
  })
  
  observe({
    updateSelectInput(session, "var7_2",
                      label = "Variable X:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  output$elegida7 <- renderText({
    if(is.null(contraste_7()) == T){
      return("Ha elegido las variables: ")
    } else{
      paste0("Ha elegido como variable de agrupación: ", contraste_7()$var1, ", y como variable X: ", contraste_7()$var2)
    }
  })
  
  output$posthoc7 <- renderText({
    if(is.null(contraste_7()) == T){
      return("¿Incluir Análisis post-hoc?: ")
    } else{
        paste0("¿Incluir Análisis post-hoc?: ", contraste_7()$posthoc)
    }
  })
  
  contraste_7 <- reactive({
    a <- input$calcular7
    isolate({
      nvars <- input$nvars7
      v1 <- data()[,input$var7_1]
      v2 <- data()[,input$var7_2]
      if(is.factor(v1) == T & is.numeric(v2) == T & a>0){
        k <- kruskal.test(v2~v1)
        return(list(k=k, v1=v1, v2=v2, var1=input$var7_1, var2=input$var7_2, posthoc=input$posthoc_ind7))
      }
    })
  })
  
  output$estadistico7 <- renderUI({
    if(is.null(contraste_7()) == T){
      return(withMathJax("Valor del estadístico \\(H_{obs}\\): "))
    } else{
      k <- contraste_7()$k
      withMathJax(paste0("Valor del estadístico \\(H_{obs}\\): ", round(k$statistic,2)))
    }
  })
  
  output$p_valor7 <- renderText({
    if(is.null(contraste_7()) == T){
      return("p-valor:")
    } else{
      k <- contraste_7()$k
      return(paste0("p-valor: ", round(k$p.value, 6)))
    }
  })
  
  output$grafico7 <- renderPlot({
    if(is.null(contraste_7()) == T){
      return(NULL)
    }else{
      boxplot(contraste_7()$v2~contraste_7()$v1, col = 2:7, 
              ylab = "Valores", main = "Box-Plots", xlab = "")
    }
  })
  
  output$posthoc_ind7 <- renderTable({
    if(is.null(contraste_7()) == T | contraste_7()$posthoc == F){
      return(NULL)
    }else{
      obj <- pairwise.wilcox.test(contraste_7()$v2, contraste_7()$v1, p.adjust.method = "fdr", paired = F)
      return(obj$p.value)
    }
  }, rownames = T)
  
  ##### ANALISIS: k muestras relacionadas #####
  
  observe({
    updateSelectInput(session, "var8_1",
                      label = "Variable 1:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  observe({
    updateSelectInput(session, "var8_2",
                      label = "Variable 2:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1][1])
  })
  
  observe({
    updateSelectInput(session, "var8_3",
                      label = "Variable 3:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2][1])
  })
  
  observe({
    updateSelectInput(session, "var8_4",
                      label = "Variable 4:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_3],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_3][1])
  })
  
  observe({
    updateSelectInput(session, "var8_5",
                      label = "Variable 5:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_3 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_4],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_3 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_4][1])
  })
  
  observe({
    updateSelectInput(session, "var8_6",
                      label = "Variable 6:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_3 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_4 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_5],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_1 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_2 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_3 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_4 & names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var8_5][1])
  })
  
  output$elegida8 <- renderText({
    if(is.null(contraste_8()) == T){
      return("Ha elegido las variables: ")
    } else{
      nvars <- contraste_8()$nvars
      switch((nvars-2),
             paste0("Ha elegido las variables: ", contraste_8()$var1, ", ", contraste_8()$var2," y ", contraste_8()$var3),
             paste0("Ha elegido las variables: ", contraste_8()$var1, ", ", contraste_8()$var2,", ", contraste_8()$var3, " y ", 
                    contraste_8()$var4),
             paste0("Ha elegido las variables: ", contraste_8()$var1, ", ", contraste_8()$var2,", ", contraste_8()$var3, ", ", 
                    contraste_8()$var4, " y ", contraste_8()$var5),
             paste0("Ha elegido las variables: ", contraste_8()$var1, ", ", contraste_8()$var2,", ", contraste_8()$var3, ", ", 
                    contraste_8()$var4, ", ", contraste_8()$var5, " y ", contraste_8()$var6)
      )
    }
  })
  
  output$posthoc8 <- renderText({
    if(is.null(contraste_8()) == T){
      return("¿Incluir Análisis post-hoc?: ")
    } else{
      paste0("¿Incluir Análisis post-hoc?: ", contraste_8()$posthoc)
    }
  })
  
  contraste_8 <- reactive({
    a <- input$calcular8
    isolate({
      nvars <- input$nvars8
      v1 <- data()[,input$var8_1]
      v2 <- data()[,input$var8_2]
      v3 <- data()[,input$var8_3]
      v <- c(v1,v2,v3)
      m <- matrix(v,nrow = length(v1), ncol = 3)
      if(nvars>3){
        v4 <- data()[,input$var8_4]
        v <- c(v1,v2,v3,v4)
        m <- matrix(v,nrow = length(v1), ncol = 4)
        if(nvars>4){
          v5 <- data()[,input$var8_5]
          v <- c(v1,v2,v3,v4,v5)
          m <- matrix(v,nrow = length(v1), ncol = 5)
          if(nvars>5){
            v6 <- data()[,input$var8_6]
            v <- c(v1,v2,v3,v4,v5,v6)
            m <- matrix(v,nrow = length(v1), ncol = 6)
          }
        }
      }
      if(is.numeric(v) == T & a>0){
        fr <- friedman.test(m)
        return(list(fr=fr, m=m, var1=input$var8_1, var2=input$var8_2, var3=input$var8_3,
                    var4=input$var8_4, var5=input$var8_5, var6=input$var8_6, nvars=nvars, posthoc=input$posthoc_rel8, n=length(v1)))
      }
    })
  })
  
  output$estadistico8 <- renderUI({
    if(is.null(contraste_8()) == T){
      return(withMathJax("Valor del estadístico \\(S_{obs}\\): "))
    } else{
      fr <- contraste_8()$fr
      withMathJax(paste0("Valor del estadístico \\(S_{obs}\\): ", round(fr$statistic,2)))
    }
  })
  
  output$p_valor8 <- renderText({
    if(is.null(contraste_8()) == T){
      return("p-valor:")
    } else{
      fr <- contraste_8()$fr
      return(paste0("p-valor: ", round(fr$p.value, 6)))
    }
  })
  
  output$grafico8 <- renderPlot({
    if(is.null(contraste_8()) == T){
      return(NULL)
    }else{
      m <- contraste_8()$m
      colnames(m) <- c(contraste_8()$var1,contraste_8()$var2,contraste_8()$var3,contraste_8()$var4,
                       contraste_8()$var5,contraste_8()$var6)[1:contraste_8()$nvars]
      boxplot(m, col = 2:7, 
              ylab = "Valores", main = "Box-Plots")
    }
  })
  
  output$posthoc_rel8 <- renderTable({
    if(is.null(contraste_8()) == T | contraste_8()$posthoc == F){
      return(NULL)
    }else{
      dat <- as.vector(contraste_8()$m)
      fact <- rep(c(contraste_8()$var1,contraste_8()$var2,contraste_8()$var3,contraste_8()$var4,
                    contraste_8()$var5,contraste_8()$var6)[1:contraste_8()$nvars], each=contraste_8()$n)
      obj <- pairwise.wilcox.test(dat, fact, p.adjust.method = "fdr", paired = T)
      return(obj$p.value)
    }
  }, rownames = T)
  
  ##### ANALISIS: independencia: Kendall y Spearman #####
  
  observe({
    updateSelectInput(session, "var9_1",
                      label = "Variable 1:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][1])
  })
  
  observe({
    updateSelectInput(session, "var9_2",
                      label = "Variable 2:",
                      choices = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var9_1],
                      selected = names(data())[as.vector(sapply(data(), "is.numeric"))][names(data())[as.vector(sapply(data(), "is.numeric"))]!=input$var9_1][1])
  })
  
  contraste_9 <- reactive({
    a <- input$calcular9
    isolate({
      x <- data()[,input$var9_1]
      y <- data()[,input$var9_2]
      n <- length(x)
      if(is.numeric(x) == T & is.numeric(y) == T & a>0){
        c <- cor.test(x, y, alternative = "t", method = input$metodo9, exact = input$exact9, continuity = input$continuity9)
        return(list(c=c, x=x, y=y, var1=input$var9_1, var2=input$var9_2, method=input$metodo9, exact=input$exact9, continuity=input$continuity9, n=n))
      }
    })
  })
  
  output$elegida9 <- renderText({
    paste0("Ha elegido las variables: ", contraste_9()$var1, " y ", contraste_9()$var2)
  })
  
  output$metodo9 <- renderText({
    paste0("Ha elegido el método: ", contraste_9()$method)
  })
  
  output$exact9 <- renderText({
    paste0("¿Estadístico y p-valor exacto?: ", contraste_9()$exact)
  })
  
  output$continuity9 <- renderText({
    paste0("¿Corrección por continuidad?: ", contraste_9()$continuity)
  })
  
  output$asociacion9 <- renderUI({
    if(is.null(contraste_9()) == T){
      return(withMathJax("Valor de asociación (Kendall: \\(\\tau_{obs}\\), Spearman: \\(r_{obs}\\) ): "))
    } else{
      c <- contraste_9()$c
      withMathJax(paste0("Valor de asociación (Kendall: \\(\\tau_{obs}\\), Spearman: \\(r_{obs}\\) ): ", round(c$estimate,3)))
    }
  })
  
  output$estadistico9 <- renderUI({
    if(is.null(contraste_9()) == T){
      return(withMathJax("Valor del estadístico (Kendall: \\(Z_{obs}\\), Spearman: \\(t_{obs}\\) ): "))
    } else{
      if(contraste_9()$method == "kendall"){
        c <- contraste_9()$c
        withMathJax(paste0("Valor del estadístico (Kendall: \\(Z_{obs}\\), Spearman: \\(t_{obs}\\) ): ", round(c$statistic,3)))
      } else{
        n <- contraste_9()$n
        c <- contraste_9()$c
        r <- c$estimate
        t <- r*sqrt((n-2)/(1-r*r))
        withMathJax(paste0("Valor del estadístico (Kendall: \\(Z_{obs}\\), Spearman: \\(t_{obs}\\) ): ", round(t,3)))
      }
    }
  })
  
  output$p_valor9 <- renderText({
    if(is.null(contraste_9()) == T){
      return("p-valor:")
    } else{
      c <- contraste_9()$c
      return(paste0("p-valor: ", round(c$p.value, 6)))
    }
  })
  
  output$grafico9 <- renderPlot({
    if(is.null(contraste_9()) == T){
      return(NULL)
    }else{
      plot(contraste_9()$x, contraste_9()$y, main = "Diagrama de dispersión",
            xlab = contraste_9()$var1, ylab = contraste_9()$var2)
    }
  })
  
}



##########################################################################################################
#                                                 ShinyApp                                               #
##########################################################################################################

#Juntamos ui y server
shinyApp(ui = ui, server = server)




