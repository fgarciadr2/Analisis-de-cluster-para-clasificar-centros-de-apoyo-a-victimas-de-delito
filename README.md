# Análisis de¬ cluster¬ para clasificar centros de apoyo a víctimas de delito (CAVD)

(Trabajo realizado en octubre de 2019) 

Mediante análisis de clúster, clasifiqué de 3 formas a los centros de apoyo a víctimas, que dependen de la subsecretaria de prevención del delito. Para esto utilicé el algoritmo K-Medias, con los ingresos a los centros en 2018.

Debido a que los CAVD tienen distintas dimensiones por las que se pueden relacionar, las cuales no tienen un nivel muy alto de correlación entre sí, opté por hacer 3 clasificaciones, en torno a cada una de esas dimensiones. Clasificando a los centros en dimensiones distintas, obtuve mejores indicadores de calidad de los conglomerados que haciendo una sola clasificación que incluyera a todas esas variables. Estas dimensiones son las siguientes:

•	Dimensión contextual: Distingue a los CAVD según los contextos en los que desempeñan, y no dependen del trabajo de sus equipos. Incluye en sus categorías el número de víctimas que recibe, los delitos que tienden a sufrir sus víctimas atendidas, el número de delitos denunciados en las comunas donde prestan atención y la edad promedio de sus víctimas, entre otras. Los análisis demostraron que lo óptimo fue clasificarlos en 4 grupos.
•	Dimensión de gestión: Distingue a los CAVD según la gestión y administración con la que trabajan. Incluye como variables las vías por las que tienden a ingresar a las víctimas, el número de víctimas cuya intervención se mantiene vigente, la cantidad de profesionales de los que dispone para atender a víctimas (Capacidad instalada), y las vías por las que cierran los casos. Lo óptimo fue clasificarlos en 2 grupos.
•	Dimensión de intervención: Distingue a las víctimas según qué tipo de acciones tienden a llevar a cabo para la atención a las víctimas que atiende. Incluye como variables el número de atenciones psicológicas, sociales y jurídicas que prestan; el número de actividades realizadas en conjunto con otras instituciones; y el número de causas presentadas en Tribunales. Lo óptimo en esta dimensión fue clasificar a los centros en 3 grupos. 

También analicé a través de un diagrama de Sankey, cómo interactuaban los conglomerados obtenidos, concluyendo que pese a que no es eficiente hacer una sola clasificación con todas las variables independientes posibles de incorporar, sí es posible encontrar combinaciones que se repiten con frecuencia.


Solo se muestra el código y el informe de resultados, debido a la dificultad para presentar las bases de datos sin incluir información que pueda comprometer la privacidad de las víctimas atendidas en el programa y de los profesionales que se desempeñan en los CAVD.
