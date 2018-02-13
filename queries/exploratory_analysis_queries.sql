# Anios con mayor cantidad de registro de nombres
SELECT sum(cantidad) registros, anio FROM nombres_hist.hist_nombres 
group by anio
order by registros desc
LIMIT 5;

# Anios con menor cantidad de registro de nombres desde 1978
SELECT sum(cantidad) registros, anio FROM nombres_hist.hist_nombres 
group by anio
having anio >= 1978
order by registros asc;

# Cual fueron los nombres mas registrados?
SELECT nombre, sum(cantidad) registros FROM nombres_hist.hist_nombres 
group by nombre
having registros > 100
order by registros desc;

# Cual fueron los 5 nombres mas registrados por decada - Por posible asociacion historica
# 1950 - 1960
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros 
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1922 AND 1929
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1930 AND 1939
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1940 AND 1949
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1950 AND 1959
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1960 AND 1969
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1970 AND 1979
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1980 AND 1989
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 1990 AND 1999
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 2000 AND 2009
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5)
UNION
(SELECT concat('Decada ',decada) titulo, nombre, sum(cantidad) registros
FROM nombres_hist.hist_nombres 
WHERE anio BETWEEN 2010 AND 2015
GROUP BY nombre, titulo
HAVING registros > 100
ORDER BY registros DESC
LIMIT 5);


