CREATE TABLE IF NOT EXISTS hist_nombres (
id INT NOT NULL AUTO_INCREMENT,
nombre VARCHAR(200) CHARACTER SET 'utf8' DEFAULT NULL,
cantidad SMALLINT DEFAULT NULL,
anio SMALLINT DEFAULT NULL,
PRIMARY KEY (`id`) 
);

LOAD DATA LOCAL INFILE '/Users/inesfrias/Documents/pers_gender/data/historico-nombres-clean.csv'
 INTO TABLE hist_nombres
CHARACTER SET 'utf8' 
   FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\n'
 IGNORE 1 ROWS
(nombre, cantidad, anio)
SET id = NULL
 ;

# 9761609 rows

TRUNCATE hist_nombres;