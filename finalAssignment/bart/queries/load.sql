LOAD DATA INFILE 'C:\\Users\\Bart\\Documents\\TU\\Master\\IN4334\\results\\filtered_data.csv' 
INTO TABLE filtered_data 
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;