Roi's SQLDBsheet
================

## prerequisits
* `brew install mysql`
* `brew services start mysql`
* `brew services stop mysql`
## client
* mysql.server start / stop
* `mysql -h localhost -u root -p`
* to change password (inside mysql):  `ALTER USER 'root'@'localhost' IDENTIFIED BY 'MyNewPass';`

## databse commands
* `show databases;`
* `CREATE DATABASE database name;`
* `DROP DATABASE database name;`
* `USE database name;`
* `SHOW tables;` # shows the tables we made for the database name
* `CREATE TABLE potluck (id INT NOT NULL PRIMARY KEY AUTO_INCREMENT, name VARCHAR(20), food VARCHAR(30), confirmed CHAR(1), signup_date DATE);`
* `DESCRIBE potluck;`
* `INSERT INTO potluck (id,name,food,confirmed,signup_date) VALUES (NULL, "John", "Casserole","Y", '2012-04-11');`
* `UPDATE potluck SET confirmed='N' WHERE potluck.name='Sandy';`
* `ALTER TABLE potluck ADD email VARCHAR(40) AFTER name;`
* `ALTER TABLE potluck DROP email;` # remove a column
* `DELETE from [table name] where [column name]=[field text];` # delete row
* `SELECT * FROM potluck where id = 5 And 'confirmed' = 'Y';`
