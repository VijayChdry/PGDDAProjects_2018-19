#########Assignment - Problem Statement########################
This is a compulsory assignment on SQL. You will get a document with instructions to code. You need to write the SQL queries below each 
instruction and submit the document. You will work on superstore sales data and will find some interesting insights from it. 
The data used contains information about the customers, orders placed by them, the products, shipping details etc.

 

NOTE
This assignment needs to be undertaken by setting the 'ONLY_FULL_GROUP_BY' mode enabled. For achieving the same execute the 
following query in MySQL workbench. Ignore the warning which you get after executing the query.
 
SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
Once this is done, check if the 'ONLY_FULL_GROUP_BY' mode is enabled by executing the following query:
select @@GLOBAL.sql_mode;
The output should show 'ONLY_FULL_GROUP_BY'.

 If you restart the MySQL server then the 'ONLY_FULL_GROUP_BY' mode will be disabled. It should be enabled again following all the 
 above steps.

 If 'ONLY_FULL_GROUP_BY'  is disabled then the correct output will be returned for wrong queries also. Marks will not be awarded if 
 the query is wrong and the output is correct. So it is important to ensure this step.

 

About the assignment

#########Where do I get the data from?
You can download the superstore sales data from the bottom of this page. It contains the following files:

 

File Name

Description
cust_dimen
This file gives information about the customers of the superstore

orders_dimen
File containing all the details of orders

prod_dimen
The file containing all the details about all the products available in the superstore

shipping_dimen
The file giving shipping details 

market_fact
This is the biggest file containing the market facts for all the products customers and orders

 

 
#########How should I approach the assignment solution?

 

Step 1: Creating the schema
In this step, you will create the schema for this assignment. Please follow the instructions given below in the same order:
Create a database/schema and name it as "superstoresdb"
Download the SQLAssignment(Instruction File) file available on next page. Follow the instructions given in Task 0 to create all 
the tables.


Step 2: Importing the data to the tables
MySQL allows us to import the data from csv, json files into the tables. You are required to do the same in MySQL Workbench through 
the following steps.
Using the "Navigator" pane at the left-hand side, locate your database i.e. "superstoresdb"
Expand it to get the tables list
Right-click on the table name to get the menu.
Select "Table Data Import Wizard" from this menu.
Follow the steps of the wizard and the data will be imported in respective tables
Repeat this process for all the tables in the schema.
 

Step 3: Solving the assignment
You are required to write queries given in the instruction file. The instruction file can be downloaded from the bottom of the next page.


Step 4: Submitting the solution
It is recommended to write all your queries in a .sql file. Upload this file using the submission section present on the next page.
