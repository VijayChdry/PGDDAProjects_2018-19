/* Ensuring that 'ONLY_FULL_GROUP_BY' mode is enabled */

SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;


/* Task 0:
Download the data files for this assignment. Your first task is to create tables from these files. In order to do so, please follow the steps given below sequentially:
1. Open MySQL Workbench
2. Connect to your database using the connection you have created
3. Create a database named superstoresDB
4. In the “Navigator” pane on the left hand side, you will find the created database
5. Right click on the superstoresDB
6. You will see the option called “Table Data Import Wizard”. Click on it.
7. Follow the wizard to create tables by providing the .csv data files that you have downloaded
8. You need to follow the “Table Data Import Wizard” for each data file given for this assignment.
Please refer https://dev.mysql.com/doc/workbench/en/wb-admin-export-import-table.html to get more information on table data import. Once you are done with this task, attempt following tasks:*/

#Vijay_ANSWER: Successfully created a schema as 'superstoresdb' and imported all the 5 tables through above mentioned instructions.


# Task 1: Understanding the data in hand
-- A. Describe the data in hand in your own words. (Word Limit is 500)
-- Vijay_ANSWER - Please execute the below query as i have recorded my view on data in the form of sql query. 
-- you can considered my answer as plain text written in between of double quotes.  

SELECT "A company store maintains the information of customers, products, orders, Shipping details and 
‘some market facts for all the products, customers and orders’ etc in a database called “superstoredb” 
in the form of tables cust_dimen,prod_dimen,orders_dimen,shipping_dimen and market_fact respectively. 
While importing tables are available with generic data types like TEXT which needs to be corrected (VARCHAR(), date..etc) 
post importing into SQL Workbench data types and tables are linked to each other through referential integrity 
constraints and key constraints like primary Key, Foreign Key etc.All dimen tableas are having PK and details 
about customer, products, orders, shipment wherein fact table holds FK and through which it linked with all the
dimen tables and it also hold details about overall orders,sells,cost,profit and product wise margin.
The databse design seems as 'star schema' where fact table is centralized data which is linked with many other 
dimensional tables which holds more details informtion and connected through foreign keys." as MY_RESPONSE;


-- B. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then specifically mention it in your answer.)
/*  Vijay_ANSWER :  

Table Name	         Primary Key	        Foreign Key
cust_dimen	         Cust_id	            NO Foreign Key
orders_dimen	     Ord_id	                NO Foreign Key
prod_dimen	         Prod_id	            NO Foreign Key
shipping_dimen	     Ship_id	            NO Foreign Key
market_fact	         NO Primary Key	        Cust_id, Prod_id, Ord_id, Ship_id

*/

# Task 2: Basic Analysis
-- Write the SQL queries for the following:

-- A. Find the total and the average sales (display total_sales and avg_sales)

SELECT sum(sales) as total_sales , avg(sales) as avg_sales from market_fact;


-- B. Display the number of customers in each region in decreasing order of no_of_customers. The result should contain columns Region, no_of_customers

SELECT Region, count(*) as no_of_customers from cust_dimen group by region order by no_of_customers DESC; 


-- C. Find the region having maximum customers (display the region name and max(no_of_customers)

SELECT Region, count(*) as max_no_of_customers
FROM cust_dimen 
GROUP BY Region 
ORDER BY max_no_of_customers DESC
limit 1;


-- D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)

select prod_id as 'product id', count(Order_Quantity) as no_of_products from market_fact group by Prod_id order by no_of_products DESC;


-- E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)

select Customer_Name , count(Order_Quantity) as no_of_tables from cust_dimen
inner join market_fact on cust_dimen.cust_id = market_fact.cust_id
inner join prod_dimen on prod_dimen.Prod_id = market_fact.prod_id
where region = 'Atlantic'
group by Customer_Name
order by no_of_tables DESC;



# Task 3: Advanced Analysis
/*Write sql queries for the following:  */

-- A. Display the product categories in descending order of profits (display the product
-- category wise profits i.e. product_category, profits)?

select Product_Category, sum(profit) as profits from prod_dimen
inner join market_fact on prod_dimen.Prod_id = market_fact.Prod_id
group by Product_Category
order by Profits DESC; 


-- B. Display the product category, product sub-category and the profit within each subcategory
-- in three columns.

select Product_Category, Product_Sub_Category, sum(profit) as profit from prod_dimen
inner join market_fact on prod_dimen.Prod_id = market_fact.Prod_id
group by Product_Category, Product_Sub_Category
order by Profit; 


-- C. Where is the least profitable product subcategory shipped the most? For the least
-- profitable product sub-category, display the region-wise no_of_shipments and the
-- profit made in each region in decreasing order of profits (i.e. region,
-- no_of_shipments, profit_in_each_region)
-- o Note: You can hardcode the name of the least profitable product subcategory

/* finding the name of the least profitable product subcategory */
select Product_Sub_Category, sum(profit) as profit from prod_dimen
inner join market_fact on market_fact.Prod_id = prod_dimen.Prod_id
group by Product_Sub_Category
order by profit
limit 1;



/*Where is the least profitable product subcategory shipped the most? */

select Region, count(Ship_id) as no_of_shipments, sum(profit) as profit_in_each_region from cust_dimen
inner join market_fact on market_fact.Cust_id = cust_dimen.Cust_id
inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id
where market_fact.Prod_id = 'prod_11'
group by Region
order by no_of_shipments DESC
LIMIT 1; 



/* final query to display the region-wise no_of_shipments and the
profit made in each region in decreasing order of profits (i.e. region,
no_of_shipments, profit_in_each_region) for least profitable product subcategory 
 Note: You can hardcode the name of the least profitable product subcategory    */

select Region, count(Ship_id) as no_of_shipments, sum(profit) as profit_in_each_region from cust_dimen
inner join market_fact on market_fact.Cust_id = cust_dimen.Cust_id
inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id
where market_fact.Prod_id = 'prod_11'
group by Region
order by Profit_in_each_region DESC; 




-- -------------------- END------------------------------------------------------------------------------

