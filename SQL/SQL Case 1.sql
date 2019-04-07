CREATE TABLE dbo.Customer360
(Customer_id int PRIMARY KEY,
Gender nchar(10),
City_code int,
Age_in_months int,
Basket_count int,
Total_sales_amt int,
Total_sales_qnt int,
Unq_cat_cnt int,
Unq_sCat_cnt int,
Unq_chnl_cnt int,
Last_trans_date Date,
Avg_basket_qty int,
Avg_basket_val int,
Preferred_product_Code int,
Avg_trans_days int,
Loyalty_cat nchar(50),
Loyalty_pts int,
RFM_Seg nchar(50));

-- Preferred_product_Code is the code of the product that the customer purchased most in terms of frequency
-- Avg_trans_days is the average number of days it takes for the customer to purchase again
-- Loyalty_cat is the loyalty plan that the customer has opted
-- Loyalty_points is Number of loyalty points currently earned by customer
-- RFM_Seg is the segment that the particular customer falls post analysis of RFM combinations