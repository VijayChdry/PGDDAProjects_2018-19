companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t",dec = ".")
View(companies)
rounds2 <-read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
View(rounds2)
library(tidyverse)
library(gdata)
library(stringr)
library(RMySQL)
library(dplyr)
library(tidyr)

#Table 1.1

#How many unique companies are present in the companies file?

companies$permalink <- tolower(companies$permalink)
unique_companies <- length(unique(companies$permalink))


#How many unique companies are present in rounds2?

rounds2$company_permalink <- tolower(rounds2$company_permalink)
unique_companie_rounds2 <- length(unique(rounds2$company_permalink))


#In the companies data frame, which column can be used as the  unique key for each company? Write the name of the column.
companies$permalink


#Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
setdiff(rounds2$company_permalink, companies$permalink)


#Merge the two data frames so that all  variables (columns)  in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?

master_frame<-merge(companies, rounds2, by.x = c("permalink"), by.y = c("company_permalink"), all = TRUE)




#Table - 2.1
#Checkpoint 2: Investment analysis

# For Average, We are removing the NA values as replacing it with zero has more affect on the average then ignoring it.
#also we already have many records which have 0 as value so we are ignoring thee rows rahter than replacing
#with any other value

#Average funding amount of venture type

group_venture <- filter(master_frame, funding_round_type == "venture")
avg_venture <-summarise(group_venture, Average= mean(group_venture$raised_amount_usd, na.rm = TRUE))
avg_venture



#Average funding amount of angel type

group_angel <- filter(master_frame, funding_round_type == "angel")
avg_angel <-summarise(group_angel, Average= mean(group_angel$raised_amount_usd, na.rm = TRUE))
avg_angel



#Average funding amount of seed type

group_seed <- filter(master_frame, funding_round_type == "seed")
avg_seed <-summarise(group_seed, Average= mean(group_seed$raised_amount_usd, na.rm = TRUE))
avg_seed



#Average funding amount of private equity type

group_private <- filter(master_frame, funding_round_type == "private_equity")
avg_private <-summarise(group_private, Average= mean(group_private$raised_amount_usd, na.rm = TRUE))
avg_private



#Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, which investment type is the most suitable for them?

grouping <-aggregate(master_frame$raised_amount_usd, by=list(master_frame$funding_round_type) ,FUN= function(x) mean(x,na.rm=TRUE))
most_suitable_investment <- filter(grouping, grouping$x > 5000000 & grouping$x < 15000000)
most_suitable_investment



#Table 3.1
#Checkpoint 3: Country Analysis

#Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
# I am assuming investment type as "venture" as it's most suitable investment type.

Grouping_country <- aggregate(group_venture$raised_amount_usd, by=list(group_venture$country_code), sum)
arrange(Grouping_country, desc(Grouping_country$x))
Top9 <-head(arrange(Grouping_country, desc(Grouping_country$x)), n = 9)
Top9

#Checkpoint 4: Sector Analysis 1

mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)
str(mapping)
summary(mapping)
View(mapping)



#Extract the primary sector of each category list from the category_list column
#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors (Note that 'Others' is also considered one of the main sectors)
Category_split_primary <-separate(master_frame, col= category_list, sep = "\\|" ,into =c("Primary_Sector"),remove=FALSE)

#converting mapping data frame from wide to long and removing unwanted rows
mapping_gather <- gather(mapping,key=category,value = sector,2:10)
mapping_gather$category_list <-str_replace_all(mapping_gather$category_list, "0", "na")
mapping_gather <- mapping_gather[-which(mapping_gather$sector==0),]

# converting sector to lowercase for merge
Category_split_primary$Primary_Sector <- tolower(Category_split_primary$Primary_Sector)
mapping_gather$category_list <- tolower(mapping_gather$category_list)
master_mapping <- merge(Category_split_primary,mapping_gather, by.x = c("Primary_Sector"), by.y = c("category_list"), sort = FALSE, all = TRUE)
View(master_mapping)


#Checkpoint 5: Sector Analysis 2
#Sector-wise Investment Analysis

#Creation of D1, D2 and D3 based on given criteria and handling missing values.
D1 <- filter(master_mapping, country_code == "USA" & funding_round_type == "venture" & raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)
D1$Primary_Sector[is.na(D1$Primary_Sector)]<- "Blank" 
D2 <- filter(master_mapping, country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)
D3 <- filter(master_mapping, country_code == "IND" & funding_round_type == "venture" & raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)
D2$Primary_Sector[is.na(D2$Primary_Sector)]<- "Blank" 
D3$Primary_Sector[is.na(D3$Primary_Sector)]<- "Blank" 


#Total Number of investments(Count) & SUM of investment for D1 and D2
D1_investment_details <- group_by(D1,category)
D1_invesment_count <- summarise(D1_investment_details,count = sum(sector,na.rm = TRUE))
D1_invesment_sum <- summarise(D1_investment_details,sum = sum(raised_amount_usd,na.rm = TRUE))

D2_investment_details <- group_by(D2,category)
D2_invesment_count <- summarise(D2_investment_details,count = sum(sector,na.rm = TRUE))
D2_invesment_sum <- summarise(D2_investment_details,sum = sum(raised_amount_usd,na.rm = TRUE))

D3_investment_details <- group_by(D3,category)
D3_invesment_count <- summarise(D3_investment_details,count = sum(sector,na.rm = TRUE))
D3_invesment_sum <- summarise(D3_investment_details,sum = sum(raised_amount_usd,na.rm = TRUE))

#Total number of investment(Count)
D1_total_count <- sum(D1_invesment_count$count)
D2_total_count <- sum(D2_invesment_count$count)
D3_total_count <- sum(D3_invesment_count$count)


#Total amount of investment
D1_total_investment <- sum(D1_invesment_sum$sum)
D2_total_investment <- sum(D2_invesment_sum$sum)
D3_total_investment <- sum(D3_invesment_sum$sum)

#For the top sector count-wise (point 3), which company received the highest investment?
D1_top_companies_others <-summarise(group_by(filter(D1,category =="Others"),name), sum_company = sum(raised_amount_usd,na.rm = TRUE) )
D2_top_companies_others <-summarise(group_by(filter(D2,category =="Others"),name), sum_company = sum(raised_amount_usd,na.rm = TRUE) )
D3_top_companies_others <-summarise(group_by(filter(D3,category =="Others"),name), sum_company = sum(raised_amount_usd,na.rm = TRUE) )

# For the second-best sector count-wise (point 4), which company received the highest investment?
D1_top_companies_Social <-summarise(group_by(filter(D1,category =="Social..Finance..Analytics..Advertising"),name), sum_company = sum(raised_amount_usd,na.rm = TRUE) )
D2_top_companies_Social <-summarise(group_by(filter(D2,category =="Social..Finance..Analytics..Advertising"),name), sum_company = sum(raised_amount_usd,na.rm = TRUE) )
D3_top_companies_Social <-summarise(group_by(filter(D3,category =="Social..Finance..Analytics..Advertising"),name), sum_company = sum(raised_amount_usd,na.rm = TRUE) )









