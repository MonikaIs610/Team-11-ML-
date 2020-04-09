#####################################################################################################
                                          #Importing packages
#####################################################################################################

library(readxl)
library(xlsx)
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging


#####################################################################################################
                                          #Importing Dataset
#####################################################################################################

group_assignment_dataset <- read_excel("group_assignment_dataset.xlsx")
View(group_assignment_dataset)


#####################################################################################################
                                            #Renaming columns
#####################################################################################################

names(group_assignment_dataset)[names(group_assignment_dataset) == "Publisher ID"] <- "Publisher_ID"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Publisher Name"] <- "Publisher_Name"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Keyword ID"] <- "Keyword_ID"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Match Type"] <- "Match_Type"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Keyword Group"] <- "Keyword_Group"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Bid Strategy"] <- "Bid_Strategy"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Keyword Type"] <- "Keyword_Type"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Search Engine Bid"] <- "Bid"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Click Charges"] <- "Click_Charges"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Avg. Cost per Click"] <- "Cost_per_Click"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Engine Click Thru %"] <- "Click_Rate"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Avg. Pos."] <- "Avg_Pos"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Trans. Conv. %"] <- "Conversion_Rate"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Total Cost/ Trans."] <- "Cost_per_Booking"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Amount"] <- "Total_Revenue"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Total Cost"] <- "Total_Cost"
names(group_assignment_dataset)[names(group_assignment_dataset) == "Total Volume of Bookings"] <- "Bookings"


#####################################################################################################
                                    #New variables creation
#####################################################################################################

#Create conversion column 1 or 0, depending if there was any conversion or not
group_assignment_dataset$Flag_Conversion <-c()

for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Bookings[i]>0){
    group_assignment_dataset$Flag_Conversion[i] <- 1
  }else{
    group_assignment_dataset$Flag_Conversion[i] <- 0
  }
}

#Create Profit column
group_assignment_dataset$Profit <-c()

group_assignment_dataset$Profit <- group_assignment_dataset$Total_Revenue - group_assignment_dataset$Total_Cost


#Create Ticket_Amount column
group_assignment_dataset$Ticket_Amount <-c()

for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Bookings[i] == 0){
    group_assignment_dataset$Ticket_Amount[i] <- 0
  }else{
    group_assignment_dataset$Ticket_Amount[i] <- group_assignment_dataset$Total_Revenue[i]/group_assignment_dataset$Bookings[i]
  }
}

#Create ROA column
group_assignment_dataset$ROA <- group_assignment_dataset$Total_Revenue/group_assignment_dataset$Total_Cost

#Cleaning Match Type - Found that different SE use different names for the same match types
for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Match_Type[i] == 'Advanced'){
    group_assignment_dataset$New_Match_Type[i] <- 'Broad'
  }else if (group_assignment_dataset$Match_Type[i] == 'Standard'){
    group_assignment_dataset$New_Match_Type[i] <- 'Exact'
  }else{
    group_assignment_dataset$New_Match_Type[i] <- group_assignment_dataset$Match_Type[i]
  }
}

#Grouping Keyword Group

for (i in 1:nrow(group_assignment_dataset)){
  if(group_assignment_dataset$Keyword_Group[i] == "Air France"|group_assignment_dataset$Keyword_Group[i] == "Air France Brand"|group_assignment_dataset$Keyword_Group[i] == "Air France Website"|group_assignment_dataset$Keyword_Group[i] == "Business Class - Generic"|group_assignment_dataset$Keyword_Group[i] == "Google|Flights Relaunch"){group_assignment_dataset$Keyword_Group_new[i] <- "Air France"}
  else if(group_assignment_dataset$Keyword_Group[i] == "Athens"|group_assignment_dataset$Keyword_Group[i] == "France"|group_assignment_dataset$Keyword_Group[i] == "Lyon"|group_assignment_dataset$Keyword_Group[i] == "Marseille"|group_assignment_dataset$Keyword_Group[i] == "Nice"|group_assignment_dataset$Keyword_Group[i] == "Nice Branded"|group_assignment_dataset$Keyword_Group[i] == "Paris"|group_assignment_dataset$Keyword_Group[i] == "Paris Branded"|group_assignment_dataset$Keyword_Group[i] == "Tour de France"|group_assignment_dataset$Keyword_Group[i] == "Barcelona"|group_assignment_dataset$Keyword_Group[i] == "Europe"|group_assignment_dataset$Keyword_Group[i] == "Florence"|group_assignment_dataset$Keyword_Group[i] == "Google|Athens"|group_assignment_dataset$Keyword_Group[i] == "Google|Barcelona"|group_assignment_dataset$Keyword_Group[i] == "Google|Europe"|group_assignment_dataset$Keyword_Group[i] == "Google|Italy"|group_assignment_dataset$Keyword_Group[i] == "Google|rabat"|group_assignment_dataset$Keyword_Group[i] == "Google|Rome"|group_assignment_dataset$Keyword_Group[i] == "Google|Spain"|group_assignment_dataset$Keyword_Group[i] == "Italy"|group_assignment_dataset$Keyword_Group[i] == "Madrid"|group_assignment_dataset$Keyword_Group[i] == "Google|Rome"|group_assignment_dataset$Keyword_Group[i] == "Spain"){group_assignment_dataset$Keyword_Group_new[i] <- "Europe"}
  else if(group_assignment_dataset$Keyword_Group[i] == "Unassigned"){group_assignment_dataset$Keyword_Group_new[i] <- "Unassigned"}
  else{
    group_assignment_dataset$Keyword_Group_new[i] <- "US"
  }
}


#####################################################################################################
                                          #Variable Analysis
#####################################################################################################

#Match Type
matchTypeBoxplotData <- subset(x=group_assignment_dataset, subset = (between(group_assignment_dataset$ROA,1,1000)&(group_assignment_dataset$New_Match_Type != 'N/A')))
View(matchTypeBoxplotData)
ggplot(matchTypeBoxplotData, aes(New_Match_Type, ROA)) + geom_boxplot() + facet_grid(.~Publisher_Name)

#New Keyword Group
KGroupBoxplotData <- subset(x=group_assignment_dataset, subset = (between(group_assignment_dataset$ROA,1,500)))
View(KGroupBoxplotData)
ggplot(KGroupBoxplotData, aes(Keyword_Group_new, ROA)) + geom_boxplot() + facet_grid(.~Publisher_Name)

#Deleting the one value with N/A Match Type
index <- which(group_assignment_dataset$New_Match_Type == 'N/A')
group_assignment_dataset <- group_assignment_dataset[-index,]

#Bids
qplot(group_assignment_dataset$Bid, geom="histogram", bins = 15)
summary(group_assignment_dataset$Bid)

#Clicks
ggplot(data=group_assignment_dataset, aes(Clicks)) + geom_histogram()
summary(group_assignment_dataset$Clicks)#Prevalence of ones and clear outliers

#Click Charges
ggplot(data=group_assignment_dataset, aes(Click_Charges)) + geom_histogram()
summary(group_assignment_dataset$Click_Charges)#Clear outliers plus a zero

ggplot(data=group_assignment_dataset, aes(Click_Charges, Clicks)) + geom_point()#Correlation with clicks

#Cost per Click
ggplot(data=group_assignment_dataset, aes(Cost_per_Click)) + geom_histogram()
summary(group_assignment_dataset$Cost_per_Click) #Some outliers plus a zero

#Deleting the one value with cost 0 - Unnecesary because deleting the Match Type N/A deletes this as well
#index <- which(group_assignment_dataset$Click_Charges==0)
#group_assignment_dataset <- group_assignment_dataset[-index,]

#Impressions
ggplot(data=group_assignment_dataset, aes(Impressions)) + geom_histogram()
summary(group_assignment_dataset$Impressions) #Clear outliers

#Click Rate
ggplot(data=group_assignment_dataset, aes(Click_Rate)) + geom_histogram()
summary(group_assignment_dataset$Click_Rate) #Some outliers

#Conversion Rate
ggplot(data=group_assignment_dataset, aes(Conversion_Rate)) + geom_histogram()
summary(group_assignment_dataset$Conversion_Rate) #Prevalence of zero

nrow(group_assignment_dataset[group_assignment_dataset$Conversion_Rate>0,]) #Only 367 keywords convert to sales from almost 4K

summary(group_assignment_dataset[group_assignment_dataset$Conversion_Rate>0,]$Conversion_Rate) #Huge outliers remain

nrow(subset(group_assignment_dataset,subset = (Conversion_Rate>0 & Profit>0),Keyword_ID))

#Cost per Booking
ggplot(data=group_assignment_dataset, aes(Cost_per_Booking)) + geom_histogram()
summary(group_assignment_dataset$Cost_per_Booking) #Prevalence of zero

summary(group_assignment_dataset[group_assignment_dataset$Cost_per_Booking>0,]$Cost_per_Booking) #Huge outliers

ggplot(data=group_assignment_dataset, aes(Conversion_Rate, Cost_per_Booking)) + geom_point()

#Total Revenue
ggplot(data=group_assignment_dataset, aes(Total_Revenue)) + geom_histogram(bins = 20) + xlim(0,10000) + ylim(0,100)
summary(group_assignment_dataset$Total_Revenue) #Zeros and outliers

sum(group_assignment_dataset[group_assignment_dataset$Conversion_Rate==0,]$Total_Cost)

#Total Cost
ggplot(data=group_assignment_dataset, aes(Total_Cost)) + geom_histogram()
summary(group_assignment_dataset$Total_Cost) #Outliers

ggplot(data=group_assignment_dataset, aes(Total_Revenue, Total_Cost)) + geom_point()

#Bookings
ggplot(data=group_assignment_dataset, aes(Bookings)) + geom_histogram()
summary(group_assignment_dataset$Bookings) #Outliers

#Profit
ggplot(data=group_assignment_dataset, aes(Profit)) + geom_histogram()
summary(group_assignment_dataset$Profit) #Outliers

#Ticket Amount
ggplot(data=group_assignment_dataset, aes(Ticket_Amount)) + geom_histogram()
summary(group_assignment_dataset$Ticket_Amount) #Outliers

#ROA
ggplot(data=group_assignment_dataset, aes(ROA)) + geom_histogram() + xlim(0,500) + ylim(0,100)
summary(group_assignment_dataset[group_assignment_dataset$Conversion_Rate>0,]$ROA) #Outliers

#Define ROA cutoff in 200 and flag the outliers

for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$ROA[i] > 100){
    group_assignment_dataset$Outlier_ROA[i] <- 1
  }else{
    group_assignment_dataset$Outlier_ROA[i] <- 0
  }
}
summary(group_assignment_dataset$Outlier_ROA)

#Flagging records with any revenue

for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Total_Revenue[i] > 0){
    group_assignment_dataset$Flag_Revenue[i] <- 1
  }else{
    group_assignment_dataset$Flag_Revenue[i] <- 0
  }
}
summary(group_assignment_dataset$Flag_Revenue)

#Grouping Campaigns

for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Campaign[i] %in% c('Air France Brand & French Destinations','Air France Branded','Air France Global Campaign')){
    group_assignment_dataset$Campaign_Group[i] <- 'Air France'
  }else if (group_assignment_dataset$Campaign[i]=='Business Class'){
    group_assignment_dataset$Campaign_Group[i] <- 'Business Class'
  }else if ('Geo Targeted' %in% group_assignment_dataset$Campaign[i]){
    group_assignment_dataset$Campaign_Group[i] <- 'Geo Targeted'
  }else if (group_assignment_dataset$Campaign[i]=='Unassigned'){
    group_assignment_dataset$Campaign_Group[i] <- 'Unassigned'
  }else{
    group_assignment_dataset$Campaign_Group[i] <- 'General'
  }
}

#####################################################################################################
                                        #Exporting Dataset
#####################################################################################################

write.csv(group_assignment_dataset, file = 'groupDataset.csv')

#####################################################################################################
                                          #Pivot Tables
#####################################################################################################

#Pivot table to evaluate publishers

aggregated_revenue = sum(group_assignment_dataset$Total_Revenue)
aggregated_cost = sum(group_assignment_dataset$Total_Cost)

publisher_pivot <- group_by(group_assignment_dataset, Publisher_Name)
publisher_pivot <- summarise(publisher_pivot, 
                             Revenue = sum(Total_Revenue),
                             `%Rev` = Revenue/aggregated_revenue*100,
                             Cost = sum(Total_Cost),
                             `%Cost` = Cost/aggregated_cost*100,
                             Profit = sum(Profit),
                             Total_ROA = Revenue/Cost,
                             Impressions= sum(Impressions),
                             Click = sum(Clicks),
                             Total_Bookings = sum(Bookings),
                             CPC = Cost/Click,
                             CPB = Cost/Total_Bookings,
                             avg_ticket = mean(Ticket_Amount),
                             Click_Thru = Click/Impressions,
                             Conversion = Total_Bookings/Click,
                             Prob_Purchase = Total_Bookings/sum(Impressions),
                             Campaigns = n_distinct(Campaign)
)

View(publisher_pivot)

#Pivot table for publisher and match type

publisher_pivot2 <- subset(x=group_assignment_dataset, subset = (between(group_assignment_dataset$ROA,1,500)))

publisher_pivot2 <- group_by(publisher_pivot2, Publisher_Name, New_Match_Type)

publisher_pivot2 <- summarise(publisher_pivot2,
                              Count_Keywords = n_distinct(Keyword),
                              Median_ROA = median(ROA),
                              Total_Profits = sum(Profit)
)
View(publisher_pivot2)

#Pivot table to evaluate campaigns

aggregated_revenue = sum(group_assignment_dataset$Total_Revenue)
aggregated_cost = sum(group_assignment_dataset$Total_Cost)

campaign_pivot <- group_by(group_assignment_dataset, Campaign)
campaign_pivot <- summarise(campaign_pivot, 
                            Revenue = sum(Total_Revenue),
                            Cost = sum(Total_Cost),
                            Profit = sum(Profit),
                            Total_ROA = Revenue/Cost,
                            Impressions= sum(Impressions),
                            Click = sum(Clicks),
                            Total_Bookings = sum(Bookings),
                            CPC = Cost/Click,
                            CPB = Cost/Total_Bookings,
                            avg_ticket = mean(Ticket_Amount),
                            Search_Engines  = n_distinct(Publisher_Name),
                            Google_Global = any(Publisher_Name =='Google - Global')*1,
                            Google_US = any(Publisher_Name =='Google - US')*1,
                            Yahoo_US = any(Publisher_Name =='Yahoo - US')*1,
                            MSN_Global = any(Publisher_Name =='MSN - Global')*1,
                            MSN_US = any(Publisher_Name =='MSN - US')*1,
                            Overture_Global = any(Publisher_Name =='Overture - Global')*1,
                            Overture_US = any(Publisher_Name =='Overture - US')*1,
                            Keywords  = n_distinct(Keyword),
                            Click_Thru = Click/Impressions,
                            Conversion = Total_Bookings/Click,
                            Prob_Purchase = Total_Bookings/sum(Impressions)
)

View(campaign_pivot)

#Pivot table for Campaign and match type

Campaign_pivot2 <- group_by(group_assignment_dataset, Campaign, Match_Type)

Campaign_pivot2 <- summarise(Campaign_pivot2, 
                             Count_Match_Type = n(),
                             Total_Profit = sum(Profit)
)
View(Campaign_pivot2)

#Pivot table for Campaign and Publisher
Campaign_pivot4 <- group_by(group_assignment_dataset, Campaign, Publisher_Name)

Campaign_pivot4 <- summarise(Campaign_pivot4, 
                             Revenue = sum(Total_Revenue),
                             Count_Match_Type = n(),
                             Cost = sum(Total_Cost),
                             Profit = sum(Profit),
                             Total_ROA = Revenue/Cost
)
View(Campaign_pivot4)

#Pivot table for Campaign and New Keyword Group

Campaign_pivot3 <- group_by(group_assignment_dataset, Campaign, Keyword_Group_new)

Campaign_pivot3 <- summarise(Campaign_pivot3, 
                             Count_ = n(),
                             Total_Profit = sum(Profit)
)
View(Campaign_pivot3)

#Profitable and Non-Profitable Campaigns by Publisher
Google_Global_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$Google_Global)
Google_US_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$Google_US)
Yahoo_US_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$Yahoo_US)
MSN_Global_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$MSN_Global)
MSN_US_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$MSN_US)
Overture_Global_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$Overture_Global)
Overture_US_Profit <- sum(campaign_pivot[campaign_pivot$Profit>0,]$Overture_US)

Google_Global_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$Google_Global)
Google_US_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$Google_US)
Yahoo_US_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$Yahoo_US)
MSN_Global_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$MSN_Global)
MSN_US_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$MSN_US)
Overture_Global_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$Overture_Global)
Overture_US_No_Profit <- sum(campaign_pivot[campaign_pivot$Profit<=0,]$Overture_US)


#Pivot table for Keyword
keyword_pivot <- group_by(group_assignment_dataset, Keyword)
keyword_pivot <- summarise(keyword_pivot,
                           Revenue = sum(Total_Revenue),
                           Cost = sum(Total_Cost),
                           Profit = sum(Profit),
                           Total_ROA = Revenue/Cost,
                           Impressions= sum(Impressions),
                           Click = sum(Clicks),
                           Total_Bookings = sum(Bookings),
                           CPC = Cost/Click,
                           CPB = Cost/Total_Bookings,
                           avg_ticket = mean(Ticket_Amount),
                           Search_Engines  = n_distinct(Publisher_Name),
                           Google_Global = any(Publisher_Name =='Google - Global')*1,
                           Google_US = any(Publisher_Name =='Google - US')*1,
                           Yahoo_US = any(Publisher_Name =='Yahoo - US')*1,
                           MSN_Global = any(Publisher_Name =='MSN - Global')*1,
                           MSN_US = any(Publisher_Name =='MSN - US')*1,
                           Overture_Global = any(Publisher_Name =='Overture - Global')*1,
                           Overture_US = any(Publisher_Name =='Overture - US')*1,
                           Campaign  = n_distinct(Campaign),
                           Click_Thru = Click/Impressions,
                           Conversion = Total_Bookings/Click,
                           Prob_Purchase = Total_Bookings/sum(Impressions)
)

#There are about 2K keywords but most of them have negative profits

#####################################################################################################
                                            #Graphs
#####################################################################################################

#1. SE Budget Share
ggplot(publisher_pivot, aes(x=Publisher_Name, y=`%Cost`)) + geom_bar(stat="identity")

#2. Publisher Scatterplot
ggplot(publisher_pivot, aes(y=CPC, x=Prob_Purchase, size=Profit)) + geom_point() + geom_text(aes(label=Publisher_Name), size=3)

#3. Campaign and Publisher Bar Chart
barchartData <- Campaign_pivot4[which(Campaign_pivot4$Total_ROA>6),]
barchartData <- barchartData[-which(barchartData$Campaign=='Unassigned'),]

ggplot(barchartData, aes(x=Campaign, y=Total_ROA, fill=Publisher_Name)) +   
geom_bar(position = "Dodge", stat="identity", size=10) +
coord_flip()

#4. Match Type Influence
scatterData <- publisher_pivot2[-which(publisher_pivot2$Publisher_Name=='MSN - Global'),]
scatterData <- scatterData[-which(scatterData$Publisher_Name=='MSN - US'),]
View(scatterData)

ggplot(scatterData, aes(y=Median_ROA, x=Total_Profits, color=New_Match_Type)) + geom_point()

#5. Scatterplot CPC and ROA per New Keyword Group

ScatterData<- subset(x=group_assignment_dataset, subset = (between(group_assignment_dataset$ROA,1,200)))

ggplot(ScatterData, aes(y=ROA, x=Cost_per_Click, color=Keyword_Group_new)) + geom_point()

#####################################################################################################
                                              #Prediction Models
#####################################################################################################

#Model to predict if a keyword will generate any conversion

model1 <- glm(Flag_Conversion~Publisher_Name+Match_Type, data = group_assignment_dataset, family = 'binomial')
summary(model1)

#Grouping Non-significant publishers

for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Publisher_Name[i] %in% c('Overture - Global','Overture - US','Yahoo - US')){
    group_assignment_dataset$New_Publisher_Name[i] <- 'Other'
  }else{
    group_assignment_dataset$New_Publisher_Name[i] <- group_assignment_dataset$Publisher_Name[i]
  }
}

#Flagging campaigns
for (i in 1:nrow(group_assignment_dataset)){
  if (group_assignment_dataset$Campaign[i] == 'Unassigned'){
    group_assignment_dataset$Flag_Campaign[i] <- 0
  }else{
    group_assignment_dataset$Flag_Campaign[i] <- 1
  }
}

#Re-run model 1

model1 <- glm(Flag_Conversion~New_Publisher_Name+Clicks+Impressions, data = group_assignment_dataset[group_assignment_dataset$Outlier_ROA==0,], family = 'binomial')
summary(model1)

#Function to predict chances of conversion

predict_conversion <- function(publisher, clicks, impressions){
  coef_intercept <- model1[[1]][1]
  coef_clicks <- model1[[1]][6]
  coef_impressions <- model1[[1]][7]
  if(publisher == 'Google - US'){
    coef_publisher <- model1[[1]][2]
  }else if (publisher == 'MSN - Global'){
    coef_publisher <- model1[[1]][3]
  }else if (publisher == 'MSN - US'){
    coef_publisher <- model1[[1]][4]
  }else if (publisher == 'Google - Global'){
    coef_publisher <- 0
  }else{
    coef_publisher <- model1[[1]][5]
  }
  
  logit <- as.numeric(coef_intercept + coef_clicks*clicks + coef_impressions*impressions + coef_publisher)
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}

predict_conversion('Google - Global',500,1000)

#Try with the addition of new variables

model2 <- glm(Flag_Conversion~New_Publisher_Name+Clicks+Impressions+New_Match_Type+Keyword_Group_new, data = group_assignment_dataset[group_assignment_dataset$Outlier_ROA==0,], family = 'binomial')
summary(model2)