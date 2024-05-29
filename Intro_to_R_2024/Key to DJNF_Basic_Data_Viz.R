#Key to DJNF_Basic_Data_Viz

# Your Turn

#**Which Day Had the Most 311 Calls?**
#  Using count to tabulate calls by day

Days <- SF %>% 
  count(call_date2) %>% 
  arrange(desc(n))

head(Days)

#*2) Chart Calls by Day**
  
  
SF %>% 
  count(call_date2) %>% 
  #Sandwich it onto a simple ggplot
  ggplot(aes(x = call_date2, y = n, fill = n)) +
  geom_bar(stat = "identity") +
  labs(title = "311 Calls for Service By Day, San Francisco", 
       subtitle = "SF PD Service Call Data, 2016-2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Day")


#### Your Turn
- **Question**: Identify the top 5 days with the most calls?
  ```{r}

Days %>% 
  slice_max(n, n = 5)
#slice_max to the rescue: https://dplyr.tidyverse.org/reference/slice.html

#Or
#Filter by the value
# Days %>% 
#   filter(n == 232)
```

- **Question**: Identify the top 5 days with the fewest calls?
  ```{r}
Days %>% 
  slice_min(n, n = 5)
#Or
#Filter by the value
# Days %>% 
#   filter(n == 10)


#From DJNF_Gathering_Cleaning_Data

#Find out when nothing happened with the officer's investigation.
#A table filtering the dispositions column to show "no disposition" or "gone on arrival"
Nothing <- SF %>% 
  filter(disposition == "ND" | disposition == "GOA")


#**Serious Actions**: Create a table with the serious actions including citations and arrests police took in the dispositions  

#Arrest, Cited, Criminal Activation, SF Fire Dept Medical Staff engaged

Busted <- SF %>% 
  filter(disposition == "ARR" | disposition == "CIT" | disposition == "CRM" | disposition == "SFD") %>% 
  count(disposition) %>% 
  arrange(desc(n))
head(Busted)



#Chart the number of calls by year and month
SF %>% 
  count(yearmo) %>% 
  group_by(yearmo) %>% 
  ggplot(aes(x = yearmo, y = n, fill=n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90)) +
  #Changes angle of x axis labels
  #coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Homeless Calls After 2017, San Francisco", 
       subtitle = "SF PD Service Call Data by Month 2017-2019",
       caption = "Graphic by Wells",
       y="Number of Calls",
       x="Year")

#Build a summary table with the days of the week with the greatest
#number of calls. Create a graphic. Then build a table to see if the complaints vary by day

SF <- SF %>% 
  mutate(weekday = wday(call_date2, label=TRUE, abbr=FALSE))
Weekday_Count <- SF %>%
  select(weekday, crime_id) %>%
  count(weekday) %>%
  arrange(desc(n))

#Graphic of calls by weekdays


Weekday_Count %>% 
  ggplot(aes(x = weekday, y = n, fill=n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle=90)) +
  #Changes angle of x axis labels
  #coord_flip() +    #this makes it a horizontal bar chart instead of vertical
  labs(title = "Homeless Calls By Weekday in San Francisco", 
       subtitle = "SF PD Service Call Data 2017-2019",
       caption = "Graphic by Moore and Seiter",
       y="Number of Calls",
       x="Weekday")

### How to Clean all of the Crime Columns

- **Cleaning Sequence**
  ```{r}
#convert all text to lowercase
SF$crime1 <- tolower(SF$original_crime_type_name)

#Replace / with a space
SF$crime1 <- gsub("/", " ", SF$crime1)

#Replace '
SF$crime1 <- gsub("'", "", SF$crime1)

#fix space in homeless complaint
SF$crime1 <- gsub("homeless complaint", "homeless_complaint", SF$crime1)

#split crime1 into three new columns
SF <- separate(data = SF, col = crime1, into = c("crime2", "crime3", "crime4"), sep = " ", extra = "merge", fill = "right")

```

For this lesson, we will clean just one column. See Appendix on the method to clean and construct a new dataframe with all of the columns

```{r}
#Clean Crime2
SF <- SF %>%
  mutate(crime_cleaned = case_when(
    str_detect(crime2, '919') ~ "sit_lying",
    str_detect(crime2, '915') ~ "homeless_complaint",
    str_detect(crime2, '915s') ~ "homeless_complaint",
    str_detect(crime2, '915x') ~ "homeless_complaint",  
    str_detect(crime2, '909') ~ "interview",
    str_detect(crime2, '902') ~ "aggress_solicit",
    str_detect(crime2, '811') ~ "intoxicated",
    str_detect(crime2, '601') ~ "trespasser",     
    str_detect(crime2, "aggressive") ~ "aggressive",
    str_detect(crime2, "chop shop") ~ "chop_shop",
    str_detect(crime2, "dog") ~ "dog",    
    str_detect(crime2, "drugs") ~ "drugs",    
    str_detect(crime2, "homeless_complaint") ~ "homeless_complaint",
    str_detect(crime2, "music") ~ "music",
    str_detect(crime2, "panhandling") ~ "panhandling",
    str_detect(crime2, "poss") ~ "possession",
    str_detect(crime2, "sleep") ~ "sleep",
    str_detect(crime2, "tent") ~ "tent",
    TRUE ~ ""
  ))
```


```{r}
#Clean Crime3
SF <- SF %>%
  mutate(crime_cleaned1 = case_when(
    str_detect(crime3, '919') ~ "sit_lying",
    str_detect(crime3, '915') ~ "homeless_complaint",
    str_detect(crime3, '915s') ~ "homeless_complaint",
    str_detect(crime3, '915x') ~ "homeless_complaint",  
    str_detect(crime3, '909') ~ "interview",
    str_detect(crime3, '902') ~ "aggress_solicit",
    str_detect(crime3, '811') ~ "intoxicated",
    str_detect(crime3, '601') ~ "trespasser",     
    str_detect(crime3, "aggressive") ~ "aggressive",
    str_detect(crime3, "chop shop") ~ "chop_shop",
    str_detect(crime3, "dog") ~ "dog",    
    str_detect(crime3, "drugs") ~ "drugs",    
    str_detect(crime3, "homeless_complaint") ~ "homeless_complaint",
    str_detect(crime3, "music") ~ "music",
    str_detect(crime3, "panhandling") ~ "panhandling",
    str_detect(crime3, "poss") ~ "possession",
    str_detect(crime3, "sleep") ~ "sleep",
    str_detect(crime3, "tent") ~ "tent",
    TRUE ~ ""
  ))
```


```{r}
#Clean Crime4
SF <- SF %>%
  mutate(crime_cleaned2 = case_when(
    str_detect(crime4, '919') ~ "sit_lying",
    str_detect(crime4, '915') ~ "homeless_complaint",
    str_detect(crime4, '915s') ~ "homeless_complaint",
    str_detect(crime4, '915x') ~ "homeless_complaint",  
    str_detect(crime4, '909') ~ "interview",
    str_detect(crime4, '902') ~ "aggress_solicit",
    str_detect(crime4, '811') ~ "intoxicated",
    str_detect(crime4, '601') ~ "trespasser",     
    str_detect(crime4, "aggressive") ~ "aggressive",
    str_detect(crime4, "chop shop") ~ "chop_shop",
    str_detect(crime4, "dog") ~ "dog",    
    str_detect(crime4, "drugs") ~ "drugs",    
    str_detect(crime4, "homeless_complaint") ~ "homeless_complaint",
    str_detect(crime4, "music") ~ "music",
    str_detect(crime4, "panhandling") ~ "panhandling",
    str_detect(crime4, "poss") ~ "possession",
    str_detect(crime4, "sleep") ~ "sleep",
    str_detect(crime4, "tent") ~ "tent",
    TRUE ~ ""
  ))
```

--**Create a New Dataframe to Tabulate the Crimes**
  ```{r}
#Three mini dataframes with two columns
crime1 <- SF %>% 
  select(report_date, crime_cleaned)
crime2 <- SF %>% 
  select(report_date, crime_cleaned1) %>% 
  rename(crime_cleaned = crime_cleaned1)
crime3 <- SF %>% 
  select(report_date, crime_cleaned2) %>% 
  rename(crime_cleaned = crime_cleaned2)

#Create a new dataframe called Total_Calls_Master
Total_Calls_Master <- rbind(crime1, crime2, crime3)

#filter blank values, rename columns
Total_Calls_Master <- Total_Calls_Master %>% 
  filter(!crime_cleaned=="") %>% 
  rename(Date=report_date, Complaint = crime_cleaned)

#export
write_csv(Total_Calls_Master, "Total_Calls_Master.csv")
```
--**Fact Check**
  ```{r}
x <- SF %>% 
  select(crime_cleaned) %>% 
  count(crime_cleaned) %>% 
  arrange(desc(n))
head(x)
```


```{r}
Crime_Sums <- Total_Calls_Master %>% 
  select(Complaint) %>% 
  count(Complaint) %>% 
  arrange(desc(n))
head(Crime_Sums)
```

Make into html table
```{r}
#install.packages("kableExtra")
library(kableExtra)
```

```{r}
#This makes html tables called "kables"
Crime_Sums %>% 
  kable() %>%
  kable_styling("striped")
```





