Key to DJNF_Basic_Data_Viz

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

