# packages to use and prepare them with "library"
install.packages(c('insuranceData', 'dplyr' ,'ggplot2', 'visdat', 'skimr', 'kableExtra'))
library(insuranceData)
library(dplyr)
library(ggplot2)
data(dataCar) # uploads data
str(dataCar) 
summary(dataCar) # shows statistical descriptive information

# making database queries and storing them on variables
################################################################################
# first query
# vehicle type and vehicle value
query1 <- dataCar %>%
  group_by(veh_body) %>%
  summarise(veh_val = sum(veh_value)) %>%
  arrange(desc(veh_val))
query1

ggplot(query1, aes(x = reorder(veh_body, -veh_val), y = veh_val, fill = veh_body)) +
  geom_bar(stat = "identity") + # use 'identity' because we're plotting already summarized values
  labs(title = "Total Vehicle Value by Vehicle Type",
       x = "Vehicle Type",
       y = "Total Vehicle Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
################################################################################
# second query
# exposure and gender including vehicle type
query2 <- dataCar %>%
  group_by(gender, veh_body) %>%
  summarise(gender_exposure = sum(exposure)) %>%
  arrange(desc(gender_exposure))
query2

ggplot(query2, aes(x = gender, y = gender_exposure, fill = gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(~veh_body, scales = "free_y") +
  labs(title = "Total Exposure by Gender and Vehicle Type",
       x = "Gender",
       y = "Total Exposure") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  theme_minimal()
################################################################################
# third query
# multiple plots
query3 <- dataCar %>%
  group_by(agecat, gender, veh_body) %>%
  summarise(total_claims = sum(numclaims)) %>%
  arrange(desc(total_claims))

ggplot(query3, aes(x = agecat, y = total_claims, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~veh_body) +
  labs(title = "Number of Claims by Vehicle Age, Gender, and Vehicle Type",
       x = "Age Category",
       y = "Number of Claims") +
  theme_minimal()

head(dataCar$veh_body)
