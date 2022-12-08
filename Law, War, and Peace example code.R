pacman::p_load(dplyr, readxl, data.table)

#### The code for Figure 2

asMade <- read_xlsx("As made Acts and Legislative Instruments.xlsx") %>% data.table() # Import the data set from Excel (you need to append the directory in which the Excel file appears, such as "C:/UserName/Downloads/As made Acts and Legislative Instruments.xlsx")

# Create a subset of the 'As made legislation.xlsx' data set, including only Acts and regulations.
actsRegs <- asMade[grepl("Regulations|Act", asMade$regsLis)] 

# Create a further subset of data for Acts and regulations made during the First World War.
data <- actsRegs[legDate >= "1914-07-28" & legMonth <= "1918-11-11"] 

# Create a new column that will be used to visualise the number of Acts and regulations based on their type. 
data$lawType <- "Other"
# If the name of the Act or regulation contains "War Precautions", then type of legislation is "War Precautions framework". The 'grepl' function searches for the relevant term in the name column. 
data[grepl("War Precautions", data$name)]$lawType <- "War Precautions framework"
# If the name of the Act or regulation contains "Appropriation", then type of legislation is "Appropriations".
data[grepl("Appropriation", data$name)]$lawType <- "Appropriations"

# Use functions from the dplyr R package to summarise the data based on the month in which it was made and the type of legislation we have assigned it ("Appropriations", "War Precautions framework", or "Other").
data <- data %>%
  group_by(legMonth, lawType) %>% 
  summarise(instrumentsMade = length(name)) %>% data.table()

# We now have a new data set comprised of three columns - 
#     First column: a month (legMonth)
#     Second column: the legislation type (lawType)
#     Third column: the number of Acts and regulations made in that month of the particular type

# E.g. 
#   legMonth    lawType                     instrumentsMade
#   1914-08-01   Other                      32
#   1914-09-01   Other                      9
#   1914-10-01   Other                      25
#   1914-10-01   War Precautions framework  2

# Create the Figure using the ggplot R package. 
data %>%
  ggplot(aes(x=as.Date(legMonth), # x axis is the month in which the legislation was made
             y=instrumentsMade, # y axis is the number of Acts and instruments made in the month
             fill= factor(lawType, # Make the colour fill of the legislation based on the lawType
                          levels = c("War Precautions framework", "Appropriations", "Other")))) + # Manually determine the order in which the legislation types should be displayed.  
  geom_col() + # Create bar plot
  theme_bw() + # The ALRC has its own theme for Figure design, but theme_bw is a default that appears in the ggplot package
  theme(legend.position = "bottom", 
        legend.text.align = 0, 
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(), 
        legend.key = ggplot2::element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,0,0,0)) +
  xlab("Month") +
  ylab("Acts and legislative instruments made") +
  scale_y_continuous(expand = c(0, 0, 0.05, 0), label=comma) + # Ensure y axis is recognised as containing continuous numerical data for which a comma is needed. The expand option limits the white space around the Figure. 
  scale_x_date(date_labels = "%b %Y", expand = c(0.01, 0.01)) + # Ensure x axis is recognised as containing dates. The expand option limits the white space around the Figure. 
  scale_fill_manual(values = c("#ffdd00", "#80b0c9", "#006092")) # Manually set the colours used to fill the bars.

```

#### The code to calculate topic page length

# To analyse the number of pages of legislation relating to particular
# topics in the Second World War, the ALRC used the following code.

asMade <- read_xlsx("As made Acts and Legislative Instruments.xlsx") %>% data.table() # Import the data set from Excel (you need to append the directory in which the Excel file appears, such as "C:/UserName/Downloads/As made Commonwealth legislation.xlsx")

# Create a subset of the 'As made legislation.xlsx' data set, including only Acts and regulations.
actsRegs <- asMade[grepl("Regulations|Act", asMade$regsLis)] 

# Create a further subset of data for Acts and regulations made during the First World War.
data <- actsRegs[legMonth >= "1939-09-01" & legMonth <= "1945-09-01"]

# Include only Acts and Regulations that include 'National Security' in their name. These are part of the National Security legislative framework in the Second World War.  
data <- data[grepl("National Security", data$name)]

#### Use 'grepl' to identify subject matter of instruments based on their names. 

# Product-specific arrangements, such as for coal and meat
nationalSecurityProducts <- sum(data[grepl("Agricul|Coal|Egg|Meat|Wool|Wheat|Sheep|Gold|Minerals|Fish|Flour", data$name)]$legPages)

#Industrial relations, employment, and economic organisation
nationalSecurityIndustrialPeace <- sum(data[grepl("Industrial Peace|Economic Org|Employment", data$name)]$legPages)

# Internment and prisoners of war
nationalSecurityInternment <- sum(data[grepl("Internment|Prisoners", data$name)]$legPages)

# Housing, including relationships between landlords and tenants
nationalSecurityHousing <- sum(data[grepl("Housing|Landlord", data$name)]$legPages)

# Prices and rationing
nationalSecurityPricing <- sum(data[grepl("Pricing|Rationing", data$name)]$legPages)

# Tea controls
nationalSecurityTea <- sum(data[grepl("Tea Control", data$name)]$legPages)

# Monetary control and financial markets
nationalSecurityFinancial <- sum(data[grepl("Monetary|Securities|Insurance", data$name)]$legPages)
```
