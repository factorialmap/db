library(tidyverse)
library(scales)
library(ggrepel)

data_cars <- 
  readxl::read_xlsx("C:/Users/Usuario/OneDrive/Documents/customers/carvalhoribeiro/db_cars.xlsx")


write.csv(data_cars, file = "ev_sales_yoy.csv", row.names = FALSE)



