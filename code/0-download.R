#load tidyverse libraries
library(tidyverse)

#load url
url = "http://www.stat.columbia.edu/~gelman/arm/examples/
speed.dating/Speed%20Dating%20Data.csv"
raw_data = read_csv(url) #read file

write_csv(x = raw_data, file = "data/raw/raw_data.csv") #write to save
