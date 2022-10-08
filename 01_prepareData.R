# Read Plato's Pizza datasets

# What days and times do we tend to be busiest?
# How many pizzas are we making during peak periods?
# What are our best and worst selling pizzas?
# What's our average order value?
# How well are we utilizing our seating capacity? (we have 15 tables and 60 seats)

library(tidyverse)
library(ggwordcloud)

Sys.getlocale("LC_TIME")

# Read files
files <- list.files("pizza_sales", pattern = ".csv")


orders <- read.csv(file = paste0("pizza_sales/",files[2]))

orders.details <- read.csv(file = paste0("pizza_sales/",files[1]))


pizza.types <- read.csv(file = paste0("pizza_sales/",files[3]))

pizzas <- read.csv(file = paste0("pizza_sales/",files[4]))

# KPI's


# What days and times do we tend to be busiest?

orders2 <- orders.details %>%
        pivot_wider(id_cols = order_id,
                    values_from = quantity,
                    names_from = pizza_id) %>%
        left_join(orders, orders2, by= "order_id") %>%
        pivot_longer(cols = -c(order_id, date, time), values_drop_na = T,
                     values_to = "quantity")

Sys.setlocale("LC_TIME","C")

heatmap1.df <- orders2 %>%
        mutate(hour = as.numeric(strtrim(time,2)),
               weekday = weekdays(as.Date(date, "%Y-%m-%d"), abbreviate = F),
               month = lubridate::month(as.Date(date))) %>%
        group_by(date, hour) %>%
        summarise(total = sum(quantity)) %>%
        mutate(weekday = weekdays(as.Date(date, "%Y-%m-%d"), abbreviate = F)) %>%
        mutate(weekday = factor(weekday,
                                levels= c("Sunday", "Monday", 
                                          "Tuesday", "Wednesday", 
                                          "Thursday", "Friday",
                                          "Saturday"))) %>%
        
        group_by(weekday, hour) %>%
        summarise(mean = round(mean(total, na.rm = T),0))

# What pizza types are favourite between times and days?

orders3 <- left_join(orders2, pizzas, by= c("name" = "pizza_id")) %>%
        mutate(revenue = price * quantity)
         
# KPI's
revenue <- sum(orders3$revenue)

t.pizzas <- sum(orders3$quantity)

t.orders <- length(unique(orders3$order_id))        

avg.order <- round(revenue/t.orders,1)

types.pyzze <- nrow(pizza.types)

categories <- length(unique(pizza.types$category))


nmax <- max(stringr::str_count(pizza.types$ingredients, ",")) + 1

ingredients2 <- pizza.types %>%
        separate(col = ingredients,
                 into = paste0("col", seq_len(nmax)),
                 fill = "right",
                 sep = ", ") %>%
        pivot_longer(cols = -c(pizza_type_id, name, category),
                     names_to = "ingredients")

ingredients <- length(unique(ingredients2$value))-1

# Ingredients per pizza

ingre2 <- pizza.types %>%
        mutate(n.ingredients =  stringr::str_count(pizza.types$ingredients, ",") + 1)

n.ingredients <- round(mean(ingre2$n.ingredients, na.rm = T),0)



orders4 <- left_join(orders3, ingre2, by= "pizza_type_id")

g3 <- orders4 %>%
        group_by(name.y, n.ingredients, category) %>%
        summarise(valor = sum(price),
                  quantity = sum(quantity)) %>%
        arrange(desc(valor)) %>%
        mutate(name.y = gsub("Pizza", "", name.y))



# Heatmap 2


heatmap2.df <- orders %>%
        mutate(hour = as.numeric(strtrim(time,2)),
               weekday = weekdays(as.Date(date, "%Y-%m-%d"), abbreviate = F),
               month = lubridate::month(as.Date(date))) %>%
        group_by(date, hour) %>%
        summarise(total = n()) %>%
        mutate(weekday = weekdays(as.Date(date, "%Y-%m-%d"), abbreviate = F)) %>%
        mutate(weekday = factor(weekday,
                                levels= c("Sunday", "Monday", 
                                          "Tuesday", "Wednesday", 
                                          "Thursday", "Friday",
                                          "Saturday"))) %>%
        
        group_by(weekday, hour) %>%
        summarise(mean = round(mean(total, na.rm = T),0))


# Ingredients World Cloud

cloudWorld <- orders4 %>%
        select(ingredients, quantity) %>%
        slice(rep(seq_len(n()), quantity))%>%
        select(-quantity) 

cloudWorld2 <- data.frame(do.call('rbind', strsplit(as.character(cloudWorld$ingredients),', ',fixed=TRUE))) 
        
cloudWorld2 <- cloudWorld2 %>%
        mutate(id = row_number()) %>%
        pivot_longer(cols = -id) %>%
        select(value) %>%
        group_by(value) %>%
        summarise(freq = n()) %>%
        arrange(freq) 

t1 <- cloudWorld2 %>%
        mutate(value = case_when(value == " Mozzarella Chesse" ~ "Mozzarella Chesse",
               TRUE ~ value)) %>%
        group_by(value) %>%
        summarise(freq = sum(freq)) %>%
        arrange(desc(freq)) %>%
        mutate(pct  = round(freq/t.pizzas * 100,1))


# Size time series 
timeseries <- orders4 %>%
        group_by(date, size) %>%
        summarise(n = sum(quantity)) %>%
        mutate(date = as.Date(date, "%Y-%m-%d"))

save.image("objects.RData")
