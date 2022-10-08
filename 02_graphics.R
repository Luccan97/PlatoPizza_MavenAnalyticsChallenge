library(cowplot)
library(ggplot2)

# Graphics

# First heatmap
heatmap1 <- ggplot(heatmap1.df,
       aes(hour,reorder(weekday, desc(weekday)),fill = mean))+
        geom_tile(show.legend = F,
                color= "grey",
                lwd = 0.5,
                linetype = 1) +
        scale_x_continuous(breaks = 1:24, expand = c(0,0))+
        scale_fill_gradient(low = "wheat", 
                            high = "tomato")+
         # scico::scale_fill_scico(palette = "lajolla") +
        theme_minimal(base_size = 8)+
        labs(x="", y="", fill = "") +
        geom_text(aes(label = mean), color = "#556B2F") +
        theme_classic()+
        theme(legend.position = "bottom",
              axis.text = element_text(size = 12)) +
        geom_rect(aes(ymin = which(levels(as.factor(weekday))=="Friday") +0.5,
                      ymax = which(levels(as.factor(weekday))=="Monday") -0.5,
                      xmin=11.5,
                      xmax=13.5),
                  alpha = 0,
                  color = "#556B2F",
                  size =1,
                  show.legend = F) +
        
        geom_rect(aes(ymin = which(levels(weekday)=="Sunday") -0.5,
                      ymax = which(levels(weekday)=="Monday") +0.5,
                      xmin=16.5,
                      xmax=19.5),
                  alpha = 0,
                  color = "#556B2F",
                  size =1,
                  show.legend = F)
heatmap1
getwd()
ggsave(filename = "graphics/heatmap1.jpeg")

# Create second heatmap
heatmap2 <- ggplot(heatmap2.df,
                   aes(hour,reorder(weekday, desc(weekday)),fill = mean))+
        geom_tile(show.legend = F,
                  color= "grey",
                  lwd = 0.5,
                  linetype = 1) +
        scale_x_continuous(breaks = 1:24, expand = c(0,0))+
        scale_fill_gradient(low = "wheat", 
                            high = "tomato")+
        # scico::scale_fill_scico(palette = "lajolla") +
        theme_minimal(base_size = 8)+
        labs(x="", y="", fill = "") +
        geom_text(aes(label = mean), color = "#556B2F") +
        theme_classic()+
        theme(legend.position = "bottom",
              axis.text = element_text(size = 12)) +
        geom_rect(aes(ymin = which(levels(as.factor(weekday))=="Friday") +0.5,
                      ymax = which(levels(as.factor(weekday))=="Monday") -0.5,
                      xmin=11.5,
                      xmax=13.5),
                  alpha = 0,
                  color = "#556B2F",
                  size =1,
                  show.legend = F) +
        
        geom_rect(aes(ymin = which(levels(weekday)=="Saturday")+0.5,
                      ymax = which(levels(weekday)=="Sunday")-0.5,
                      xmin=15.5,
                      xmax=18.5),
                  alpha = 0,
                  color = "#556B2F",
                  size =1,
                  show.legend = F)+
        geom_rect(aes(ymin = which(levels(weekday)=="Sunday")-0.5,
                      ymax = which(levels(weekday)=="Monday")+0.5,
                      xmin=18.5,
                      xmax=20.5),
                  alpha = 0,
                  color = "#556B2F",
                  size =1,
                  show.legend = F)
heatmap2
# Graphics
ggsave(filename = "graphics/heatmap2.jpeg")


# Pizza Types Graphics

g3.df <- g3 %>%
        mutate(quantity2 = prettyNum(quantity,big.mark = ","),
               sales.worth = case_when(valor > 41683 - 4000 ~ "Best sales worth",
                                       valor < 11352+ 3800 ~ 'Worst sales worth',
                                       TRUE ~ "")) %>%
        mutate(pos = case_when(sales.worth == "Best sales worth" ~ -1050,
                               name.y %in% c("The Green Garden ", "The Spinach Supreme ") ~ -50,
                               name.y == "The Brie Carre " ~ 400),
               valor2 = paste0(strtrim(valor,2),"K"))

ggplot(g3.df, aes(x = reorder(name.y, quantity), y = quantity))+
        geom_col(aes(fill = sales.worth))+
        # geom_col(width = 0.75, alpha =0.7,
        #          aes(fill = ifelse(valor > max(valor)-4000, "Best Sales Worth",""))) +
        coord_flip() +
        theme_minimal_vgrid() +
        geom_text(nudge_y = -150,
                aes(label = paste0("$",valor2))) +
        scale_fill_manual(values = c("wheat","tomato", "grey")) +
        labs(x = "Pizza Type", y = "Quantity of Pizzas Sold", fill = "") +
        theme(legend.position = "top",
              axis.text.y = element_text(size = 10))
        

ggsave(filename = "graphics/pizzaTypes.jpeg", height = 9.5, width = 9)



###### World Cloud as a circle

# Saved this one manually

 wordcloud(words = t1$value, 
          freq = t1$freq, max.words = 73, random.order = F,
          colors = brewer.pal(8, "Dark2"),
          rot.per = 0.35,scale = c(3,.5),
          use.r.layout = F)


 
 ### Size pizza Time series
 
 ggplot(timeseries, aes(x = date, y = n, group = size))+
         # geom_line(aes(color = size)) +
         tidyquant::geom_ma(aes(color = size), linetype = 1, size = 2)+
         theme_minimal_hgrid()+
         scale_x_date(breaks = "1 month", date_labels = "%b") +
         scale_color_manual(values = c("tomato", "wheat", 'darkgreen', "yellow", 'grey'))+
         labs(y = "Moving Avg. Quantity Pizza Sold", x = "", color = "Size") +
         theme(legend.position = "top")

 ggsave(filename = "graphics/timeseries.jpeg", height = 5, width = 10)
 