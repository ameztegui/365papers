rm(list=ls())

library(dplyr)
library(ggplot2)
library(gsheet)
library(ggvis)

papers<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1YfoVr_NlJkR3vOkrF8iyM2LJO_Q9rW9bd2ljNBXFoEg/edit?usp=sharing")
papers$Journal <- as.factor(papers$Journal)
papers$Institution <- as.factor(papers$Institution)
papers$Country<- as.factor(papers$Country)
papers$Gender <- as.factor(papers$Gender)
papers$Date_read <- as.Date(papers$Date_read, "%d/%m/%Y")

# Histogram per years
papers %>%
        ggvis(~Year) %>%
        layer_bars() %>%
        add_axis("x",  values = seq(2000, 2016, by = 1),
                 title_offset = 50,
                 properties = axis_props(labels = list(angle = 90, align = "left", 
                                                       baseline="middle"))) 


# Histogram per # authors
papers %>%
    filter(N_authors <= 40) %>%
        ggvis(~N_authors) %>%
        layer_bars() 
        
# Bars for Journals
papers %>%
        ggvis(~Journal) %>%
        layer_bars() %>%
        add_axis("x", title = "Journal", 
                 title_offset = 230,
                 properties = axis_props(labels = list(angle = 90, align = "left",
                                                       baseline="middle")))

# Bars for Institution
papers %>%
        ggvis(~Institution) %>%
        layer_bars() %>%
        add_axis("x", title = "Institution", title_offset = 240,
                 properties = axis_props(labels = list(angle = 90,
                                                       align = "left",
                                                       baseline="middle")))

# Bars for Country
papers %>%
        ggvis(~Country) %>%
        layer_bars() %>%
        add_axis("x", title = "Country",title_offset = 60,
                 properties = axis_props(labels = list(angle = 90,
                                                       align = "left",
                                                       baseline="middle")))

# Bars for Gender
papers %>%
        ggvis(~Gender) %>%
        layer_bars() %>%
        add_axis("x", title = "Gender")

# Bars for Source
papers %>%
      ggvis(~Source) %>%
      layer_bars() %>%
      add_axis("x", title = "Source")

# Cumulative frequency date read
papers %>% 
        ggplot(aes(x =Date_read,y=seq_along(Date_read))) +
        geom_step() +
        scale_x_date(date_breaks = "1 month", date_labels = "%B") +
        ylab("Number of papers") +
        xlab ("Date read") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

