---
title: "Tufts Forest Climate Adaptability"
format: html
editor: visual
---

## Tufts Forest Climate Adaptability

First, I loaded in the necessary packages and files needed to start coding. These included tree inventory data collected by the Fall 2025 Environmental Fieldwork class,

```{r}
install.packages("tidyverse")
install.packages("terra")
install.packages("tidyterra")
install.packages("viridis")

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(viridis)

treedata<-read.csv("Tree Inventory Cleaned copy.csv")
NEdata<-read.csv("NE updated.csv")
benefitdata<-read.csv("benefitstree2.csv")
NEnum<-read.csv("NEnum.csv")

treetotals<-read_csv("Tree Totals.csv")
treetotals2<-read_csv("Tree Totals2.csv")
```

Here, I changed the column names to make it easier to plot.

```{r}
#changing column names 
names(combined)<-c("Species", "Replacement.Value...","Trees","Carbon.Storage..mton.", "Carbon.Storage....", "Gross.Carbon.Sequestration.m.ton.yr.","Gross.Carbon.Sequestration...yr.","Avoided.Runoff.m3.yr.","Avoided.Runoff...yr.","Pollution.Removal.m.ton.yr.","Pollution.Removal...yr.","Scientific.Name","Range","MR","X.Cell","FIAsum","FIAiv","ChngCl45","ChngCl85","Adap","Abund","Capabil45","Capabil85","SHIFT45","SHIFT85","SSO","N")
names(combined)[7] <- "Yearly Gross Carbon Sequestration"
names(combined)[3] <- "Trees"
names(combined)[9] <- "Yearly Avoided Runoff"
names(combined)[11] <- "Yearly Pollution Removal"

names(comb2)[7] <- "Yearly Gross Carbon Sequestration"
names(comb2)[3] <- "Trees"
names(comb2)[9] <- "Yearly Avoided Runoff"
names(comb2)[11] <- "Yearly Pollution Removal"

names(comb3)[7] <- "Yearly Gross Carbon Sequestration"
names(comb3)[3] <- "Trees"
names(comb3)[9] <- "Yearly Avoided Runoff"
names(comb3)[11] <- "Yearly Pollution Removal"

```

Here, I joined the species benefit data with numerical data of the New England data.

```{r}
numcombdata2 <- left_join(benefitdata, NEnum, by = c("Species" = "Common.Name"))%>%
  filter(MR %in% c("High", "Medium"))
```

I combined the New England projection data with the benefits from the various tree species on Tufts campus calculated by I-Tree-Eco to analyze various factors that influence its ability to handle climate change.

```{r}
combineddata <- left_join(benefitdata, NEdata, by = c("Species" = "Common.Name")) %>%
  drop_na() %>%
  filter(MR %in% c("High", "Medium", "Low"))
  
comb2<-filter(combineddata, MR %in% c("High"))
comb3<-filter(combineddata, MR %in% c("Medium"))
adapthigh<-filter(combineddata, Adap %in% c("High"),MR %in% c("High", "Medium"))
```

Comb2 is a subset of only the high reliability of the species’ model predicting current and future suitable habitats. Comb3 is a subset of only the medium reliability of the species’ model predicting current and future suitable habitats. I graphed these two data sets on different graphs at first.

```{r}
ggplot(data=comb2,mapping=aes(x=Adap, fill=Species)) + 
  geom_bar() + labs(x="Adaptability", y="Count",title="Adaptability of Various Native Tree Species")

ggplot(data=comb3,mapping=aes(x=Adap, fill=Species)) + 
  geom_bar() + labs(x="Adaptability", y="Count",title="Adaptability of Various Native Tree Species")
```

Next, to compare them together I combined the data sets together into a data set called **combined**. graphs below visualizes the adaptability of these species that have a high and medium reliability in model predictions. I combine the two to create a graph that shows both at the same time. It is important

```{r}
comb2$ModelReliability <- "Medium"
comb3$ModelReliability <- "High"
combined <- rbind(comb2, comb3)
library(ggplot2)
install.packages("viridis")
library(viridis)
combined$Adap <- factor(
  combined$Adap,
  levels = c("Low", "Medium","High"))

ggplot(combined, aes(x = factor(Adap, levels = c("Low", "Medium", "High")), 
                     y = Species, 
                     color = MR)) +
  geom_point(size = 4) +
  scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako") +
  labs(
    x = "Adaptability Rating",
    y = "Number of Species",
    title = "Tree Species Adaptability by Model Reliability") +
  theme_minimal() +
  guides(color = guide_legend(title = "Model Reliability")) +
  theme(text = element_text(family = "Times New Roman"))
```

The graph below visualizes the capability of these species that have a high reliability in model predictions. The overall estimate of capability for the species to cope with the changing climate within the region.

```{r}
combined$Capabil85 <- factor(
  combined$Capabil85,
  levels = c("Poor", "Fair","Good", "Very Good"))

ggplot(combined, aes(x = factor(Capabil85, levels = c("Poor", "Fair", "Good","Very Good")), 
                     y = Species,
                     color = MR)) +
  geom_point(size = 4) +
  scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako") +
  labs(
    x = "Capability Rating",
    y = "Number of Species",
    title = "Tree Species Capability by Model Reliability") +
  theme_minimal() +
  guides(color = guide_legend(title = "Model Reliability")) +
  theme(text = element_text(family = "Times New Roman"))

```

```{r}
combined$Capabil45 <- factor(
  combined$Capabil45,
  levels = c("Poor", "Fair","Good", "Very Good"))

ggplot(combined, aes(x = factor(Capabil45, levels = c("Poor", "Fair", "Good","Very Good")), 
                     y = Species,
                     color = MR)) +
  geom_point(size = 4) +
  scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako") +
  labs(
    x = "Adaptability Rating",
    y = "Number of Species",
    title = "Tree Species Adaptability by Model Reliability") +
  theme_minimal() +
  guides(color = guide_legend(title = "Model Reliability")) +
  theme(text = element_text(family = "Times New Roman"))

```

The graph below visualizes the species within Comb2's financial value in pollution removal per year.

```{r}
names(comb2)[7] <- "Yearly Gross Carbon Sequestration"
names(comb2)[3] <- "Trees"
names(comb2)[9] <- "Yearly Avoided Runoff"
names(comb2)[11] <- "Yearly Pollution Removal"
comb2$`Yearly Pollution Removal`<- as.numeric(comb2$`Yearly Pollution Removal`)

ggplot(comb2, aes(x = Species, y = `Yearly Pollution Removal`, color = Species)) + geom_point(width = 0.3)+scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako")+labs(y="Pollution Removal ($)", title="Value of Pollution Removal by Native Tufts Tree Species")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(text = element_text(family = "Times New Roman")) 

comb3$`Yearly Pollution Removal`<- as.numeric(comb3$`Yearly Pollution Removal`) 
comb3 <- comb3 %>% drop_na()

ggplot(comb3, aes(x = Species, y = `Yearly Pollution Removal`, color = Species)) + geom_point(width = 0.3)+scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako")+labs(y="Pollution Removal ($)", title="Value of Pollution Removal by Native Tufts Tree Species")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(text = element_text(family = "Times New Roman"))
```

The graph below visualizes the ecosystem services that Tufts native trees who have high model reliability provide. These ecosystem services include the yearly avoided runoff, gross sequestration, and pollution removal.

```{r}

comb2$`Yearly Avoided Runoff`<- as.numeric(comb2$`Yearly Avoided Runoff`)
comb2$`Yearly Gross Carbon Sequestration` <- as.numeric(comb2$`Yearly Gross Carbon Sequestration`)
comb2$`Yearly Pollution Removal`<- as.numeric(comb2$`Yearly Pollution Removal`)
comb2 <- comb2 %>%
  mutate(across(
    c(`Yearly Avoided Runoff`,
      `Yearly Gross Carbon Sequestration`,
      `Yearly Pollution Removal`),
    ~ as.numeric(gsub(",", "", .x))))

long <- comb2 %>%
  pivot_longer(
    cols = c(`Yearly Avoided Runoff`,
             `Yearly Gross Carbon Sequestration`,
             `Yearly Pollution Removal`),
    names_to = "Service",
    values_to = "Value")

ggplot(long, aes(x = Species, y = Value, fill = Service)) +
  geom_col(position = "dodge") +
  labs(
    title = "Ecosystem Services by Species",
    x = "Species",
    y = "Value"
  ) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

```

Facet Wrap

```{r}
combined_long <- combined %>%
  pivot_longer(
    cols = c(Adap, Capabil85, Capabil45),
    names_to = "RatingType",
    values_to = "RatingValue"
  )

# ---- Factor levels for each category ----
combined_long <- combined_long %>%
  mutate(
    RatingValue = case_when(
      RatingType == "Adap" ~ factor(RatingValue, levels = c("Low", "Medium", "High")),
      RatingType == "Capabil85" ~ factor(RatingValue, levels = c("Poor", "Fair", "Good", "Very Good")),
      RatingType == "Capabil45" ~ factor(RatingValue, levels = c("Poor", "Fair", "Good", "Very Good")),
      TRUE ~ RatingValue
    ),
    RatingType = recode(RatingType,
                        "Adap" = "Adaptability",
                        "Capabil45" = "Capability 45",
                        "Capabil85" = "Capability 85") )

# ---- Plot ----
ggplot(combined_long, aes(
  x = RatingValue,
  y = Species,
  color = MR
)) +
  geom_point(size = 4) +
  scale_color_viridis(
    begin = 0.2,
    end = 0.8,
    discrete = TRUE,
    option = "mako"
  ) +
  facet_wrap(~ RatingType, scales = "free_x") +
  labs(
    x = "Rating",
    y = "Number of Species",
    title = "Tree Species Ratings by Model Reliability"
  ) +
  theme_minimal(base_size = 13) +
  guides(color = guide_legend(title = "Model Reliability")) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1),

    # ---- Make facets more visually distinct ----
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "#e6e6e6", color = "black", linewidth = 0.7),
    panel.border = element_rect(color = "gray50", fill = NA, linewidth = 0.7),
    panel.spacing = unit(1.2, "lines")
  )


```

```{r}
numcombdata2 <- combined %>%
  pivot_longer(
    cols = c(Adap, Capabil85, Capabil45),
    names_to = "RatingType",
    values_to = "RatingValue"
  )

# ---- Factor levels for each category ----
combined2_long <- combined_long %>%
  mutate(RatingValue = case_when(RatingType == "Adap" ~ factor(RatingValue, levels = c("Low", "Medium", "High")),
      RatingType == "Capabil85" ~ factor(RatingValue, levels = c("Poor", "Fair", "Good", "Very Good")),
      RatingType == "Capabil45" ~ factor(RatingValue, levels = c("Poor", "Fair", "Good", "Very Good")),
      TRUE ~ RatingValue
    ),
    RatingType = recode(RatingType,
                        "Adap" = "Adaptability",
                        "Capabil45" = "Capability 45",
                        "Capabil85" = "Capability 85")
crit_range <- crit_long %>%
  group_by(Common.Name) %>%
  summarise(
    Tmin = min(T_crit, na.rm = TRUE),
    Tmax = max(T_crit, na.rm = TRUE))
    
ggplot() +geom_segment(data = crit_range,aes(y = Common.Name, yend = Common.Name,x = Tmin, xend = Tmax),
    linewidth = 4,
    color = "21908CFF",
    lineend = "round") +
  geom_point(
    data = crit_long,
    aes(x = T_crit, y = Common.Name, color = Year),
    size = 5) +
  geom_vline(
    aes(linetype = "3 Days in 2070-2099 > 105°F", xintercept = 105),
    linewidth = 0.8,
    color = "gray20") +scale_linetype_manual(
    name = NULL,
    values = c("3 Days in 2070-2099 > 105°F" = "dashed")) +
  scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.7) +
  scale_x_continuous(breaks = seq(100, 125, by = 2), limits = c(103, 118)) +
  scale_y_discrete(expand = expansion(mult = 0.8)) +
  labs(
    title = "Critical Temperatures for Two Tree Species on Tufts Campus\n Compared to Future Temperature Projections",
    x = "Critical Temperature (°F)",
    y = "Species",
    color = "Annual Leaf Critical Temp. Mean") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 11), legend.title = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))

ggsave("critical temp. trees in future.png", width=7, height=4,units="in")
```

filtering species that have low adaptabiltiy and capability

```{r}
#First, I made all column names easily ledgible and clear. I did this to remove the $ sign in the column names. 
names(treetotals2)
names(treetotals2)[2] <- "Total Replacement Value"
names(treetotals2)[3] <- "Carbon Storage"
names(treetotals2)[4] <- "Total Yearly Gross Carbon Sequestration"
names(treetotals2)[5] <- "Total Yearly Avoided Runoff"
names(treetotals2)[6] <- "Total Yearly Pollution Removal"  

# Next, I used pivot longer to sort the columns into Measures (of ecosystem services) and value in dollar amount.
treetotals2_long <- treetotals2 %>%
  pivot_longer(
    cols = -`Potential Losses in Tree Ecosystem Services`,
    names_to = "Measure ($)",
    values_to = "Value")

# I then factored the categories of ecosystem services from largest to smallest. 
treetotals2_long$`Measure ($)`<- factor(treetotals2_long$`Measure ($)`, 
                         levels = c("Total Replacement Value", "Carbon Storage","Total Yearly Pollution Removal", "Total Yearly Avoided Runoff","Total Yearly Gross Carbon Sequestration"))

#next, I plotted using additional labels on the graph to make it clear the value of each bar and added more specific x, y, and title captions. I filled the bar with dark red to signify a loss. 
ggplot(treetotals2_long, aes(x = `Measure ($)`, y = Value)) +
  geom_col(fill="darkred") +
  geom_text(aes(label = round(Value, 2)),      # labels on bars + pushes text slightly to the right
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Potential Losses in Tree Ecosystem Services\n on the Tufts Campus from Climate Change", x = "Measure ($)", y = "Value") +
  theme_minimal() +
  expand_limits(y = max(treetotals2_long$Value) * 1.2) + theme(text = element_text(family = "Times New Roman"),axis.text.y = element_text(angle = 25, hjust = 1))
```

```{r}
names(comb2)[7] <- "Yearly Gross Carbon Sequestration"
names(comb2)[3] <- "Trees"
names(comb2)[9] <- "Yearly Avoided Runoff"
names(comb2)[11] <- "Yearly Pollution Removal"
comb2$`Yearly Pollution Removal`<- as.numeric(comb2$`Yearly Pollution Removal`)

ggplot(comb2, aes(x = Species, y = `Yearly Pollution Removal`, color = Species)) + geom_point(width = 0.3)+scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako")+labs(y="Pollution Removal ($)", title="Value of Pollution Removal by Native Tufts Tree Species")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(text = element_text(family = "Times New Roman")) 

names(comb3)[7] <- "Yearly Gross Carbon Sequestration"
names(comb3)[3] <- "Trees"
names(comb3)[9] <- "Yearly Avoided Runoff"
names(comb3)[11] <- "Yearly Pollution Removal"
comb3$`Yearly Pollution Removal`<- as.numeric(comb3$`Yearly Pollution Removal`) 
comb3 <- comb3 %>% drop_na()

ggplot(comb3, aes(x = Species, y = `Yearly Pollution Removal`, color = Species)) + geom_point(width = 0.3)+scale_color_viridis(begin = 0.2, end = 0.8, discrete = TRUE, option="mako")+labs(y="Pollution Removal ($)", title="Value of Pollution Removal by Native Tufts Tree Species")+theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(text = element_text(family = "Times New Roman"))
```
