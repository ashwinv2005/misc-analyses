library(tidyverse)
library(extrafont)
font_import()
library(Hmisc)

data = read.csv("aparajita_home_data.csv")
data = data %>%
  mutate(Date = as.character(Date)) %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "-") %>%
  mutate(Day = as.numeric(Day),
         CYear = as.numeric(Year)+2000,
         Time = as.POSIXct(Time, format = "%I:%M %p"))

data$Month = factor(data$Month, levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                           "Jul","Aug","Sep","Oct","Nov","Dec"))
         
         
monthly_sampling_stats = data %>%
  group_by(CYear,Month) %>% 
  reframe(n_lists = n_distinct(Submission.ID),
          mean_time_of_day = format(mean(Time),"%I:%M %p"),
          sd_time_of_day = sd(Time)/60)


## converting to migratory year and assigning seasons    
         
data = data %>%
  # from June to May)
  mutate(Year = ifelse(!Month %in% c("Jan","Feb","Mar","Apr","May"), CYear, CYear-1)) %>%
  mutate(Season = case_when(Month %in% c("Dec","Jan","Feb") ~ "Winter",
                           Month %in% c("Mar","Apr","May") ~ "Summer",
                           Month %in% c("Jun","Jul","Aug") ~ "Monsoon",
                           Month %in% c("Sep","Oct","Nov") ~ "Autumn"))

data$Season = factor(data$Season, levels = c("Monsoon","Autumn","Winter","Summer"))

seasonal_sampling_stats = data %>%
  group_by(Year,Season) %>% 
  reframe(n_lists = n_distinct(Submission.ID),
          mean_duration = mean(Duration.Min),
          sd_duration = sd(Duration.Min),
          mean_time_of_day = format(mean(Time),"%I:%M %p"),
          sd_time_of_day = sd(Time)/60) %>%
  arrange(Season,Year)

## restricting the analysis to seasons with 10 or more checklists

data_to_retain = seasonal_sampling_stats %>%
  filter(n_lists >= 10)

## two years 14 and 15 have migher mean durations of the lists but let's retain those for now

data_analysis = data %>%
  semi_join(data_to_retain %>% distinct(Year,Season))




## plotting species richness per checklist
  
richness_per_checklist = data_analysis %>%
  group_by(Year,Season,Submission.ID) %>% reframe(rich = n_distinct(Common.Name)) %>%
  group_by(Year,Season) %>% reframe(mean_rich = mean(rich),
                                    se_rich = sd(rich)/sqrt(n())) %>%
  mutate(cil = mean_rich - 1.96*se_rich,
         ciu = mean_rich + 1.96*se_rich) %>%
  arrange(Season,Year)

ggp = ggplot(richness_per_checklist, aes(x=Year, y=mean_rich)) +
  facet_wrap(.~Season) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), linewidth = 0.3, width = 0.1) +
  xlab("Year") +
  ylab("Bird species per checklist")

ggp1 = ggp +
  scale_y_continuous(breaks = seq(0,20,4), limits = c(0,20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = -15)),
        #axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_text(size = 16),
        axis.title.y=element_text(size = 16, vjust = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(strip.text.x = element_text(size = 14))+
  guides(fill = guide_legend(nrow = 1))

n1 = paste("Species_per_checklist.jpg",sep="")
print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)



## plotting frequency of any single species in a single season

species = "White-cheeked Barbet"
season = "Summer"
n1 = paste(species,season,sep="_")
n1 = paste(n1,".jpg",sep="")
n2 = paste(species,season,sep=": ")


frequency = data_analysis %>%
  filter(Season == season) %>%
  group_by(Year,Season) %>% mutate(n_lists = n_distinct(Submission.ID)) %>%
  filter(Common.Name == species) %>%
  group_by(Year) %>% reframe(freq = n()/max(n_lists),
                             cil = binconf(n(),max(n_lists))[2],
                             ciu = binconf(n(),max(n_lists))[3])


ggp = ggplot(frequency, aes(x=Year, y=freq)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = cil, ymax = ciu), linewidth = 0.3, width = 0.1) +
  xlab("Year") +
  ylab("Reporting frequency")

ggp1 = ggp +
  ggtitle(n2) +
  scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line.x=element_blank(),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 14, margin = margin(r = -15)),
        #axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_text(size = 16),
        axis.title.y=element_text(size = 16, vjust = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0.5), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.background = element_blank())+
  theme(strip.text.x = element_text(size = 14))+
  guides(fill = guide_legend(nrow = 1))

print(ggp1)
ggsave(file=n1, units="in", width=10, height=7)



