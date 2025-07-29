data = read.delim(rawpath, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))

data1 = data %>%
  filter(PROTOCOL.TYPE == "Traveling") %>%
  mutate(speed = (EFFORT.DISTANCE.KM*60)/DURATION.MINUTES) %>%
  filter(speed >= 80) %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup

rev = read.csv("reviewers.csv")

head(rev)

rev = rev %>%
  filter(REGION_CODE != "IN")

names(rev)[7] = "STATE.CODE"

rev1 = rev %>%
  select(3,5,6,7)
rev2 = rev %>%
  select(3,5,6,8)

data1 = data1 %>%
  select(16,17,31,48)

data2 = left_join(rev2,data1)
data3 = left_join(rev1,data1)

table(data3$COUNTY)
length(unique(data4$SAMPLING.EVENT.IDENTIFIER))

data4 = rbind(data2,data3)

data4 = data4 %>%
  distinct(EMAIL,FIRST_NAME,REVIEWER,STATE.CODE,COUNTY,SAMPLING.EVENT.IDENTIFIER,speed)

data4 = data4 %>%
  filter(!is.na(SAMPLING.EVENT.IDENTIFIER))

data5 = data4 %>%
  mutate(name = paste(FIRST_NAME,REVIEWER))

data5 = data5 %>%
  select(1,4,5,6,7,8)

head(data5)
unique(data5$name)

data5 = data5 %>%
  arrange(SAMPLING.EVENT.IDENTIFIER)

head(data5)

write.csv(data5,"incorrect.csv")
