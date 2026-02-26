#work on barley trail
library(tidyverse)
library(readxl)
library(RColorBrewer)
setwd("C:/Users/hrlexd/OneDrive - Plant and Food Research/Beneficial_insects/Aphid_work_2026")

data<-read.table('Aphid data_2025_2026_data_clean_ejd.txt',sep='\t',header=T)
metadata<-read.table('Aphid data_2025_2026_metadata_ejd.txt',sep='\t',header=T)

#just looking at trail one
metadata_treat1<-metadata %>% filter(Tray1_barley.treatment==1)

data_treat1<-data %>% filter(SampleName %in% metadata_treat1$Samplename)


#aphid counts through time

data_treat1_aphids<-data_treat1 %>% mutate(sum_counts=rowSums(across(where(is.numeric)))) %>% filter(Insect=='Wingless aphids')
data_treat1_aphids<-left_join(data_treat1_aphids,metadata_treat1,by=c('SampleName'='Samplename'))

ggplot(data_treat1_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
theme_bw()+
  scale_fill_manual(values=c('red','orange'))+
  labs(x = "Date",
       y = "Aphid counts (wingless)",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

#winged aphids

data_treat1_aphids<-data_treat1 %>% mutate(sum_counts=rowSums(across(where(is.numeric)))) %>% filter(Insect=='Winged aphids')
data_treat1_aphids<-left_join(data_treat1_aphids,metadata_treat1,by=c('SampleName'='Samplename'))

ggplot(data_treat1_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=c('red','orange'))+
  labs(x = "Date",
       y = "Aphid counts (winged)",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))


#lacewings
data_treat1_aphids<-data_treat1 %>% mutate(sum_counts=rowSums(across(where(is.numeric)))) %>% filter(Insect=='Lacewing adults')
data_treat1_aphids<-left_join(data_treat1_aphids,metadata_treat1,by=c('SampleName'='Samplename'))

ggplot(data_treat1_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=c('red','orange'))+
  labs(x = "Date",
       y = "Lacewing adults",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

#lacewings
data_treat1_aphids<-data_treat1 %>% mutate(sum_counts=rowSums(across(where(is.numeric)))) %>% filter(Insect=='Lacewing larvae')
data_treat1_aphids<-left_join(data_treat1_aphids,metadata_treat1,by=c('SampleName'='Samplename'))

ggplot(data_treat1_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=c('red','orange'))+
  labs(x = "Date",
       y = "Lacewing larvae",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))


#alright having a look at trail 2
metadata_treat2<-metadata %>% filter(Tray1_barley.treatment==2|Tray1_barley.treatment==3)

data_treat2<-data %>% filter(SampleName %in% metadata_treat2$Samplename)


#aphid counts through time

data_treat2_aphids<-data_treat2 %>% mutate(sum_counts=rowSums(across(where(is.numeric)),na.rm = TRUE)) %>% filter(Insect=='Wingless aphids')
data_treat2_aphids<-left_join(data_treat2_aphids,metadata_treat2,by=c('SampleName'='Samplename'))

my_palette = c( 'red',"#08306B","#08519C","#2171B5","#4292C6","#6BAED6","#9ECAE1","#C6DBEF","#DEEBF7","#F7FBFF")

data_treat2_aphids$Date <- factor(data_treat2_aphids$Date,levels=c('1/19/2026','1/21/2026','1/23/2026','1/26/2026','1/28/2026','1/30/2026','2/2/2026','2/4/2026','2/11/2026','2/19/2026'),ordered = T)

ggplot(data_treat2_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=my_palette)+
  labs(x = "Date",
       y = "Aphid counts (wingless)",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

#winged aphids

data_treat2_aphids<-data_treat2 %>% mutate(sum_counts=rowSums(across(where(is.numeric)),na.rm = TRUE)) %>% filter(Insect=='Winged aphids')
data_treat2_aphids<-left_join(data_treat2_aphids,metadata_treat2,by=c('SampleName'='Samplename'))

data_treat2_aphids$Date <- factor(data_treat2_aphids$Date,levels=c('1/19/2026','1/21/2026','1/23/2026','1/26/2026','1/28/2026','1/30/2026','2/2/2026','2/4/2026','2/11/2026','2/19/2026'),ordered = T)

ggplot(data_treat2_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=my_palette)+
  labs(x = "Date",
       y = "Aphid counts (winged)",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))


#lacewings
data_treat2_aphids<-data_treat2 %>% mutate(sum_counts=rowSums(across(where(is.numeric)),na.rm = TRUE)) %>% filter(Insect=='Lacewing adults')
data_treat2_aphids<-left_join(data_treat2_aphids,metadata_treat2,by=c('SampleName'='Samplename'))

data_treat2_aphids$Date <- factor(data_treat2_aphids$Date,levels=c('1/19/2026','1/21/2026','1/23/2026','1/26/2026','1/28/2026','1/30/2026','2/2/2026','2/4/2026','2/11/2026','2/19/2026'),ordered = T)

ggplot(data_treat2_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=my_palette)+
  labs(x = "Date",
       y = "Lacewing adults",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

#lacewings
data_treat2_aphids<-data_treat2 %>% mutate(sum_counts=rowSums(across(where(is.numeric)),na.rm = TRUE)) %>% filter(Insect=='Lacewing larvae')
data_treat2_aphids<-left_join(data_treat2_aphids,metadata_treat2,by=c('SampleName'='Samplename'))

data_treat2_aphids$Date <- factor(data_treat2_aphids$Date,levels=c('1/19/2026','1/21/2026','1/23/2026','1/26/2026','1/28/2026','1/30/2026','2/2/2026','2/4/2026','2/11/2026','2/19/2026'),ordered = T)

ggplot(data_treat2_aphids,aes(x=Date,y=sum_counts,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=my_palette)+
  labs(x = "Date",
       y = "Lacewing larvae",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

#diversity
library(vegan)
#looking at natural enemy diversity 

counts_wide<-data_treat2 %>% filter(!Insect=='Wingless aphids') %>% filter(!Insect=='Winged aphids') %>% filter(!Insect=='Thrips') %>% filter(!Insect=='Slugs') %>% filter(!Insect=='Spidermite') %>%  mutate(sum_counts=rowSums(across(where(is.numeric)),na.rm = TRUE)) %>% select(Insect, SampleName,sum_counts) %>% pivot_wider(names_from=Insect,values_from=sum_counts)%>% remove_rownames %>% column_to_rownames(var="SampleName")

sppr<-specnumber(counts_wide)

head(sppr)

sppr_df <- sppr %>% 
  enframe() %>% 
  inner_join(metadata_treat2, by = c("name" = "Samplename"))

ggplot(sppr_df,aes(x=Date,y=value,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=my_palette)+
  labs(x = "Date",
       y = "Natural enemy counts",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

#look at aphid count per tiller

#aphid counts through time

data_treat2_aphids<-data_treat2 %>% mutate(sum_counts=rowSums(across(where(is.numeric)),na.rm = TRUE)) %>% filter(Insect=='Wingless aphids')
metadata_treat2<-metadata_treat2 %>% mutate(tiller_sum = rowSums(across(starts_with("Tillercount_")), na.rm = T))
data_treat2_aphids<-left_join(data_treat2_aphids,metadata_treat2,by=c('SampleName'='Samplename'))

data_treat2_aphids$Date <- factor(data_treat2_aphids$Date,levels=c('1/19/2026','1/21/2026','1/23/2026','1/26/2026','1/28/2026','1/30/2026','2/2/2026','2/4/2026','2/11/2026','2/19/2026'),ordered = T)

data_treat2_aphids$aphid_per_tiller<-data_treat2_aphids$sum_counts/data_treat2_aphids$tiller_sum

ggplot(data_treat2_aphids,aes(x=Date,y=aphid_per_tiller,fill=Date))+
  geom_boxplot()+
  facet_wrap(~Treatment)+
  theme_bw()+
  scale_fill_manual(values=my_palette)+
  labs(x = "Date",
       y = "Aphid counts (wingless) per tiller",
       title = "Counts by boundary",
       fill = "Date")+
  theme(axis.text.x = element_text(angle = 90,size=7,hjust=1))

