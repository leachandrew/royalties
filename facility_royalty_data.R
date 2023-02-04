library(janitor)
library(scales)
library(tidyverse)
library(readxl)
library(openxlsx)
library(viridis)



get_data<-function(){
#load plant data
  
  os_data_2021 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2021 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  
  os_data_2020 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2020 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  
  os_data_2019 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2019 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  os_data_2018 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2018 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

os_data_2017 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2017 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
os_data_2016 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2016 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
os_data<-rbind(os_data_2021,os_data_2020,os_data_2019,os_data_2018,os_data_2017,os_data_2016)


os_data$royalty_bbl<-os_data$`Royalty.Payable.($)`/os_data$`Cleaned.Crude.Bitumen.at.RCP.(barrels)`
os_data$op_costs_bbl<-os_data$`Operating.Costs.($)`/os_data$`Cleaned.Crude.Bitumen.at.RCP.(barrels)`
os_data$cap_costs_bbl<-os_data$`Capital.Costs.($)`/os_data$`Cleaned.Crude.Bitumen.at.RCP.(barrels)`

os_data <- os_data %>% mutate(
  Project.Name=gsub(" Project","",Project.Name),
  Project.Name=gsub("Christina Lake Regional","Christina Lake (MEG)",Project.Name),
  Project.Name=gsub("Christina Lake Thermal","Christina Lake (CVE)",Project.Name),
  Project.Name=gsub("MacKay River Commercial","PetroChina",Project.Name),
  Project.Name=gsub("MacKay River","MacKay River (Suncor)",Project.Name),
  Project.Name=gsub("PetroChina","MacKay River (PetroChina)",Project.Name),
  Project.Name=gsub(" Thermal","",Project.Name),
  Project.Name=gsub(" Mine","",Project.Name),
  Project.Name=gsub(" Oil Sands","",Project.Name),
  Project.Name=gsub(" EOR","",Project.Name),
  Project.Name=gsub(" Commercial","",Project.Name),
  Project.Name=gsub(" SAGD","",Project.Name),
  Project.Name=gsub(" Demonstration","",Project.Name),
  Project.Name=gsub(" In-Situ","",Project.Name),
)

os_data$Project.Name<-as.factor(os_data$Project.Name)
os_data$Project.Name<-factor(os_data$Project.Name,levels=rev(levels(os_data$Project.Name)))
os_data$op_profit<-os_data$`Gross.Revenue.($/bbl)`-os_data$op_costs_bbl-os_data$royalty_bbl
return(os_data)
}

os_data<-get_data()

big_projects<-function(os_data_sent,threshold){
  big_projects<-os_data_sent %>% filter(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`>threshold*365) %>% select(Project.Name) %>% unique()
  filter(os_data_sent,Project.Name %in% big_projects$Project.Name)
}

p<-ggplot(filter(os_data,os_data$`Cleaned.Crude.Bitumen.at.RCP.(barrels)`>10000*365) ,aes(Project.Name,royalty_bbl,colour=royalty_bbl,fill=royalty_bbl),alpha=0.5)+
  geom_col(aes(Project.Name,royalty_bbl),size=.5,position = position_dodge(width = .5),color="black")+
  #scale_color_viridis("Royalties Paid\nPer Barrel\nCleaned Bitumen")+
  #scale_fill_viridis(discrete=FALSE,"Royalties Paid ($/bbl)")+
  coord_flip()+
  facet_wrap(~Reporting.Year)+
  #scale_x_reverse()+
  weekly_graphs()+
  labs(x=NULL,y=NULL,
       title="Alberta Royalties Paid by Oil Sands Project",
       #subtitle="Excluding Electricity,by NAICS 4-Digit Code",
       caption="Source: Alberta Government Data\nGraph by @andrew_leach")
p
ggsave("royalties_bbl.png",bg="white")


mines<-c("Muskeg River","Fort Hills","Kearl","Horizon","Muskeg River","Jackpine","Syncrude","Suncor")

SAGD<-c("Hangingstone","Leismer","Blackrod","Mackay River","Kirby","Christina Lake","Foster Creek",
"Long Lake","Great Divide","Surmont","Jackfish","Sunrise","Orion","Firebag","MacKay River",
"West Ells","SAGD","Tucker","Narrows Lake")





big_projects<-os_data %>% group_by(Project.Name) %>% mutate(max_bbls=max(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`),
                                                            min_bbls=min(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`))%>%
  filter(min_bbls>30000*365) %>%
  filter(max_bbls<400000*365)%>% select(Project.Name) %>% unique()

med_SAGD<-os_data %>% filter(Project.Name %in% big_projects$Project.Name) %>% filter(!Project.Name %in% mines)

unique(med_SAGD$Project.Name)

bigger_projects<-os_data %>% filter(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`>25000*365) %>% select(Project.Name) %>% unique()

mine_projects<-os_data %>% filter(Project.Name %in% mines)

#SAGD_matches <- data.frame(test_data$Project.Name[grep(paste(SAGD,collapse="|"), 
#                                                  test_data$Project.Name)])

big_SAGD<-os_data %>% filter(Project.Name %in% bigger_projects$Project.Name) %>% filter(!Project.Name %in% mines)

small_SAGD<-os_data %>% filter(!Project.Name %in% bigger_projects$Project.Name) %>% filter(!Project.Name %in% mines)



test_data <- os_data %>% mutate(SAGD_indicator=Project.Name %in% SAGD) %>%
  select(Project.Name,SAGD_indicator)



non_matches <- data.frame(test_data$Project.Name[-grep(paste(SAGD,collapse="|"), 
                                                  test_data$Project.Name)])

big_SAGD %>% group_by(Reporting.Year) %>% summarize(op_costs_bbl=sum(`Operating.Costs.($)`)/sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`))

mine_projects %>% group_by(Reporting.Year) %>% summarize(op_costs_bbl=sum(`Operating.Costs.($)`)/sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`))

big_SAGD %>% group_by(Reporting.Year) %>% summarize(cap_costs_bbl=sum(`Capital.Costs.($)`)/sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`))
mine_projects %>% group_by(Reporting.Year) %>% summarize(cap_costs_bbl=sum(`Capital.Costs.($)`)/sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`))

big_SAGD <-big_SAGD %>% group_by(Project) %>% mutate(higher_2018=sum(op_costs_bbl*(Reporting.Year==2018))>sum(op_costs_bbl*(Reporting.Year==2017))) 


big_SAGD %>% filter(Reporting.Year==2018) %>% group_by(higher_2018) %>% summarise(barrels=sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`,na.rm=T))

  

costs<-os_data %>% group_by(Reporting.Year) %>% summarize(avg_costs=sum(`Operating.Costs.($)`/sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`)),
                                                          avg_rev=sum(`Project.Revenue.($)`,na.rm = T)/sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`,na.rm = T))


#df1<- df1[df1$NAICS4!=2211,]
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("op_profit_SAGD.png")
p<-ggplot(big_SAGD)+
  #geom_col(aes(Project.Name,op_profit,group=Reporting.Year,fill = factor(Reporting.Year,levels = c(2018,2017,2016))),size=.5,position = position_dodge(width = 1))+
  geom_line(aes(Reporting.Year,op_profit),size=1.5)+
  facet_wrap(~Project.Name,nrow = 3)+
  #scale_fill_manual()+
  #scale_x_reverse()+
  #coord_flip()+
  scale_fill_viridis("Reporting Year",discrete = T)+
  guides(colour=FALSE,fill=guide_legend())+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 10,angle=0)
    #axis.text = element_blank()
  )+
  labs(y="Operating Costs ($/bbl)",x=NULL,
       title="2016-2019 Operating Costs by Oil Sands In Situ Project",
       subtitle="For Projects with production above 25k bbl/d in any year",
       caption="Source: Alberta Government Data\nGraph by @andrew_leach")

print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


big_SAGD$Reporting.Year<-as_factor(big_SAGD$Reporting.Year)
#df1<- df1[df1$NAICS4!=2211,]
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("op_costs_bbl_SAGD.png",width=1600)
p<-ggplot(big_SAGD)+
  geom_col(aes(factor(Reporting.Year,levels = c(2016,2017,2018,2019,2020)),op_costs_bbl),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
  facet_wrap(~Project.Name,nrow = 3)+
  #scale_fill_manual()+
  #scale_x_reverse()+
  #coord_flip()+
  #scale_fill_viridis("Reporting Year",discrete = T)+
  guides(colour=FALSE,fill=FALSE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 8, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 10,face = "bold"),
    axis.text = element_text(size = 10,angle=0,hjust = 1)
    #axis.text = element_blank()
  )+
  labs(y="Operating Costs ($/bbl)",x=NULL,
       title="2016-2018 Operating Costs by Oil Sands In Situ Project",
       subtitle="Projects with production above 25k bbl/d in any year",
       caption="Source: Alberta Government data, graph by @andrew_leach")

print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

med_SAGD<-med_SAGD %>% group_by(Project.Name)%>%
  mutate(prod_2019=max(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`*(Reporting.Year==2019)/365))%>%
  mutate(label=paste(Project.Name,"\n(",formatC(round(prod_2019,digits=-3),format="f", big.mark=",",digits = 0)," bbl/d)",sep=""))
  
#set_png("op_costs_bbl_SAGD.png",width=1600)


p<-ggplot(med_SAGD)+
  geom_col(aes(factor(Reporting.Year,levels = c(2016,2017,2018,2019)),op_costs_bbl+cap_costs_bbl),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
  geom_hline(aes(yintercept = 32.65*1.26,colour="Current $CA bitumen value"))+
  facet_wrap(~label,nrow = 3)+
  scale_colour_manual("",values="black")+
  #scale_x_reverse()+
  #coord_flip()+
  #scale_fill_viridis("Reporting Year",discrete = T)+
  guides(colour=guide_legend(),fill=FALSE)+
  work_theme()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    #legend.text = element_text(colour="black", size = 10, face = "bold"),
    #plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 18,face = "bold"),
    #axis.text = element_text(size = 10,angle=0,hjust = 1),
    #axis.text.x = element_text(size = 10,hjust=0.5),
    plot.caption = element_blank()
  )+
  labs(y="Operating and Sustaining Capital Costs ($/bbl)",x=NULL,
       title="2016-2019 Operating and Sustaining Capital Costs by Oil Sands In Situ Project",
       subtitle="Projects with production above 40,000 bbl/d in every year. 2019 production, rounded to the nearest thousand barrels per day, shown in brackets.",
       caption="Source: Alberta Government data, graph by @andrew_leach")
print(p)
ggsave("op_costs_bbl_SAGD.png",dpi=600,width=24,height=16)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("op_costs_bbl_mines.png")
p<-ggplot(filter(mine_projects,Project.Name!="Fort Hills Oil Sands Project"))+
  geom_col(aes(Reporting.Year,op_costs_bbl,group=Reporting.Year),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
  facet_wrap(~Project.Name)+
  #scale_fill_manual()+
  #scale_x_reverse()+
  #coord_flip()+
  #scale_fill_maual()+
  guides(colour=FALSE,fill=FALSE)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 10,angle=0),
    #axis.text = element_blank()
  )+
  labs(y="Operating Costs ($/bbl)",x=NULL,
       title="2016-2018 Operating Costs by Oil Sands Mining Project",
       subtitle="For Projects with 2016 Production above 10k bbl/d.\nIncludes only bitumen production costs, not upgrading.",
       caption="Source: Alberta Government data, graph by @andrew_leach")

print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


project_data <-big_projects(os_data,10000) %>% group_by(Reporting.Year) %>% 
  mutate(year_total=sum(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`), weight=`Cleaned.Crude.Bitumen.at.RCP.(barrels)`/year_total) %>%
  ungroup()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("op_costs_density.png")
g <- ggplot(project_data, aes(op_costs_bbl,group=factor(Reporting.Year),weights=weight))+
  stat_density(aes(color=factor(Reporting.Year)),geom="line",position = "identity",trim=T,size=1.6)+
  scale_colour_manual(NULL,values=colors_tableau10())+
  #geom_density(aes(color=factor(Reporting.Year)), alpha=0.8) + 
  labs(title="Density plot of oil sands operating costs per barrel bitumen", 
       subtitle="Production-weighted, for projects with more than 10,000 barrels per day of bitumen production",
       caption="Source: Government of Alberta 2016 and 2017 Royalty Data, graph by Andrew Leach",
       x="Operating Costs ($Cdn/bbl bitumen)",
       fill="Reporting Year")+
     weekly_graphs()
print(g)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

ggplot(subset(os_data,os_data$`Cleaned.Crude.Bitumen.at.RCP.(barrels)`>10000*365), aes(op_costs_bbl), ) +
  geom_col(aes(Project.Name,op_costs_bbl,group=Reporting.Year,fill = factor(Reporting.Year)),size=.5,position = position_dodge(width = 1))+
  #scale_fill_manual()+
  #scale_x_reverse()+
  coord_flip()


#df1<- df1[df1$NAICS4!=2211,]

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("net_rev_bbl.png")

projects<-os_data%>%filter(`Cleaned.Crude.Bitumen.at.RCP.(barrels)`>=10000*365)%>%select(Project.Name)%>%distinct()%>%
  mutate(name=as.character(Project.Name))
p<-
  ggplot(os_data%>%filter(Project.Name %in% projects$name))+
  geom_col(aes(as.factor(Reporting.Year),op_profit,fill=op_profit>0),size=.5,position = position_dodge())+
  scale_fill_manual("",values=c("FALSE"="red","TRUE"="darkgreen"),labels=c("Operating Loss","Operating Profit"))+
  scale_x_discrete(breaks=pretty_breaks(n=5))+
  facet_wrap(~Project.Name)+
  #scale_x_reverse()+
  #coord_flip()+
  guides(fill=guide_legend())+
  weekly_graphs()+
  theme(axis.text.x = element_text(size = 10,angle=90,vjust=0.5),
        strip.text = element_text(size = 10))+
  labs(y="Operating Profit or Loss ($/bbl)",x=NULL,
       title="Operating Profit by Oil Sands Project",
       subtitle="Gross bitumen revenue net operating costs and royalties for projects with at least one year in 2016-2021 with production above 10k bbl/d",
       caption="Source: Alberta Government data, graph by @andrew_leach")
print(p)

p<-
  ggplot(os_data%>%filter(Project.Name %in% projects$name)%>%
           mutate(payout=as.factor(Payout.Status),
                  payout=fct_relevel(payout,"PRE"),
                  Project.Name=fct_rev(Project.Name)))+
  geom_col(aes(as.factor(Reporting.Year),`Royalty.Payable.($)`/`Cleaned.Crude.Bitumen.at.RCP.(barrels)`,fill=payout),size=.5,position = position_dodge())+
  scale_fill_manual("",values=c("PRE"="red","POST"="darkgreen"),labels=c("Pre-Payout","Post-Payout"))+
  scale_x_discrete(breaks=pretty_breaks(n=5))+
  facet_wrap(~Project.Name,nrow = 5)+
  #scale_x_reverse()+
  #coord_flip()+
  guides(fill=guide_legend())+
  weekly_graphs()+
  theme(axis.text.x = element_text(size = 10,angle=90,vjust=0.5),
        strip.text = element_text(size = 8))+
  labs(y="Royalties Payable ($/bbl)",x=NULL,
       title="Royalties Paid by Oil Sands Project",
       subtitle="Royalties payable for projects with at least one year in 2016-2021 with production above 10k bbl/d",
       caption="Source: Alberta Government data, graph by @andrew_leach")
print(p)
ggsave("royalties.png",dpi=300,bg="white",width=16, height=10)


mine_projects<-mine_projects%>%clean_names()
p<-
  ggplot(mine_projects%>%ungroup()%>%
           mutate(
             project_name=factor(project_name),
             project_name=fct_relevel(project_name,"Suncor"),
             project_name=fct_relevel(project_name,"Kearl",after = Inf),
             project_name=fct_relevel(project_name,"Fort Hills",after = Inf),
             project_name=fct_relevel(project_name,"Horizon",after = 2),
             mfsp=(gross_revenue-operating_costs
                   #-other_costs
                   +other_net_proceeds-royalty_payable)
                  /cleaned_crude_bitumen_at_rcp_barrels,
         mfsp_col = ifelse(mfsp >= 0, "darkgreen", "red"))%>%
           filter(!is.infinite(mfsp)))+
  geom_col(aes(as.factor(reporting_year),mfsp,fill=mfsp_col),size=.5,position = position_dodge())+
  #scale_fill_manual("",values=c("PRE"="red","POST"="darkgreen"),labels=c("Pre-Payout","Post-Payout"))+
  scale_x_discrete(breaks=pretty_breaks(n=5))+
  scale_y_continuous(breaks=pretty_breaks(n=10),expand=c(0,0))+
  scale_fill_identity()+
  facet_wrap(~project_name,nrow = 1)+
  #scale_x_reverse()+
  #coord_flip()+
  guides(fill=guide_legend())+
  weekly_graphs()+
  theme(axis.text.x = element_text(size = 10,angle=90,vjust=0.5),
        strip.text = element_text(size = 8))+
  labs(y="MFSP Netback ($/bbl)",x=NULL,
       #title="Royalties Paid by Oil Sands Project",
       #subtitle="Royalties payable for projects with at least one year in 2016-2021 with production above 10k bbl/d",
       caption="Source: Alberta Government data, graph by @andrew_leach",
       NULL)
print(p)
ggsave("mfsp_netback.png",dpi=300,bg="white",width=16, height=7)

print(p+labs(y="Revenue net Opex and Royalties ($/bbl)"))
ggsave("mine_netback.png",dpi=300,bg="white",width=16, height=7)

mine_projects%>%
  rename(bitumen_prod=cleaned_crude_bitumen_at_rcp_barrels)%>%
  mutate(
    project_name=factor(project_name),
    project_name=fct_relevel(project_name,"Suncor"),
    project_name=fct_relevel(project_name,"Kearl",after = Inf),
    project_name=fct_relevel(project_name,"Fort Hills",after = Inf),
    project_name=fct_relevel(project_name,"Horizon",after = 2),
    mfsp=(gross_revenue-operating_costs
          #-other_costs
          +other_net_proceeds-royalty_payable)
    /bitumen_prod)%>%
  filter(reporting_year==2021)%>%
  summarize(mfsp=sum(mfsp*bitumen_prod)/sum(bitumen_prod),
            op_cost=sum(operating_costs)/sum(bitumen_prod),
            revenue=sum(gross_revenue_bbl*bitumen_prod)/sum(bitumen_prod),
            royalties=sum(royalty_payable)/sum(bitumen_prod),
            )

sproule_data<-read_xlsx("sproule_2022.xlsx",sheet = "North American Oil",range = "B8:P22")%>%
  slice(-c(1)) %>% 
  clean_names()%>%
  mutate(year=gsub(" Act","",year))

names(sproule_data)<-gsub("x1_","",names(sproule_data))
names(sproule_data)<-gsub("x2_","",names(sproule_data))
names(sproule_data)<-gsub("x3_","",names(sproule_data))

mine_projects<-mine_projects %>% left_join(sproule_data %>% select(year,wti_cushing_oklahoma_us_bbl,exchange_rate_cad_usd)%>%
                              mutate(year=as.numeric(year)),by=c("reporting_year"="year"))

mine_projects<-mine_projects %>% rename(wti=wti_cushing_oklahoma_us_bbl,
                                                        cad_usd=exchange_rate_cad_usd)%>%
  rename(bitumen_prod=cleaned_crude_bitumen_at_rcp_barrels)%>%
  mutate(mfsp=(gross_revenue-operating_costs
               #-other_costs
               +other_net_proceeds-royalty_payable)
         /bitumen_prod)


model_data <-mine_projects %>% filter(!is.na(mfsp),!is.infinite(mfsp))
lm(mfsp ~ wti+factor(reporting_year)+factor(payout_status), data = model_data)

lm(mfsp ~ wti, data = model_data)


pre_pay<-subset(os_data,os_data$`Unrecovered.Balance/Net.Loss.at.EOP.($)`>0)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("remaining_recov.png")
p<-ggplot(subset(os_data,`Cleaned.Crude.Bitumen.at.RCP.(barrels)`>50000*365& `Unrecovered.Balance/Net.Loss.at.EOP.($)`>0))+
  geom_col(aes(factor(Reporting.Year,levels = c(2016,2017,2018)),`Unrecovered.Balance/Net.Loss.at.EOP.($)`/10^9,fill=Payout.Status,colour=Payout.Status),size=.5,position = position_dodge(width = .5))+
  #geom_col(aes(Project.Name,op_costs_bbl),size=.5,position = position_dodge(width = .5),fill="firebrick",colour="firebrick",alpha=1)+
  #scale_color_viridis("Operating Costs\nPer Barrel\nCleaned Bitumen")+
  scale_fill_manual("Royalty Status",values=c("PRE"="red","POST"="darkgreen"),labels=c("Post-Payout","Pre-Payout"))+
  scale_colour_manual("",values=c("PRE"="red","POST"="darkgreen"))+
  facet_wrap(~Project.Name,nrow = 3)+
  #scale_x_reverse()+
  #coord_flip()+
  guides(colour=FALSE,fill=guide_legend())+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12,angle=0,hjust=.5,vjust=.5)
    #axis.text = element_blank()
  )+
  labs(y="Unrecovered expenditures ($ billions)",x=NULL,
       title="Unrecovered Oil Sands Capital Expenditures, 2016-2018",
       subtitle="Projects with Production Greater than 50k bbl/d",
       caption="Source: Alberta Government Data\nGraph by @andrew_leach")

print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

}


os_data<-get_data()
graphs()

mines<-c("Muskeg River Mine","Fort Hills Oil Sands Project","Kearl","Horizon Mine","Muskeg River Mine","Jackpine Mine","Syncrude Mine","Suncor Oil Sands")
mine_projects<-os_data %>% filter(Project.Name %in% mines) %>% select(Project.Name) %>% unique()


#GHG data

#load plant data
os_ghg_data <- read_excel(path = "ab_os_ghgs.xlsx", sheet = "Emission Intensity", range="A4:AO35")%>%
  clean_names()%>% pivot_longer(cols=seq(15,23),names_to="year",values_to="adj_ghg")%>%
  select(company,facility,subsector,product,year,adj_ghg)%>%mutate(year=str_sub(year,start=2,end=5))

os_prod_data <- read_excel(path = "ab_os_ghgs.xlsx", sheet = "Emission Intensity", range="A4:AO35")%>%
  clean_names()%>% pivot_longer(cols=seq(24,32),names_to="year",values_to="prod")%>%
  select(company,facility,subsector,product,year,prod)%>%mutate(year=str_sub(year,start=2,end=5))

os_data <- read_excel(path = "ab_os_ghgs.xlsx", sheet = "Emission Intensity", range="A4:AO35")%>%
  clean_names()%>% pivot_longer(cols=seq(33,41),names_to="year",values_to="ei")%>%
  select(company,facility,subsector,product,year,ei)%>%mutate(year=str_sub(year,start=2,end=5)) %>% 
  left_join(os_ghg_data)%>% 
  left_join(os_prod_data)


#keep anything larger than tucker
os_big_projects<- os_data %>% filter(year==2019) %>% group_by(subsector)%>% filter(prod>1250000)

os_data<-os_data %>% mutate(facility=factor(facility),facility=fct_other(facility,keep = os_big_projects$facility))%>%
  group_by(facility,subsector,product,year) %>% summarize(prod=sum(prod),ei=sum(adj_ghg)/sum(prod),adj_ghg=sum(adj_ghg))


os_data<-os_data %>% mutate(facility=fct_recode(facility, "Foster Creek" = "Foster Creek SAGD Bitumen Battery (with Cogen)",
                                                "Canadian Natural AOSP"="Canadian Natural Upgrading Limited Muskeg River Mine and Jackpine Mine and Scotford Upgrader",
                                                "MEG Christina Lake"="MEG Christina Lake Regional Project",
                                                "Cenovus Christina Lake"="Christina Lake SAGD Bitumen Battery",
                                                "Hangingstone"="Hangingstone Expansion Project",
                                                "MacKay River"="MacKay River, In-Situ Oil Sands Plant",
                                                "Surmont"="Surmont SAGD Commercial Battery"
                                                
                                                
                                                
                                                
                                                ))%>%
        group_by(facility) %>% mutate(prod_2019=sum(prod*(year==2019)*6.2929/365))%>%
  mutate(label=paste(facility,"\n(",formatC(round(prod_2019,digits=-3),format="f", big.mark=",",digits = 0)," bbl/d)",sep=""))

  


  p<-ggplot(os_data%>%filter(subsector=="In Situ",ei<2))+
    geom_col(aes(year,ei),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
    #geom_hline(aes(yintercept = 32.65*1.26,colour="Current $CA bitumen value"))+
    facet_wrap(~label,nrow = 3)+
    scale_colour_manual("",values="black")+
    #scale_x_reverse()+
    #coord_flip()+
    #scale_fill_viridis("Reporting Year",discrete = T)+
    guides(colour=guide_legend(),fill=FALSE)+
    blake_theme()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(.10,0,.10,0),unit="cm"),
      #legend.text = element_text(colour="black", size = 10, face = "bold"),
      #plot.caption = element_text(size = 8, face = "italic"),
      #plot.title = element_blank(),
      #plot.subtitle = element_blank(),
      #panel.grid.minor = element_blank(),
      text = element_text(size = 18,face = "bold"),
      #axis.text = element_text(size = 10,angle=0,hjust = 1),
      axis.text.x = element_text(size = 18,hjust=0.5,angle=90),
      plot.caption = element_blank()
    )+
    labs(y="Cogen-adjusted emissions per barrel (t/bbl)",x=NULL,
         title="2011-2019 Emissions Intensity by Oil Sands In-Situ Project",
         #subtitle="Projects with production above 40,000 bbl/d in every year. 2019 production, rounded to the nearest thousand barrels per day, shown in brackets.",
         caption="Source: Alberta Government data, graph by @andrew_leach")
  print(p)
  ggsave("ghg_bbl_SAGD.png",dpi=300,width=20,height=14)
  
  
  
  
  p<-ggplot(os_data%>%filter(subsector!="In Situ",ei<2))+
    geom_col(aes(year,ei),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
    #geom_hline(aes(yintercept = 32.65*1.26,colour="Current $CA bitumen value"))+
    facet_wrap(~label,nrow = 3)+
    scale_colour_manual("",values="black")+
    #scale_x_reverse()+
    #coord_flip()+
    #scale_fill_viridis("Reporting Year",discrete = T)+
    guides(colour=guide_legend(),fill=FALSE)+
    work_theme()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(.10,0,.10,0),unit="cm"),
      #legend.text = element_text(colour="black", size = 10, face = "bold"),
      #plot.caption = element_text(size = 8, face = "italic"),
      #plot.title = element_blank(),
      #plot.subtitle = element_blank(),
      #panel.grid.minor = element_blank(),
      text = element_text(size = 18,face = "bold"),
      #axis.text = element_text(size = 10,angle=0,hjust = 1),
      axis.text.x = element_text(size = 18,hjust=0.5,angle=90),
      #plot.caption = element_blank()
    )+
    labs(y="Cogen-adjusted emissions per barrel (t/bbl)",x=NULL,
         title="2011-2019 Emissions Intensity by Oil Sands Mining Project",
         #subtitle="Projects with production above 40,000 bbl/d in every year. 2019 production, rounded to the nearest thousand barrels per day, shown in brackets.",
         caption="Source: Alberta Government data, graph by @andrew_leach")
  print(p)
  ggsave("ghg_bbl_mines.png",dpi=300,width=16,height=12)
  
  
  
#horizon
  horizon_data<-os_data %>% clean_names() %>% filter(grepl("Horizon",project_name))%>%
    mutate(production_daily=cleaned_crude_bitumen_at_rcp_barrels/365,
           mfsp=(gross_revenue-operating_costs)/cleaned_crude_bitumen_at_rcp_barrels)
           
           