library(ggthemes)
library(janitor)
library(scales)
library(tidyverse)
library(readxl)
library(openxlsx)
library(viridis)

work_theme<-function(){
  theme_hc(20)+
    theme(plot.subtitle = element_text(color="grey10",size=rel(.5)),
          plot.title = element_text(face="bold"),
          plot.caption = element_text(color="grey50",size=rel(.5)),
          legend.title = element_text(color="grey10",size=rel(1)),
          legend.text = element_text(color="grey10",size=rel(1)),
          strip.text = element_text(size=rel(1)),
          axis.title = element_text(size=rel(1)),
          axis.text = element_text(size=rel(1)),
          axis.ticks = element_blank(),
          panel.spacing = unit(2,"lines"),
          legend.position = "bottom",
          plot.margin = margin(t = .5, r = 1, b = .25, l = 1,unit= "cm"),
          axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
          
    )
}



#load plant data
os_data_2021 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2021 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

os_data_2020 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2020 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

os_data_2019 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2019 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
os_data_2018 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2018 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

os_data_2017 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2017 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
os_data_2016 <- read.xlsx(xlsxFile = "royalty_data.xlsx", sheet = "2016 Royalty Data", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
os_data<-bind_rows(os_data_2021,os_data_2020,os_data_2019,os_data_2018,os_data_2017,os_data_2016)


os_data<-os_data %>% clean_names()


os_data <- os_data %>% mutate(
  royalty_bbl=royalty_payable/cleaned_crude_bitumen_at_rcp_barrels,
  op_costs_bbl=operating_costs/cleaned_crude_bitumen_at_rcp_barrels,
  cap_costs_bbl=capital_costs/cleaned_crude_bitumen_at_rcp_barrels,
  project_name=gsub(" project","",project_name),
  project_name=gsub("Christina Lake Regional","Christina Lake (MEG)",project_name),
  project_name=gsub("Christina Lake Thermal","Christina Lake (CVE)",project_name),
  project_name=gsub("MacKay River Commercial","PetroChina",project_name),
  project_name=gsub("MacKay River","MacKay River (Suncor)",project_name),
  project_name=gsub("PetroChina","MacKay River (PetroChina)",project_name),
  project_name=gsub(" Thermal","",project_name),
  project_name=gsub(" Mine","",project_name),
  project_name=gsub(" Oil Sands","",project_name),
  project_name=gsub(" EOR","",project_name),
  project_name=gsub(" Commercial","",project_name),
  project_name=gsub(" SAGD","",project_name),
  project_name=gsub(" Demonstration","",project_name),
  project_name=gsub(" In-Situ","",project_name),
  project_name=as.factor(project_name),
  op_profit=gross_revenue_bbl-op_costs_bbl-royalty_bbl
)




#big_projects<-function(os_data_sent,threshold){
#  big_projects<-os_data_sent %>% filter(cleaned_crude_bitumen_at_rcp_barrels>threshold*365) %>% select(project_name) %>% unique()
#  filter(os_data_sent,project_name %in% big_projects$project_name)
#}

p<-ggplot(os_data %>% filter(cleaned_crude_bitumen_at_rcp_barrels>10000*365), aes(project_name,royalty_bbl,colour=royalty_bbl,fill=royalty_bbl),alpha=0.5)+
  geom_col(aes(project_name,royalty_bbl),size=.5,position = position_dodge(width = .5),color="black")+
  #scale_color_viridis("Royalties Paid\nPer Barrel\nCleaned Bitumen")+
  #scale_fill_viridis(discrete=FALSE,"Royalties Paid ($/bbl)")+
  coord_flip()+
  facet_wrap(~reporting_year)+
  #scale_x_reverse()+
  weekly_graphs()+
  labs(x=NULL,y=NULL,
       title="Alberta Royalties Paid by Oil Sands project",
       #subtitle="Excluding Electricity,by NAICS 4-Digit Code",
       caption="Source: Alberta Government Data\nGraph by @andrew_leach")
p
ggsave("royalties_bbl.png",bg="white")


mines<-c("Muskeg River","Fort Hills","Kearl","Horizon","Muskeg River","Jackpine","Syncrude","Suncor")

SAGD<-c("Hangingstone","Leismer","Blackrod","Mackay River","Kirby","Christina Lake","Foster Creek",
        "Long Lake","Great Divide","Surmont","Jackfish","Sunrise","Orion","Firebag","MacKay River",
        "West Ells","SAGD","Tucker","Narrows Lake")

big_projects<-os_data %>% group_by(project_name) %>% mutate(max_bbls=max(cleaned_crude_bitumen_at_rcp_barrels),
                                                            min_bbls=min(cleaned_crude_bitumen_at_rcp_barrels))%>%
  filter(min_bbls>30000*365) %>%
  filter(max_bbls<400000*365)%>% select(project_name) %>% distinct() 

med_SAGD<-os_data %>% filter(project_name %in% big_projects$project_name) %>% filter(!project_name %in% mines)

unique(med_SAGD$project_name)

bigger_projects<-os_data %>% filter(cleaned_crude_bitumen_at_rcp_barrels>25000*365) %>% select(project_name) %>% unique()

mine_projects<-os_data %>% filter(project_name %in% mines)

#SAGD_matches <- data.frame(test_data$project_name[grep(paste(SAGD,collapse="|"), 
#                                                  test_data$project_name)])

big_SAGD<-os_data %>% filter(project_name %in% bigger_projects$project_name) %>% filter(!project_name %in% mines)

small_SAGD<-os_data %>% filter(!project_name %in% bigger_projects$project_name) %>% filter(!project_name %in% mines)



test_data <- os_data %>% mutate(SAGD_indicator=project_name %in% SAGD) %>%
  select(project_name,SAGD_indicator)



non_matches <- data.frame(test_data$project_name[-grep(paste(SAGD,collapse="|"), 
                                                       test_data$project_name)])

big_SAGD %>% group_by(reporting_year) %>% summarize(op_costs_bbl=sum(operating_costs)/sum(cleaned_crude_bitumen_at_rcp_barrels))

mine_projects %>% group_by(reporting_year) %>% summarize(op_costs_bbl=sum(operating_costs)/sum(cleaned_crude_bitumen_at_rcp_barrels))

big_SAGD %>% group_by(reporting_year) %>% summarize(cap_costs_bbl=sum(capital_costs)/sum(cleaned_crude_bitumen_at_rcp_barrels))
mine_projects %>% group_by(reporting_year) %>% summarize(cap_costs_bbl=sum(capital_costs)/sum(cleaned_crude_bitumen_at_rcp_barrels))

big_SAGD <-big_SAGD %>% group_by(project) %>% mutate(higher_2018=sum(op_costs_bbl*(reporting_year==2018))>sum(op_costs_bbl*(reporting_year==2017))) 


big_SAGD %>% filter(reporting_year==2018) %>% group_by(higher_2018) %>% summarise(barrels=sum(cleaned_crude_bitumen_at_rcp_barrels,na.rm=T))



costs<-os_data %>% group_by(reporting_year) %>% summarize(avg_costs=sum(operating_costs/sum(cleaned_crude_bitumen_at_rcp_barrels)),
                                                          avg_rev=sum(project_revenue,na.rm = T)/sum(cleaned_crude_bitumen_at_rcp_barrels,na.rm = T))


#df1<- df1[df1$NAICS4!=2211,]

ggplot(big_SAGD)+
  #geom_col(aes(project_name,op_profit,group=reporting_year,fill = factor(reporting_year,levels = c(2018,2017,2016))),size=.5,position = position_dodge(width = 1))+
  geom_line(aes(reporting_year,op_profit),size=1.5)+
  facet_wrap(~project_name,nrow = 3)+
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
       title="2016-2019 Operating Costs by Oil Sands In Situ project",
       subtitle="For projects with production above 25k bbl/d in any year",
       caption="Source: Alberta Government Data\nGraph by @andrew_leach")
ggsave("op_profit_SAGD.png",dpi=200,width=14,height=9,bg="white")


big_SAGD$reporting_year<-as_factor(big_SAGD$reporting_year)
#df1<- df1[df1$NAICS4!=2211,]
p<-ggplot(big_SAGD)+
  geom_col(aes(factor(reporting_year,levels = c(2016,2017,2018,2019,2020)),op_costs_bbl),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
  facet_wrap(~project_name,nrow = 3)+
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
       title="2016-2018 Operating Costs by Oil Sands In Situ project",
       subtitle="projects with production above 25k bbl/d in any year",
       caption="Source: Alberta Government data, graph by @andrew_leach")
p

ggsave("op_costs_bbl_big_SAGD.png",dpi=600,width=24,height=16,bg="white")


med_SAGD<-med_SAGD %>% group_by(project_name)%>%
  mutate(prod_2019=max(cleaned_crude_bitumen_at_rcp_barrels*(reporting_year==2019)/365))%>%
  mutate(label=paste(project_name,"\n(",formatC(round(prod_2019,digits=-3),format="f", big.mark=",",digits = 0)," bbl/d)",sep=""))

#set_png("op_costs_bbl_SAGD.png",width=1600)


p<-ggplot(med_SAGD)+
  geom_col(aes(factor(reporting_year,levels = c(2016,2017,2018,2019)),op_costs_bbl+cap_costs_bbl),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
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
       title="2016-2019 Operating and Sustaining Capital Costs by Oil Sands In Situ project",
       subtitle="projects with production above 40,000 bbl/d in every year. 2019 production, rounded to the nearest thousand barrels per day, shown in brackets.",
       caption="Source: Alberta Government data, graph by @andrew_leach")
print(p)
ggsave("op_costs_bbl_SAGD.png",dpi=600,width=24,height=16)


p<-ggplot(filter(mine_projects,project_name!="Fort Hills Oil Sands project"))+
  geom_col(aes(reporting_year,op_costs_bbl,group=reporting_year),size=.5,position = position_dodge(width = 1),fill="dodgerblue")+
  facet_wrap(~project_name)+
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
       title="2016-2018 Operating Costs by Oil Sands Mining project",
       subtitle="For projects with 2016 Production above 10k bbl/d.\nIncludes only bitumen production costs, not upgrading.",
       caption="Source: Alberta Government data, graph by @andrew_leach")
print(p)
ggsave("op_costs_bbl_mines.png")


project_data <-os_data%>%filter(project_name %in% big_projects$project_name) %>% group_by(reporting_year) %>% 
  mutate(year_total=sum(cleaned_crude_bitumen_at_rcp_barrels), weight=cleaned_crude_bitumen_at_rcp_barrels/year_total) %>%
  ungroup()

g <- ggplot(project_data, aes(op_costs_bbl,group=factor(reporting_year),weights=weight))+
  stat_density(aes(color=factor(reporting_year)),geom="line",position = "identity",trim=T,size=1.6)+
  scale_colour_manual(NULL,values=colors_tableau10())+
  #geom_density(aes(color=factor(reporting_year)), alpha=0.8) + 
  labs(title="Density plot of oil sands operating costs per barrel bitumen", 
       subtitle="Production-weighted, for projects with more than 10,000 barrels per day of bitumen production",
       caption="Source: Government of Alberta 2016 and 2017 Royalty Data, graph by Andrew Leach",
       x="Operating Costs ($Cdn/bbl bitumen)",
       fill="Reporting Year")+
  weekly_graphs()
print(g)
ggsave("op_costs_density.png")

ggplot(os_data%>%filter(cleaned_crude_bitumen_at_rcp_barrels>10000*365), aes(op_costs_bbl), ) +
  geom_col(aes(project_name,op_costs_bbl,group=reporting_year,fill = factor(reporting_year)),size=.5,position = position_dodge(width = 1))+
  #scale_fill_manual()+
  #scale_x_reverse()+
  coord_flip()


#df1<- df1[df1$NAICS4!=2211,]

projects<-os_data%>%filter(cleaned_crude_bitumen_at_rcp_barrels>=10000*365)%>%select(project_name)%>%distinct()%>%
  mutate(name=as.character(project_name))
p<-
  ggplot(os_data%>%filter(project_name %in% projects$name))+
  geom_col(aes(as.factor(reporting_year),op_profit,fill=op_profit>0),size=.5,position = position_dodge())+
  scale_fill_manual("",values=c("FALSE"="red","TRUE"="darkgreen"),labels=c("Operating Loss","Operating Profit"))+
  scale_x_discrete(breaks=pretty_breaks(n=5))+
  facet_wrap(~project_name)+
  #scale_x_reverse()+
  #coord_flip()+
  guides(fill=guide_legend())+
  weekly_graphs()+
  theme(axis.text.x = element_text(size = 10,angle=90,vjust=0.5),
        strip.text = element_text(size = 10))+
  labs(y="Operating Profit or Loss ($/bbl)",x=NULL,
       title="Operating Profit by Oil Sands project",
       subtitle="Gross bitumen revenue net operating costs and royalties for projects with at least one year in 2016-2021 with production above 10k bbl/d",
       caption="Source: Alberta Government data, graph by @andrew_leach")
print(p)
ggsave("net_rev_bbl.png")

p<-
  ggplot(os_data%>%filter(project_name %in% projects$name)%>%
           mutate(payout=as.factor(payout_status),
                  payout=fct_relevel(payout,"PRE"),
                  project_name=fct_rev(project_name)))+
  geom_col(aes(as.factor(reporting_year),royalty_payable/cleaned_crude_bitumen_at_rcp_barrels,fill=payout),size=.5,position = position_dodge())+
  scale_fill_manual("",values=c("PRE"="red","POST"="darkgreen"),labels=c("Pre-Payout","Post-Payout"))+
  scale_x_discrete(breaks=pretty_breaks(n=5))+
  facet_wrap(~project_name,nrow = 5)+
  #scale_x_reverse()+
  #coord_flip()+
  guides(fill=guide_legend())+
  weekly_graphs()+
  theme(axis.text.x = element_text(size = 10,angle=90,vjust=0.5),
        strip.text = element_text(size = 8))+
  labs(y="Royalties Payable ($/bbl)",x=NULL,
       title="Royalties Paid by Oil Sands project",
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
       #title="Royalties Paid by Oil Sands project",
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


pre_pay<-os_data%>%filter(os_data$unrecovered_balance_net_loss_at_eop>0)


p<-ggplot(os_data%>%filter(cleaned_crude_bitumen_at_rcp_barrels>50000*365 & unrecovered_balance_net_loss_at_eop>0))+
  geom_col(aes(factor(reporting_year,levels = c(2016,2017,2018)),unrecovered_balance_net_loss_at_eop/10^9,fill=payout_status,colour=payout_status),size=.5,position = position_dodge(width = .5))+
  #geom_col(aes(project_name,op_costs_bbl),size=.5,position = position_dodge(width = .5),fill="firebrick",colour="firebrick",alpha=1)+
  #scale_color_viridis("Operating Costs\nPer Barrel\nCleaned Bitumen")+
  scale_fill_manual("Royalty Status",values=c("PRE"="red","POST"="darkgreen"),labels=c("Post-Payout","Pre-Payout"))+
  scale_colour_manual("",values=c("PRE"="red","POST"="darkgreen"))+
  facet_wrap(~project_name,nrow = 3)+
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
       subtitle="projects with Production Greater than 50k bbl/d",
       caption="Source: Alberta Government Data\nGraph by @andrew_leach")

print(p)
ggsave("remaining_recov.png")





mines<-c("Muskeg River Mine","Fort Hills Oil Sands project","Kearl","Horizon Mine","Muskeg River Mine","Jackpine Mine","Syncrude Mine","Suncor Oil Sands")
mine_projects<-os_data %>% filter(project_name %in% mines) %>% select(project_name) %>% unique()


#GHG data

#load plant data
os_ghg_data <- read_excel(path = "ab_os_ghgs.xlsx", sheet = "Emission Intensity", range="A4:AO35")%>%
  clean_names()%>% pivot_longer(cols=seq(15,23),names_to="year",values_to="adj_ghg")%>%
  select(company,facility,subsector,product,year,adj_ghg)%>%mutate(year=str_sub(year,start=2,end=5))

os_prod_data <- read_excel(path = "ab_os_ghgs.xlsx", sheet = "Emission Intensity", range="A4:AO35")%>%
  clean_names()%>% pivot_longer(cols=seq(24,32),names_to="year",values_to="prod")%>%
  select(company,facility,subsector,product,year,prod)%>%mutate(year=str_sub(year,start=2,end=5))

os_ei_data <- read_excel(path = "ab_os_ghgs.xlsx", sheet = "Emission Intensity", range="A4:AO35")%>%
  clean_names()%>% pivot_longer(cols=seq(33,41),names_to="year",values_to="ei")%>%
  select(company,facility,subsector,product,year,ei)%>%mutate(year=str_sub(year,start=2,end=5)) %>% 
  left_join(os_ghg_data)%>% 
  left_join(os_prod_data)



#keep anything larger than tucker
os_big_projects<- os_ei_data %>% filter(year==2019) %>% group_by(subsector)%>% filter(prod>1250000)



os_ei_data %>% mutate(facility=factor(facility),facility=fct_other(facility,keep = os_big_projects$facility))%>%
  group_by(facility,subsector,product,year) %>% summarize(prod=sum(prod),ei=sum(adj_ghg)/sum(prod),adj_ghg=sum(adj_ghg))%>%
  mutate(facility=fct_recode(facility, "Foster Creek" = "Foster Creek SAGD Bitumen Battery (with Cogen)",
                                                "Canadian Natural AOSP"="Canadian Natural Upgrading Limited Muskeg River Mine and Jackpine Mine and Scotford Upgrader",
                                                "MEG Christina Lake"="MEG Christina Lake Regional project",
                                                "Cenovus Christina Lake"="Christina Lake SAGD Bitumen Battery",
                                                "Hangingstone"="Hangingstone Expansion project",
                                                "MacKay River"="MacKay River, In-Situ Oil Sands Plant",
                                                "Surmont"="Surmont SAGD Commercial Battery"))%>%
  group_by(facility) %>% mutate(prod_2019=sum(prod*(year==2019)*6.2929/365))%>%
  mutate(label=paste(facility,"\n(",formatC(round(prod_2019,digits=-3),format="f", big.mark=",",digits = 0)," bbl/d)",sep=""))%>%
  filter(subsector=="In Situ",ei<2)%>%
  ggplot()+
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
       title="2011-2019 Emissions Intensity by Oil Sands In-Situ project",
       #subtitle="projects with production above 40,000 bbl/d in every year. 2019 production, rounded to the nearest thousand barrels per day, shown in brackets.",
       caption="Source: Alberta Government data, graph by @andrew_leach")
ggsave("ghg_bbl_SAGD.png",dpi=300,width=20,height=14)



os_ei_data %>% mutate(facility=factor(facility),facility=fct_other(facility,keep = os_big_projects$facility))%>%
  group_by(facility,subsector,product,year) %>% summarize(prod=sum(prod),ei=sum(adj_ghg)/sum(prod),adj_ghg=sum(adj_ghg))%>%
  mutate(facility=fct_recode(facility, "Foster Creek" = "Foster Creek SAGD Bitumen Battery (with Cogen)",
                             "Canadian Natural AOSP"="Canadian Natural Upgrading Limited Muskeg River Mine and Jackpine Mine and Scotford Upgrader",
                             "MEG Christina Lake"="MEG Christina Lake Regional project",
                             "Cenovus Christina Lake"="Christina Lake SAGD Bitumen Battery",
                             "Hangingstone"="Hangingstone Expansion project",
                             "MacKay River"="MacKay River, In-Situ Oil Sands Plant",
                             "Surmont"="Surmont SAGD Commercial Battery"))%>%
  group_by(facility) %>% mutate(prod_2019=sum(prod*(year==2019)*6.2929/365))%>%
  mutate(label=paste(facility,"\n(",formatC(round(prod_2019,digits=-3),format="f", big.mark=",",digits = 0)," bbl/d)",sep=""))%>%
  filter(subsector!="In Situ",ei<2)%>%
  ggplot()+
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
       title="2011-2019 Emissions Intensity by Oil Sands Mining project",
       #subtitle="projects with production above 40,000 bbl/d in every year. 2019 production, rounded to the nearest thousand barrels per day, shown in brackets.",
       caption="Source: Alberta Government data, graph by @andrew_leach")

ggsave("ghg_bbl_mines.png",dpi=300,width=16,height=12)



#horizon
horizon_data<-os_data %>% 
  filter(grepl("Horizon",project_name))%>%
  mutate(production_daily=cleaned_crude_bitumen_at_rcp_barrels/365,
         mfsp=(gross_revenue-operating_costs)/cleaned_crude_bitumen_at_rcp_barrels)

