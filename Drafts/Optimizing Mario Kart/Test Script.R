
# Computes bonus/malus ratios


data <- data %>% mutate("Bonus_Rate"=ifelse(Speed_mean > Accel, abs(Speed_mean/Accel),abs(Accel/Speed_mean)))

ggplot(data %>% filter(Type=="Kart"), aes(Bonus_Rate, ID))+geom_point()+ggrepel::geom_text_repel(aes(label=paste(Speed_mean,"/",Accel)), min.segment.length = 0.1,point.padding = unit(1.2, 'lines'),box.padding = unit(0.6, 'lines'), force=2)


#Lighest combo have higher totals once weight is put out of the balance. 

-------
  
ggplot(data_combo, aes(total_perf, w_cat))+geom_density_ridges(aes(fill=w_cat),panel_scaling = F, alpha=0.8,rel_min_height = 0.000001)+scale_fill_brewer()

ggplot(data_combo %>% gather(key=stat, value=stat_v, c(Accel, Speed_mean, `M-turbo`,Traction, Handling_mean))) + geom_density_ridges2(aes(stat_v, w_cat, fill=w_cat), alpha=0.85)+facet_grid(stat~., switch = "y")+scale_fill_brewer()+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

data_combo %>% group_by(w_cat) %>% select(-Char,-Glidder, -Kart, -Tire) %>% select(Accel, Speed_mean, Handling_mean, `M-turbo`,Traction)%>% skim_to_wide() %>% arrange(variable)

--------
  
  #Total perfs computation
  data_combo$total <- rowSums(data_combo[,c(9,15:18)], na.rm = T)
data_combo$total_A_S <- rowSums(data_combo[,c(9,17)])

-------
  
#Wich is the Most balanced Combo ?

#Is there any combination that results with all stats being above or equal to 3.5 ?

data_combo %>%filter(Speed_Land>=3.5 & Accel >= 3.5 & Handling_Land >=3.5 & `M-turbo` >=3.5 & Traction >=3.5) %>% select(Char, Kart, Tire, Glidder,total_perf,Weight, Accel, Speed_mean,Speed_Land, Handling_mean, `M-turbo`,Traction, Handling_Land)


#I want a Weight less than 3.5 and the best Accel I can get in this list.


data_combo %>%filter(Speed_Land>=3.5 & Accel >= 3.5 & Handling_Land >=3.5 & `M-turbo` >=3.5 & Traction >=3.5 & Weight <3.5)%>% top_n(n=5, wt=Accel) %>% select(Char, Kart, Tire, Glidder,total_perf,Weight, Accel, Speed_mean,Speed_Land, Handling_mean, `M-turbo`,Traction, Handling_Land) %>% arrange(desc(total_perf))

