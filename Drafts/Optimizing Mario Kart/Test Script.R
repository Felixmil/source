### Game Design analysis

As longtime Nintendo fan, I know that the company puts effort into creating immersive experiences and comprehensive environment. Well, ok it's always somehow weird and cartoony... but my point is that there's always something players can rely on and feel as "normal" first (physics, storyline, characters feelings ...) before acceptiong all the crazy things that are happening around him.

That door into reality for Mario Kart is characters weights and physics : the player expects Bowser to weight more than Toad and thus be less agile while the latter have less inertia (thus lower maximum speed). 
The maths behind Mario Kart's game design can be summed with the following chart :

```{r}
plot_ly(data = (data %>% filter(Type=="Char")), x=~Speed_mean,y=~Accel, label=~ID, color=~`M-turbo`,size=~Weight,title="test",  text=~paste("Char :",ID, "<br> Weight :", Weight,"<br> M-Turbo :",`M-turbo`)) %>% hide_colorbar() %>% layout(title="Characters distribution between Acceleration, Mean Speed, Turbo and Weight", subtitle="Light characters have higher Acceleration, Turbo (+ Handling & Traction since positively correlated) and \nlower Mean Speed while heavy characters behave in an opposite way")
```


It is also true with Kart parts (Here : Tires)
```{r}
ggplot(data %>% filter(Type=="Tire"),
aes(x=Speed_mean,y=Accel))+
geom_point(aes(size=Weight, fill=`M-turbo`), 
pch=21)+
ggrepel::geom_text_repel(aes(label=ID),
min.segment.length = 0.1,
point.padding = unit(1.2, 'lines'),
box.padding = unit(0.6, 'lines'),
force=2)+
viridis::scale_fill_viridis()+
geom_rect(aes(ymin=-Inf, ymax=0, xmin=-Inf, xmax=0),
alpha=0.01,
fill="red")+
geom_rect(aes(ymin=0, ymax=Inf, xmin=0, xmax=Inf),
alpha=0.01,
fill="green")+
labs(
title="Tires distribution between Acceleration, Mean Speed, Turbo and Weight",
subtitle="Ligher Tires grant higher Acceleration, Turbo (+ Handling & Traction since positively correlated) bonus and \n Mean Speed malus while heavy characters behave in an opposite way",
caption= "Some kart parts grants malus only, none grants bonus only")
```

Weight can be considered as a player preference depending on the kind of gameplay (or character) they like :

* High wheight have generaly the best speed. Fits players that can dodge obstacle, don't want to be pushed and knows the tracks perfectly.
* Low wheight have best acceleration. Fits players that want return to max speed faster and wants to trigger turbo more ofter/powerfully


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

