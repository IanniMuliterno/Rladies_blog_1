# proximos passos

#concluir codigo
#melhorar readme
#falar com a Bea para ver como dou um push pro repositorio do blog
##perguntar se rola um tutorial de quarto

library(tidyverse)
library(readr)
library(ggplot2)
dados_olimpiadas <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv")


colSums(is.na(dados_olimpiadas))

#https://stackoverflow.com/questions/70817220/ggplot2-facet-wrap-with-scaling-for-each-variable
#checar numericas
dados_olimpiadas %>% 
  select_if(.,is.numeric) %>%
  pivot_longer(1:2, names_to = 'variaveis', values_to = 'valor') %>% 
  ggplot(aes(x = valor)) +
  geom_histogram() +
  facet_wrap(~variaveis,scales = 'free')

#gráfico para mostrar medalhas de ouro prata e bronze conquistadas por cada país


paralympic_total <- dados_olimpiadas %>%
  filter(type == "Swimming") %>%
  group_by(abb) %>%
  count() %>%
  arrange(desc(n))
paralympic_byMedal <- dados_olimpiadas %>%
  filter(type == "Swimming") %>%
  group_by(abb, medal) %>%
  count()
paralympic_swimming <- paralympic_byMedal %>%
  left_join(paralympic_total, by = "abb") %>%
  rename(number = n.x, total_medals = n.y)

#garantindo que no plot de top five teremos ordenação de acordo com o país que ganhou mais 
top5 <- paralympic_total$abb[1:5]
tail5 <- paralympic_total$abb[(nrow(paralympic_total)-4):nrow(paralympic_total)] 

table_top5 <- paralympic_swimming %>% 
  filter(abb %in% top5) %>%
  mutate(abb = factor(abb,levels = top5)) 

table_tail5 <- paralympic_swimming %>% 
  filter(abb %in% tail5) %>%
  mutate(abb = factor(abb,levels = tail5)) 


plot2 <- table_top5 %>% 
  ggplot(aes(x = abb, y=number, fill = medal))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values = c("#D89581", "#D4AF37", "#C0C0C0"))+
  scale_x_discrete(expand = c(0,0)) +
  labs(x="", y="", fill = "", title = "Swimming medals for top 5 countries")+
  theme(legend.position = "none",
        plot.title.position = "plot", 
        plot.title = element_text(family = "mono", face = "bold", size = 8),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        axis.text = element_text(family = "mono", face = "bold", size = 8))+
  ggtitle("Medalhas de ouro, prata e bronze conquistada pelos países de melhor desempenho")


plot3 <- table_tail5 %>% 
  ggplot(aes(x = abb, y=number, fill = medal))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values = c("#D89581", "#D4AF37", "#C0C0C0"))+
  scale_x_discrete(expand = c(0,0)) +
  labs(x="", y="", fill = "", title = "Swimming medals for top 5 countries")+
  theme(legend.position = "none",
        plot.title.position = "plot", 
        plot.title = element_text(family = "mono", face = "bold", size = 8),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        axis.text = element_text(family = "mono", face = "bold", size = 8))+
  ggtitle("Medalhas de ouro, prata e bronze conquistada pelos países de pior desempenho")

plot3
# gráfico de dispersão é valido !
# juntando as bases, vai ficar menor a amostra, mas vale mostrar a relação entre idade e numero de medalhas

ggplot(dados_olimpiadas, aes(x = year, y = time, color = sex)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~event, scales = "free") +
  xlab("Idade") +
  ylab("Tempo de natação (s)") +
  ggtitle("Relação entre idade e numero de medalhes por evento - Olimpíadas 2021")

# quais esportes tem e não tem guia


#Usando o dplyr para filtrar os dados e criar uma tabela com os países com os melhores tempos médios de natação:

melhores_tempos_por_pais <- dados_olimpiadas %>%
  filter(Event == "Men's 400m Freestyle" | Event == "Women's 400m Freestyle") %>%
  group_by(NOC, Sex) %>%
  summarise(tempo_medio = mean(Time)) %>%
  ungroup() %>%
  group_by(NOC) %>%
  summarise(melhor_tempo_medio = min(tempo_medio)) %>%
  arrange(melhor_tempo_medio)

melhores_tempos_por_pais