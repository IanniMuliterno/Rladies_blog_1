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

#Continuando podemos ver a diferença entre a quantidade de medalhas conquistadas entre os atletas homens 
#e mulheres? Para isso, veremos primeiro o histograma de nadadores homens e mulheres por país, em seguida 
#o box plot das medalhas conquistadas por cada um, agrupado também pelo tipo de medalha.


dados_olimpiadas %>%
  filter(type == "Swimming") %>%
  filter(gender != 'Mixed') %>% 
  group_by(abb,athlete,gender) %>% 
  count() %>% 
  group_by(abb,gender) %>% 
  count() %>% 
  ggplot(aes(x = n, fill = gender)) +
  geom_histogram(alpha = .6) +
  ggtitle("Histograma das medalhas conquistadas por gênero em cada país") +
  theme(
    panel.background = element_rect(fill = "white")
  )


dt_perc_gender <- dados_olimpiadas %>%
  filter(type == "Swimming") %>%
  filter(gender != 'Mixed') %>% 
  group_by(abb,year,gender,athlete) %>% 
  count() %>% 
  group_by(abb,year,gender) %>%
  count() %>% 
  pivot_wider(names_from  = gender,values_from = n) %>% 
  mutate(across(ends_with('en'), ~ifelse(is.na(.),0,. ) ) ) %>% 
  group_by(abb) %>% 
  summarize(perc_mulheres = mean(Women/(Women+Men)))

linha <- median(dt_perc_gender$perc_mulheres)

dt_perc_gender %>%   
ggplot(aes(x = perc_mulheres )) +
  geom_histogram()+
  geom_vline(mapping = aes(xintercept=linha),linetype = 2,color = 'red') +
  annotate("text", x=linha+0.05, y=15, label= "mediana",color = 'red') +
  ggtitle("Histograma do percentual de mulheres que os países levam em cada olimpíada") +
  theme(
    panel.background = element_rect(fill = "white")
  )
  
  
dados_olimpiadas %>%
  filter(type == "Swimming") %>%
  filter(gender != 'Mixed') %>% 
  group_by(abb,athlete,gender,medal) %>% 
  count() %>% 
  group_by(abb,gender,medal) %>% 
  count() %>% 
  ggplot(aes(y = n, fill = gender, x = medal)) +
  geom_boxplot()+

  ggtitle("Box-plot das medalhas conquistadas por tipo de medalha e gênero em cada país")+
  theme(
    panel.background = element_rect(fill = "white")
  )
#quantas medalhas já ganhou, de quantas olimpíadas já participou, proporção de medalhas por tipo, e um
#kpi de quantidade de medalhas por olimpíada

#variável resposta : última olimpíada de cada atleta
resposta <- dados_olimpiadas %>%
  filter(athlete != '-') %>% 
  filter(type == "Swimming") %>%
  group_by(athlete,year) %>% 
  count() %>% 
  group_by(athlete) %>% 
  filter(year == max(year))

resposta <- dados_olimpiadas %>%
  filter(athlete != '-') %>% 
  filter(type == "Swimming") %>%
  filter(year == 2016) %>% 
  group_by(athlete) %>% 
  count() 
#base, tira os dados da variáveis resposta
base <- dados_olimpiadas %>% 
  inner_join(resposta, by = 'athlete') %>% 
  filter(year < 2016) %>% 
#  mutate(year = year.x) %>% 
#  select(-year.x,-year.y,-n) %>%
  select(-n) %>% 
  
  group_by(athlete) %>% 
  summarize(total_medalhas = n(),
            ouro = sum(medal == 'Gold'),
            prata = sum(medal == 'Silver'),
            bronze = sum(medal == 'Bronze'),
            total_olympics = n_distinct(year),
            medal_olympics_kpi = n()/n_distinct(year),
            gender = unique(gender[gender != 'Mixed']),
            n_eventos = n_distinct(event)) %>% 
  mutate(ouro = ouro/total_medalhas,
         prata = prata/total_medalhas,
         bronze = bronze/total_medalhas)

#o warning significa que o agrupamento não funcionou para alguns atletas
base %>% group_by(athlete) %>% count() %>% arrange(desc(n))

# checando, vejo que são erros, onde na coluna de nome de atletas, temos nomes de países
manter <- base %>% 
  group_by(athlete) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n == 1)

base <- base %>% filter(athlete %in% manter$athlete)
cor(base$total_medalhas,base$total_olympics)

# escolher uma das variáveis para manter de acordo com qual traz melhor resultado para o modelo 
# ficando sozinha

#base final

base_final <- base %>% 
     inner_join(resposta, by = 'athlete') %>% 
  mutate(y = n) %>% 
  ungroup() %>% 
#  select(-year,-n, -athlete)
  select(-n, -athlete)

modelo <- glm(y~.,data = base_final,family = 'poisson')
summary(modelo)
plot(modelo)

modelo2 <- glm(y~ouro+prata+total_olympics+medal_olympics_kpi+gender+n_eventos,data = base_final,family = 'poisson')
summary(modelo2)

modelo3 <- glm(y~ouro+prata+total_olympics+medal_olympics_kpi+gender,data = base_final,family = 'poisson')
summary(modelo3)
plot(modelo3)

modelo4 <- glm(y~ouro+total_olympics+medal_olympics_kpi+gender,data = base_final, family = 'poisson')
summary(modelo4)
plot(modelo4)


modelo5 <- glm(y~total_olympics+medal_olympics_kpi+gender,data = base_final, family = 'poisson')
summary(modelo5)

modelo5 <- glm(y~medal_olympics_kpi+total_olympics,data = base_final, family = 'poisson')
summary(modelo5)
plot(modelo5)

summary(modelo5)$deviance

###################
ggplot(modelo5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)+
  ggtitle("Gráfico de resíduos versus valor predito")+
  theme(
    panel.background = element_rect(fill = "white")
  )


#Pearson's chi-squared test: Calculate the Pearson's chi-squared goodness-of-fit test to evaluate 
#the model's fit. This test compares the observed and expected counts for each group and determines
#if the difference is statistically significant. 

# Calculate the expected counts
expected <- fitted(modelo5)

# Calculate the Pearson's chi-squared statistic and p-value
pearson_chisq <- sum((base_final$y - expected)^2 / expected)
p_value <- 1 - pchisq(pearson_chisq, df = df.residual(modelo5))

# Display the results
cat("Pearson's chi-squared test:\n")
cat("X-squared = ", pearson_chisq, ", df = ", df.residual(modelo5), ", p-value = ", p_value, "\n")

