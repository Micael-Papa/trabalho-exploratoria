setwd("d:/Arquivos/Desktop/graduacao-estatistica-unb/estatistica-exploratoria/trabalho")
dir()
#install.packages("read.dbc")
require(tidyverse)
require('read.dbc')
A1=read.dbc("DNAC2017.dbc")
A2=read.dbc("DNBA2017.dbc")
A3=read.dbc("DNES2017.dbc")
A4=read.dbc("DNMS2017.dbc")
A5=read.dbc("DNPE2017.dbc")
A6=read.dbc("DNRN2017.dbc")
A7=read.dbc("DNSC2017.dbc")
A8=read.dbc("DNAL2017.dbc")      
A9=read.dbc("DNAM2017.dbc")       
A10=read.dbc("DNAP2017.dbc")      
#A11=read.dbc("DNBR2017.dbc")  #Não utilizar     
A12=read.dbc("DNCE2017.dbc")       
A13=read.dbc("DNDF2017.dbc")      
A14=read.dbc("DNGO2017.dbc")       
A15=read.dbc("DNMA2017.dbc")      
A16=read.dbc("DNMG2017.dbc")      
A17=read.dbc("DNMT2017.dbc")      
A18=read.dbc("DNPA2017.dbc")       
A19=read.dbc("DNPB2017.dbc")      
A20=read.dbc("DNPI2017.dbc")       
A21=read.dbc("DNPR2017.dbc")       
A22=read.dbc("DNRJ2017.dbc")      
A23=read.dbc("DNRO2017.dbc")       
A24=read.dbc("DNRR2017.dbc")       
A25=read.dbc("DNRS2017.dbc")      
A26=read.dbc("DNSE2017.dbc")       
A27=read.dbc("DNSP2017.dbc")       
A28=read.dbc("DNTO2017.dbc") 

require(tidyverse)

newA1=A1%>%
  mutate(Estado='AC')
newA2=A2%>%
  mutate(Estado='BA')
newA3=A3%>%
  mutate(Estado='ES')
newA4=A4%>%
  mutate(Estado='MS')
newA5=A5%>%
  mutate(Estado='PE')
newA6=A6%>%
  mutate(Estado='RN')
newA7=A7%>%
  mutate(Estado='SC')
newA8=A8%>%
  mutate(Estado='AL')
newA9=A9%>%
  mutate(Estado='AM')
newA10=A10%>%
  mutate(Estado='AP')
newA12=A12%>%
  mutate(Estado='CE')
newA13=A13%>%
  mutate(Estado='DF')
newA14=A14%>%
  mutate(Estado='GO')
newA15=A15%>%
  mutate(Estado='MA')
newA16=A16%>%
  mutate(Estado='MG')
newA17=A17%>%
  mutate(Estado='MT')
newA18=A18%>%
  mutate(Estado='PA')
newA19=A19%>%
  mutate(Estado='PB')
newA20=A20%>%
  mutate(Estado='PI')
newA21=A21%>%
  mutate(Estado='PR')
newA22=A22%>%
  mutate(Estado='RJ')
newA23=A23%>%
  mutate(Estado='RO')
newA24=A24%>%
  mutate(Estado='RR')
newA25=A25%>%
  mutate(Estado='RS')
newA26=A26%>%
  mutate(Estado='SE')
newA27=A27%>%
  mutate(Estado='SP')
newA28=A28%>%
  mutate(Estado='TO')

Data=rbind(newA1,newA2,newA3,newA4,newA5,newA6,newA7,newA8,newA9,newA10,newA12,newA13,newA14,newA15,newA16,newA17,newA18,newA19,newA20,newA21,newA22,newA23,newA24,newA25,newA26,newA27,newA28)

data01=Data%>%
  filter(LOCNASC=='1')%>%
  dplyr::select(IDADEMAE,ESCMAE,PESO,RACACOR,GESTACAO,Estado,DTNASC,PARTO)%>% 
  drop_na() #Estado vem na tentativa de seprar posteriormente por conglomerados
  
set.seed(7789)
estrato1=data01%>%
  filter(Estado=='AC')%>% #prop = estado/Data * 2000
  slice_sample(n=12)

estrato2=data01%>%
  filter(Estado=='BA')%>% #prop = nestado/nData * 2000
  slice_sample(n=140)

estrato3=data01%>%
filter(Estado=='ES')%>% 
  slice_sample(n=38)

estrato4=data01%>%
  filter(Estado=='MS')%>% 
  slice_sample(n=32)

estrato5=data01%>%
  filter(Estado=='PE')%>% 
  slice_sample(n=94)

estrato6=data01%>%
  filter(Estado=='RN')%>% 
  slice_sample(n=33)

estrato7=data01%>%
  filter(Estado=='SC')%>% 
  slice_sample(n=68)

estrato8=data01%>%
  filter(Estado=='AL')%>% 
  slice_sample(n=35)

estrato9=data01%>%
  filter(Estado=='AM')%>% 
  slice_sample(n=54)

estrato10=data01%>%
  filter(Estado=='AP')%>% 
  slice_sample(n=12)

estrato12=data01%>%
  filter(Estado=='CE')%>% 
  slice_sample(n=88)

estrato13=data01%>%
  filter(Estado=='DF')%>% 
  slice_sample(n=31)

estrato14=data01%>%
  filter(Estado=='GO')%>% 
  slice_sample(n=68)

estrato15=data01%>%
  filter(Estado=='MA')%>% 
  slice_sample(n=78)

estrato16=data01%>%
  filter(Estado=='MG')%>% 
  slice_sample(n=180)

estrato17=data01%>%
  filter(Estado=='MT')%>% 
  slice_sample(n=40)

estrato18=data01%>%
  filter(Estado=='PA')%>% 
  slice_sample(n=96)

estrato19=data01%>%
  filter(Estado=='PB')%>% 
  slice_sample(n=40)

estrato20=data01%>%
  filter(Estado=='PI')%>% 
  slice_sample(n=34)

estrato21=data01%>%
  filter(Estado=='PR')%>% 
  slice_sample(n=109)

estrato22=data01%>%
  filter(Estado=='RJ')%>% 
  slice_sample(n=154)

estrato23=data01%>%
  filter(Estado=='RO')%>% 
  slice_sample(n=20)

estrato24=data01%>%
  filter(Estado=='RR')%>% 
  slice_sample(n=8)

estrato25=data01%>%
  filter(Estado=='RS')%>% 
  slice_sample(n=98)

estrato26=data01%>%
  filter(Estado=='SE')%>% 
  slice_sample(n=24)

estrato27=data01%>%
  filter(Estado=='SP')%>% 
  slice_sample(n=420)

estrato28=data01%>%
  filter(Estado=='TO')%>% 
  slice_sample(n=18)

analise=rbind(estrato1,estrato2,estrato3,estrato4,estrato5,estrato6,estrato7,estrato8,estrato9,estrato10,estrato12,estrato13,estrato14,estrato15,estrato16,estrato17,estrato18,estrato19,estrato20,estrato21,estrato22,estrato23,estrato24,estrato25,estrato27,estrato28)


# antes de prosseguirmos com as questões iremos formular as funções que serão usadas com o decorrer do trabalho:

# assim como os pacotes necessários

require(ggplot2)
library(ggpubr)

cores <- c('#A11D21','#003366','#FF6600','#CC9900','#CC9966',"#008091", "#041835",
           "#666666")
options(
  ggplot2.discrete.color = cores,
  ggplot2.discrete.fill = cores
)
theme <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores),
      scale_colour_manual(values = cores)
    )
  )
}


# Definindo função que retorna frequências relativas de um vetor
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com todas as medidas de posição e de dispersão



summary <- function(vec, digits = 2) {
  
  vec <- vec %>%
    na.omit()
  
  dataset <- data.frame(
   Valores = c(
      mean(vec),
      sd(vec),
      min(vec),
      quantile(vec, c(.25, .5, .75)),
      max(vec),
      sd(vec/mean(vec))
    ) %>%
      round(digits = digits)
  )
  
  rownames(dataset) <- c(
    "Média", "Desvio Padrão", "Mínimo",
    "1º Quartil", "Mediana", "3º Quartil",
    "Máximo",'Coeficiente de Variação'
  )
  
  return(dataset)
}

#1) Pode-se dizer que o número de partos varia entre os dias da semana? Por quê?
install.packages("lubridate")
require(lubridate)
library(lubridate)

library(tidyverse)

partos=analise%>%
  mutate(Mulheres='Parto')%>%
  dplyr::select(Mulheres,DTNASC,PARTO)%>%
  mutate(mes=str_sub(DTNASC, start=3, end= 4))%>%
  mutate(dia=str_sub(DTNASC, start=1, end= 2))%>%
  mutate(ano=str_sub(DTNASC, start=5, end= 8))%>%
  unite('Data',ano,mes,dia,remove=F, sep='-')%>%     # Ordenamos a data para usar o lubridate corretamente
  dplyr::select(Mulheres,mes,Data,PARTO)        # 'Mulheres' é necessário para o merge posterior

partos$Data <- as.Date(partos$Data)

table(wday(partos$Data, label=TRUE))

# fazendo um data frame com os resultados previamente obtidos

dias<-c('Segunda','Terça','Quarta','Quinta','Sexta','Sábado','Domingo')
partos<- c(335,308,305,289,294,248,221)


# calculando a frequencia de cada dia:

percent(partos)

frequencia<-c(16.75,15.40,15.25,14.45,14.70,12.40,11.05)

#Agrupando tudo em um único data.frame


newData<-data.frame(dias,partos,frequencia) 


#colocando os dias na ordem correta para apresentar graficamente

newData$dias<- factor(newData$dias, levels=c('Domingo','Segunda', 'Terça','Quarta','Quinta','Sexta','Sábado'))
newData<-newData[order(newData$dias),]


# diagrama polar:
mean(newData$partos)  # para anexar no grind.mid


#install.packages('fmsb')

require(fmsb)

# sabendo que o minimo é 221 e o máximo 335:

newData1<-newData[,2]
graph<- data.frame( Segunda= c(350,150),
                    Terça= c(350, 150),
                    Quarta= c(350, 150),
                    Quinta= c(350, 150),
                    Sexta= c(350, 150),
                    Sábado= c(350, 150),
                    Domingo= c(350,150))
d_polar <- rbind(graph, newData1)

radarchart(d_polar, axistype = 1,
           pcol = "#00AFBB", pfcol = scales::alpha("#00AFBB", 0.5), plwd = 2, plty = 1,
           cglcol = "#7f7f7f", cglty = 1, cglwd = 0.8,
           axislabcol = "#7f7f7f",
           caxislabels = c(150, 200, 250, 300, 350))






# Agora iremos relacionar o tipo de parto durante os dias:

tipoparto= analise%>%
  mutate(Mulheres='Parto')%>%
  dplyr::select(Mulheres,DTNASC,PARTO)%>%
  mutate(mes=str_sub(DTNASC, start=3, end= 4))%>%
  mutate(dia=str_sub(DTNASC, start=1, end= 2))%>%
  mutate(ano=str_sub(DTNASC, start=5, end= 8))%>%
  unite('Data2',ano,mes,dia,remove=F, sep='-')%>%     # Ordenamos a data para usar o lubridate corretamente
  
  # vale ressaltar que a criação do Data2 se fez necessária pois agora temos que lidar com o valor numérico do wday
  mutate(Dias=wday(Data2))


# Denominando cada fator com seu respectivo aspecto

tipoparto$Dias <- factor(tipoparto$Dias, levels=c(1,2,3,4,5,6,7), labels=c("Domingo","Segunda","Terça","Quarta","Quinta","Sexta","Sábado"))
tipoparto$PARTO <- factor(tipoparto$PARTO, levels=c(1,2), labels=c("Vaginal","Cesáreo"))


#efetuando uma tabela de contingencia para tipo de parto segundo dia :

tab_parto=table(tipoparto$PARTO,tipoparto$Dias)
tab_parto

addmargins(tab_parto)
prop.table(tab_parto,2)


#Gráfico de colunas justapostas:

barplot(tab_parto, col = c("#666666", "#A11D21"), 
        xlab = "Dia da semana", 
        ylab = "Número de partos", ylim = c(0, 200), 
        beside=TRUE, legend=TRUE)



#2) Existe relação entre a raça ou cor da criança e o baixo peso? Considere que os recémnascidos com peso abaixo de 2,5kg são classificados como "baixo peso".


# Agrupando as categorias parda e preta para compor a categoria negra (conforme Estatuto da Igualdade Racial)


# Categorização do baixo peso ao nascer
require(dply)

dados_raca =analise%>%
  dplyr::select(PESO,RACACOR)%>%
  
  #Agrupando as categorias parda e preta para compor a categoria negra (conforme Estatuto da Igualdade Racial)
  # Categoria Outra irá designar a etnia Amarela e Indigena
  
  mutate(RACACOR2= case_when(
    RACACOR%>% str_detect('1') ~ "Branca",
    RACACOR%>% str_detect('2') ~ "Negra",
    RACACOR%>% str_detect('3') ~ "Outra",
    RACACOR%>% str_detect('4') ~ "Negra",
    RACACOR%>% str_detect('5') ~ "Outra",))%>%
  
  # colocando PESO2 como numérico para efetuar os procedimentos lógicos e para parametros de calculo( summary) e parametros gráficos
  
  mutate(PESO2=as.numeric(as.character(analise$PESO)))%>%
  
  # separando em normal e baixo peso
  
  mutate(PESO_CAT = if_else(PESO2 >= 2500, "Peso adequado","Baixo peso"))%>% 
  
  # peso3 para a categorização de baixo de peso:
  
  mutate(PESO3=as.numeric(as.character(analise$PESO)))%>%  
  
  drop_na()          # retirando as não respostas


# Gerando a tabela de contingência para peso ao nascer segundo população (branca ou negra)

tabela_populacao_peso <- table(dados_raca$RACACOR2,dados_raca$PESO_CAT)
tabela_populacao_peso

addmargins(tabela_populacao_peso)
prop.table(tabela_populacao_peso,2)

# Calculando o qui-quadrado

qui_quadrado <- dados_raca[dados_raca$RACACOR %in% c(1, 2 , 4), ]

#install.packages('epiDisplay')
library(epiDisplay)

xtabs(~RACACOR2+PESO_CAT, data=qui_quadrado)
tabela1 <- xtabs(~RACACOR2+PESO_CAT, data=qui_quadrado)
chisq.test(tabela1, correct=FALSE)
tabpct(qui_quadrado$RACACOR2, qui_quadrado$PESO_CAT, percent ="col", graph = FALSE )


# Medidas de posição e dispersão de peso ao nascer segundo cor/raça  para  baixo peso e peso normal 
# Com o coeficiente de Variação incluso

summary(dados_raca$PESO2[dados_raca$RACACOR2=="Branca"])

summary(dados_raca$PESO2[dados_raca$RACACOR2=="Negra"]) 

# Cálculo do R2

table (dados_raca$RACACOR2)
tapply(dados_raca$PESO2,dados_raca$RACACOR2,var)
var_barra<-((315024.5*54)+(357532.1*94))/(nrow(dados_raca))
var_barra
var_peso<-var(dados_raca$PESO2)
var_peso
R2 <-((var_peso)-(var_barra))/(var_peso)
R2





# Trabalhando o baixo peso categorizando cada intervalo

dados_raca_baixo_peso =dados_raca%>%
  filter(PESO2<2500)%>%
  
  # Agora criamos uma coluna que ira se restringir apenas as raças branca e negra
  mutate(RACACOR3= case_when(
    RACACOR%>% str_detect('1') ~ "Branca",
    RACACOR%>% str_detect('2') ~ "Negra",
    RACACOR%>% str_detect('4') ~ "Negra"))%>%
  drop_na()






# Categorização do baixo peso ao nascer

#Usaremos Peso 3 como parametro para designar cada intervalo de peso

dados_raca_baixo_peso$PESO3[dados_raca_baixo_peso$PESO3<1000]<-"Extremo baixo peso"

dados_raca_baixo_peso$PESO3[dados_raca_baixo_peso$PESO3>=1000 & dados_raca_baixo_peso$PESO3<1500]<-"Muito baixo peso"

dados_raca_baixo_peso$PESO3[dados_raca_baixo_peso$PESO3>=1500 & dados_raca_baixo_peso$PESO3<2500]<-"Baixo peso"

tabela_raca_baixo_peso <- table(dados_raca_baixo_peso$PESO3,dados_raca_baixo_peso$RACACOR2)

# Efetuando a tabela de baixo_peso:

tabela_raca_baixo_peso

prop.table(tabela_raca_baixo_peso,2)

# Separando os vetores de peso da raça branca e negra com baixo peso em cada subset para obter todas as medidas:
# e por consegunte obtendo as suas respectivas medidas de posição e dispersão:

Branca=dados_raca_baixo_peso%>%
  filter(RACACOR2=='Branca')

summary(Branca$PESO2)

Negra=dados_raca_baixo_peso%>%
  filter(RACACOR2=='Negra')

summary(Negra$PESO2)

Negra_Branca= rbind(Negra,Branca)
# Boxplot para baixo peso ao nascer segundo raça/cor com as categorias Branca e Negra

ggplot(dados_raca_baixo_peso) +
  aes(x=factor(RACACOR2), y=PESO2) +
  geom_boxplot(fill=c("#66CC99"), width = 0.7) +
  guides(fill=FALSE) +
  labs(x="Raça/cor", y="Peso ao nascer (em g)")+
  theme()

table(dados_raca_baixo_peso$RACACOR2)  


ggplot(Negra_Branca)+
  aes(x=factor(RACACOR2), y=PESO2) +
  geom_boxplot(fill=c("#66CC99"), width = 0.7) +
  guides(fill=FALSE) +
  labs(x="Raça/cor", y="Peso ao nascer (em g)")+
  theme()





#3) Descrever a variável duração da gestação

# frequencia absoluta:
require(tidyverse)

gest=analise%>%
  dplyr::select(GESTACAO)%>%
  count(GESTACAO)%>%
  rename(frequencia_absoluta=n)%>%
  rename(Duração_Parto=GESTACAO)

# Calculando a frequencia relativa:

percent(gest$frequencia_absoluta)


# criando vetor para ser anexado posteriormente:

frequencia_relativa<- c(0.05, 0.65, 1.40, 9.10, 86.40, 2.40)


# efetuando a junção e colocando a % na freq_relativa 

gest_table= cbind(gest,frequencia_relativa)%>%
  mutate(
    freq = gsub("\\.", ",",frequencia_relativa) %>% paste("%", sep = "") %>% 
      str_squish())

# nomeando cada fator




dados_gestação =analise[analise$GESTACAO]%>%
  mutate(GESTACAO2= case_when(
    GESTACAO%>% str_detect('1') ~ "Prematuro",
    GESTACAO%>% str_detect('2') ~ "Prematuro",
    GESTACAO%>% str_detect('3') ~ "Prematuro",
    GESTACAO%>% str_detect('4') ~ "Prematuro",
    GESTACAO%>% str_detect('5') ~"Termo",
    GESTACAO%>% str_detect('6') ~ "Pós termo"))%>%
  dplyr::select(GESTACAO2)


# gráfico de setoress

Gráfico_setor = dados_gestação%>% 
  group_by(GESTACAO2) %>%
  dplyr::select(GESTACAO2)%>%
  
#Frquencia absoluta de cada categoria:
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(GESTACAO2)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)%>%
  rename(Categoria= GESTACAO2)



ggplot(Gráfico_setor) +
  aes(x = factor(""), y = Prop , fill = Categoria) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme()+
  labs(fill='')+
  theme_void() 


# analisando os 13.6% da amostra e separando suas respectivas categorias

dados_gestação_especifica =analise[analise$GESTACAO]%>%
  mutate(GESTACAO3= case_when(
    GESTACAO%>% str_detect('2') ~ "Prematuridade extrema",
    GESTACAO%>% str_detect('3') ~ "Prematuridade acentuada",
    GESTACAO%>% str_detect('4') ~ "Prematuridade moderada"))%>%
  dplyr::select(GESTACAO3)%>%
  drop_na()


# gráfico de setoress especifico

Gráfico_setor_especifico = dados_gestação_especifica%>% 
  group_by(GESTACAO3) %>%
  dplyr::select(GESTACAO3)%>%
  
#Frquencia absoluta de cada categoria:
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(GESTACAO3)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)%>%
  rename(Categoria= GESTACAO3)


ggplot(Gráfico_setor_especifico) +
  aes(x = factor(""), y = Prop , fill = Categoria) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  labs(fill='')+
  theme()+
  theme_void()
  




#4) Descrever a variável peso do recém-nascidos da amostra.  

#Iremos utilizar a coluna PESO2 do dados_raca para descrever o peso

peso_nascidos=dados_raca%>%
  # Criando uma coluna para dividir o PESO
  mutate(Peso_Intervalos=PESO2)



# Medidas de posição e dispersão da variável peso ao nascer 

summary(peso_nascidos$PESO2)


# Efetuando a tabela:


tabela=peso_nascidos$Peso_Intervalos <- cut(peso_nascidos$Peso_Intervalos, breaks=c(0,2500,3000,4000,5100), 
                               labels=c('0|-2.500 ','2.500|-3.000 ','3.000|-4.000 ','4.000|-5.1000 '))

table(tabela)

# criando um vetor para saber o percentual de cada classe:

percentual_peso<-c(150,423,1316,111)

percent(teste)


# Representando no Histograma:

ggplot(data = peso_nascidos, aes(x = PESO2)) +
  geom_histogram( bins=20, fill='lightblue', color ='black')+
  labs(title="", x="Peso",y="Frequência absoluta")

#5) Pode-se dizer que o peso da recém-nascido está relacionado a seguintes variáveis? Se sim,como? 

#analise$ IDADEMAE<- unfactor(analise$IDADEMAE)
#analise$ESCMAE<- unfactor(analise$ESCMAE)
#analise$RACACOR<- unfactor(analise$RACACOR)

# Utilizaremos o PESO2 de dados_raca para efetuar todas as comparações de peso  (dados_raca$PESO2):


#a. Idade da mãe

# Acrescentando a IDADEMAE2 para mudar a natureza de IDADEMAE

analise$IDADEMAE2=as.numeric(as.character(analise$IDADEMAE))
class(analise$IDADEMAE2)


dados5a= as.data.frame(cbind(analise$IDADEMAE2,dados_raca$PESO2))%>%
                      #Renomeando as colunas
  rename(Idade=V1)%>%
  rename(PESO=V2)%>%
                        #Categorizando a idade da mãe:
  mutate(Idade_CAT = if_else(Idade >= 19, "Adulta" ,"Adolescente"))
  

# Boxplot para peso ao nascer segundo idade da mãe


ggplot(dados5a) +
  aes(x=factor(Idade_CAT), y=PESO) +
  geom_boxplot(fill=c("#66CC99"), width = 0.7) +
  guides(fill=FALSE) +
  labs(x="Raça/cor", y="Peso ao nascer (em g)")+
  theme()


# Fazendo o diagrama de dispersão 

ggplot(dados5a) +
  aes(x = PESO, y = Idade) +
  geom_point(colour = "#FF6600", size = 3) +
  labs(
    x = 'Peso ao nascer',
    y = 'Idade da mãe'
  ) +
  theme()



#Calculando o coeficiente de correlação


cor(dados5a$PESO,dados5a$Idade, method='pearson')


# Medidas resumo para idade da mãe

summary(dados5a$Idade)




#b. Escolaridade da mãe;

# Acrescentando a ESCMAE2 para mudar a natureza de IDADEMAE

analise$ESCMAE2=as.numeric(as.character(analise$ESCMAE))
class(analise$IDADEMAE2)


dados5b= as.data.frame(cbind(analise$ESCMAE2,dados_raca$PESO2))%>%
  
  #Renomeando as colunas
  
  rename(Escolaridade=V1)%>%
  rename(PESO=V2)%>%            
  
  #Categorizando a escolaridade:
  
  mutate(Escolaridade_CAT = if_else(Escolaridade >= 4, "Alta escolaridade" ,"Baixa escolaridade"))  



#Separando em dois data,frames:

Alta_esc=dados5b%>%
  filter(Escolaridade_CAT=='Alta escolaridade')%>%
  dplyr::select(PESO,Escolaridade_CAT)

Baixa_esc=dados5b%>%
  filter(Escolaridade_CAT=="Baixa escolaridade")%>%
  dplyr::select(PESO,Escolaridade_CAT)
  

# Histogramas para cada tipo de escolaridade



hist(Alta_esc$PESO,
     main = "",
     xlab = "Peso",ylab = "Frequência",
     col=c("green"),
     ylim =c(0,1000),
     labels = TRUE)

hist(Baixa_esc$PESO,
     main = "",
     xlab = "Peso",ylab = "Frequência",
     col=c("red"),
     ylim =c(0,200),
     labels = TRUE)



# Medidas de posição e dispersão de peso ao nascer segundo tipo de escolaridade  

# Com o coeficiente de Variação incluso

summary(Alta_esc$PESO)

summary(Baixa_esc$PESO)





  
#c. Raça ou cor da criança
  

  
  
# Como iremos trabalhar com a variável peso reaproveitaremos o banco dados_raça que contempla a categorização de peso e raça
  
ggplot(dados_raca) +
  aes(x=factor(RACACOR2), y=PESO2) +
  geom_boxplot(fill=c("#66CC99"), width = 0.7) +
  guides(fill=FALSE) +
  labs(x="Raça/cor", y="Peso ao nascer (em g)")+
  theme()




# Medidas de posição e dispersão de peso ao nascer segundo tipo de escolaridade  

summary(dados_raca$PESO2)



