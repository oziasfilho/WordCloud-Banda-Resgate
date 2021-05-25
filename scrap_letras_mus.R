library(tidyverse)
library(httr)
library(rvest)
library(stringdist)
library(wordcloud2)
link<-"https://www.letras.mus.br/resgate/"

pega_links <- function(url){
  # Lendo HTML
  site <- url %>% read_html()
  
  # Extraindo os links
  link <- site %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  
  tibble(url = link)
}

dados<-link %>% pega_links()

dados<-dados %>% filter(str_detect(dados$url,"^/resgate/"),
                        str_detect(dados$url,"/$")) 

dados<- dados %>% filter(dados$url!="/resgate/",
                         str_detect(dados$url,"/discografia/")==FALSE)


dados$link_comp<-paste0("https://www.letras.mus.br",dados$url)

dados<- dados$link_comp

extrair_musicas<-function(url){
  x<-url %>% read_html()
  
  titulo<- x %>% 
    html_nodes("div.cnt-head_title") %>% 
    html_text()
  
  musica<- x %>% 
    html_nodes("div.cnt-letra.p402_premium") %>% 
    html_text2() %>%
    str_replace_all("\n", " ")
  
  tibble(link = url, nome = titulo, letra = musica)
}

df_teste<-dados %>% map_df(extrair_musicas) %>% unique()
df_teste$nome<-str_remove(df_teste$nome,"Resgate")
df_teste$nome<-str_squish(df_teste$nome)

##### Tratamento de Strings #####
df_teste$letra<- str_to_lower(df_teste$letra) %>% str_squish()

vetor<-c("a","à","adeus","agora","aí","ainda","além","algo",
         "alguém","algum","alguma","algumas","alguns","ali","ampla",
         "amplas","amplo","amplos","ano","anos","ante","antes","ao",
         "aos","apenas","apoio","após","aquela","aquelas","aquele",
         "aqueles","aqui","aquilo","área","as","às","assim","até",
         "atrás","através","baixo","bastante","bem","boa","boas",
         "bom","bons","breve","cá","cada","catorze","cedo","cento",
         "certamente","certeza","cima","cinco","coisa","coisas","com",
         "como","conselho","contra","contudo","custa","da","dá","dão",
         "daquela","daquelas","daquele","daqueles","dar","das","de","debaixo","dela","delas",
         "dele","deles","demais","dentro","depois","desde","dessa","dessas","desse","desses",
         "desta","destas","deste","destes","deve","devem","devendo","dever","deverá","deverão",
         "deveria","deveriam","devia","deviam","dez","dezanove","dezesseis","dezessete","dezoito",
         "dia","diante","disse","disso","disto","dito","diz","dizem","dizer","do","dois","dos",
         "doze","duas","dúvida","e","é","ela","elas","ele","eles","em","embora","enquanto","entre",
         "era","eram","éramos","és","essa","essas","esse","esses","esta","está","estamos","estão",
         "estar","estas","estás","estava","estavam","estávamos","este","esteja","estejam","estejamos",
         "estes","esteve","estive","estivemos","estiver","estivera","estiveram","estivéramos","estiverem",
         "estivermos","estivesse","estivessem","estivéssemos","estiveste","estivestes","estou","etc","eu",
         "exemplo","faço","falta","favor","faz","fazeis","fazem","fazemos","fazendo","fazer","fazes","feita",
         "feitas","feito","feitos","fez","fim","final","foi","fomos","for","fora","foram","fôramos","forem",
         "forma","formos","fosse","fossem","fôssemos","foste","fostes","fui","geral","grande","grandes","grupo",
         "há","haja","hajam","hajamos","hão","havemos","havia","hei","hoje","hora","horas","houve","houvemos","houver",
         "houvera","houverá","houveram","houvéramos","houverão","houverei","houverem","houveremos","houveria","houveriam",
         "houveríamos","houvermos","houvesse","houvessem","houvéssemos","isso","isto","já","la","lá","lado","lhe","lhes",
         "lo","local","logo","longe","lugar","maior","maioria","mais","mal","mas","máximo","me","meio","menor","menos",
         "mês","meses","mesma","mesmas","mesmo","mesmos","meu","meus","mil","minha","minhas","momento","muita","muitas",
         "muito","muitos","na","nada","não","naquela","naquelas","naquele","naqueles","nas","nem","nenhum","nenhuma","nessa",
         "nessas","nesse","nesses","nesta","nestas","neste","nestes","ninguém","nível","no","noite","nome","nos","nós","nossa",
         "nossas","nosso","nossos","nova","novas","nove","novo","novos","num","numa","número","nunca","o","obra","obrigada","obrigado",
         "oitava","oitavo","oito","onde","ontem","onze","os","ou","outra","outras","outro","outros","para","parece","parte","partir",
         "paucas","pela","pelas","pelo","pelos","pequena","pequenas","pequeno","pequenos","per","perante","perto","pode","pude","pôde",
         "podem","podendo","poder","poderia","poderiam","podia","podiam","põe","põem","pois","ponto","pontos","por","porém","porque","porquê",
         "posição","possível","possivelmente","posso","pouca","poucas","pouco","poucos","primeira","primeiras","primeiro","primeiros","própria",
         "próprias","próprio","próprios","próxima","próximas","próximo","próximos","pude","puderam","quais","quáis","qual","quando","quanto",
         "quantos","quarta","quarto","quatro","que","quê","quem","quer","quereis","querem","queremas","queres","quero","questão","quinta",
         "quinto","quinze","relação","sabe","sabem","são","se","segunda","segundo","sei","seis","seja","sejam","sejamos","sem","sempre",
         "sendo","ser","será","serão","serei","seremos","seria","seriam","seríamos","sete","sétima","sétimo","seu","seus","sexta","sexto",
         "si","sido","sim","sistema","só","sob","sobre","sois","somos","sou","sua","suas","tal","talvez","também","tampouco","tanta",
         "tantas","tanto","tão","tarde","te","tem","tém","têm","temos","tendes","tendo","tenha","tenham","tenhamos","tenho","tens","ter",
         "terá","terão","terceira","terceiro","terei","teremos","teria","teriam","teríamos","teu","teus","teve","ti","tido","tinha","tinham",
         "tínhamos","tive","tivemos","tiver","tivera","tiveram","tivéramos","tiverem","tivermos","tivesse","tivessem","tivéssemos","tiveste",
         "tivestes","toda","todas","todavia","todo","todos","trabalho","três","treze","tu","tua","tuas","tudo","última","últimas","último",
         "últimos","um","uma","umas","uns","vai","vais","vão","vários","vem","vêm","vendo","vens","ver","vez","vezes","viagem","vindo",
         "vinte","vir","você","vocês","vos","vós","vossa","vossas","vosso","vossos","zero")

#Removendo Stopwords###
df_teste$letra<-tm::removeWords(df_teste$letra,stopwords("pt"))
df_teste$letra<-tm::removeWords(df_teste$letra,stopwords("en"))
df_teste$letra<-tm::removeWords(df_teste$letra,"pra")
df_teste$letra<-tm::removeWords(df_teste$letra,vetor)

#### Removendo Pontuação ####
df_teste$letra<-tm::removePunctuation(df_teste$letra)

### Removendo espaços extras ####
df_teste$letra<- str_squish(df_teste$letra)


df<-df_teste %>% select(nome,letra)
df<-as.data.frame(df)
rownames(df)<-df$nome


docs <- tm::Corpus(VectorSource(df$letra))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
palavras <- data.frame(word = names(words),freq=words)

palavras %>% top_n(15) %>% 
  ggplot(aes(x=reorder(word,freq),y=freq,label = freq)) + 
  geom_bar(stat = "identity") + coord_flip() +
  labs(title = "Top 15 Palavras Banda Resgate") + 
  xlab("Palavras") + ylab("Frequência") + theme_minimal() + 
  geom_text(vjust = -0.05, hjust=1.3,    # nudge above top of bar
            size = 3,color="white",fontface="bold") 


wordcloud2(palavras)

figPath="resgate.png"
wordcloud2(palavras, figPath = figPath, size = 1.5,
           color = "white",backgroundColor = "grey")
