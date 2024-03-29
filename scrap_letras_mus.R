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

vetor<-c("a","�","adeus","agora","a�","ainda","al�m","algo",
         "algu�m","algum","alguma","algumas","alguns","ali","ampla",
         "amplas","amplo","amplos","ano","anos","ante","antes","ao",
         "aos","apenas","apoio","ap�s","aquela","aquelas","aquele",
         "aqueles","aqui","aquilo","�rea","as","�s","assim","at�",
         "atr�s","atrav�s","baixo","bastante","bem","boa","boas",
         "bom","bons","breve","c�","cada","catorze","cedo","cento",
         "certamente","certeza","cima","cinco","coisa","coisas","com",
         "como","conselho","contra","contudo","custa","da","d�","d�o",
         "daquela","daquelas","daquele","daqueles","dar","das","de","debaixo","dela","delas",
         "dele","deles","demais","dentro","depois","desde","dessa","dessas","desse","desses",
         "desta","destas","deste","destes","deve","devem","devendo","dever","dever�","dever�o",
         "deveria","deveriam","devia","deviam","dez","dezanove","dezesseis","dezessete","dezoito",
         "dia","diante","disse","disso","disto","dito","diz","dizem","dizer","do","dois","dos",
         "doze","duas","d�vida","e","�","ela","elas","ele","eles","em","embora","enquanto","entre",
         "era","eram","�ramos","�s","essa","essas","esse","esses","esta","est�","estamos","est�o",
         "estar","estas","est�s","estava","estavam","est�vamos","este","esteja","estejam","estejamos",
         "estes","esteve","estive","estivemos","estiver","estivera","estiveram","estiv�ramos","estiverem",
         "estivermos","estivesse","estivessem","estiv�ssemos","estiveste","estivestes","estou","etc","eu",
         "exemplo","fa�o","falta","favor","faz","fazeis","fazem","fazemos","fazendo","fazer","fazes","feita",
         "feitas","feito","feitos","fez","fim","final","foi","fomos","for","fora","foram","f�ramos","forem",
         "forma","formos","fosse","fossem","f�ssemos","foste","fostes","fui","geral","grande","grandes","grupo",
         "h�","haja","hajam","hajamos","h�o","havemos","havia","hei","hoje","hora","horas","houve","houvemos","houver",
         "houvera","houver�","houveram","houv�ramos","houver�o","houverei","houverem","houveremos","houveria","houveriam",
         "houver�amos","houvermos","houvesse","houvessem","houv�ssemos","isso","isto","j�","la","l�","lado","lhe","lhes",
         "lo","local","logo","longe","lugar","maior","maioria","mais","mal","mas","m�ximo","me","meio","menor","menos",
         "m�s","meses","mesma","mesmas","mesmo","mesmos","meu","meus","mil","minha","minhas","momento","muita","muitas",
         "muito","muitos","na","nada","n�o","naquela","naquelas","naquele","naqueles","nas","nem","nenhum","nenhuma","nessa",
         "nessas","nesse","nesses","nesta","nestas","neste","nestes","ningu�m","n�vel","no","noite","nome","nos","n�s","nossa",
         "nossas","nosso","nossos","nova","novas","nove","novo","novos","num","numa","n�mero","nunca","o","obra","obrigada","obrigado",
         "oitava","oitavo","oito","onde","ontem","onze","os","ou","outra","outras","outro","outros","para","parece","parte","partir",
         "paucas","pela","pelas","pelo","pelos","pequena","pequenas","pequeno","pequenos","per","perante","perto","pode","pude","p�de",
         "podem","podendo","poder","poderia","poderiam","podia","podiam","p�e","p�em","pois","ponto","pontos","por","por�m","porque","porqu�",
         "posi��o","poss�vel","possivelmente","posso","pouca","poucas","pouco","poucos","primeira","primeiras","primeiro","primeiros","pr�pria",
         "pr�prias","pr�prio","pr�prios","pr�xima","pr�ximas","pr�ximo","pr�ximos","pude","puderam","quais","qu�is","qual","quando","quanto",
         "quantos","quarta","quarto","quatro","que","qu�","quem","quer","quereis","querem","queremas","queres","quero","quest�o","quinta",
         "quinto","quinze","rela��o","sabe","sabem","s�o","se","segunda","segundo","sei","seis","seja","sejam","sejamos","sem","sempre",
         "sendo","ser","ser�","ser�o","serei","seremos","seria","seriam","ser�amos","sete","s�tima","s�timo","seu","seus","sexta","sexto",
         "si","sido","sim","sistema","s�","sob","sobre","sois","somos","sou","sua","suas","tal","talvez","tamb�m","tampouco","tanta",
         "tantas","tanto","t�o","tarde","te","tem","t�m","t�m","temos","tendes","tendo","tenha","tenham","tenhamos","tenho","tens","ter",
         "ter�","ter�o","terceira","terceiro","terei","teremos","teria","teriam","ter�amos","teu","teus","teve","ti","tido","tinha","tinham",
         "t�nhamos","tive","tivemos","tiver","tivera","tiveram","tiv�ramos","tiverem","tivermos","tivesse","tivessem","tiv�ssemos","tiveste",
         "tivestes","toda","todas","todavia","todo","todos","trabalho","tr�s","treze","tu","tua","tuas","tudo","�ltima","�ltimas","�ltimo",
         "�ltimos","um","uma","umas","uns","vai","vais","v�o","v�rios","vem","v�m","vendo","vens","ver","vez","vezes","viagem","vindo",
         "vinte","vir","voc�","voc�s","vos","v�s","vossa","vossas","vosso","vossos","zero")

#Removendo Stopwords###
df_teste$letra<-tm::removeWords(df_teste$letra,stopwords("pt"))
df_teste$letra<-tm::removeWords(df_teste$letra,stopwords("en"))
df_teste$letra<-tm::removeWords(df_teste$letra,"pra")
df_teste$letra<-tm::removeWords(df_teste$letra,vetor)

#### Removendo Pontua��o ####
df_teste$letra<-tm::removePunctuation(df_teste$letra)

### Removendo espa�os extras ####
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
  xlab("Palavras") + ylab("Frequ�ncia") + theme_minimal() + 
  geom_text(vjust = -0.05, hjust=1.3,    # nudge above top of bar
            size = 3,color="white",fontface="bold") 


wordcloud2(palavras)

figPath="resgate.png"
wordcloud2(palavras, figPath = figPath, size = 1.5,
           color = "white",backgroundColor = "grey")
