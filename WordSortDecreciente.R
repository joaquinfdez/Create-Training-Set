
Messages <- read.csv("./data/Dataset.csv")
palabras = 20

for( tema in levels(Messages$Corpus))
{
  word <- read.csv(file = toString(paste("./data/word", tema ,".csv", sep = " ")))
  
  word <- word[ 
        with(word, order(c("tfidf", "word")))
    ]
  
  word <- word[1:palabras, ]
  
  ruta_relativa = paste0("./data/word_", palabras)
  
  write.table(word, file = toString(paste(ruta_relativa, tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
}


