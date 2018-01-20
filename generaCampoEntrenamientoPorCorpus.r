install.packages("textstem")
debugonce(devtools::install)
# Packages & Libraries
library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(data.table)
library(SnowballC)
library(textstem)

word.freq <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  # construct word frequency df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

preparaDatos <- function(twit, resultado, tema, palabras)
{
  print(tema)
  
  nrowMessages <- nrow(twit)
  
  print(nrowMessages)
  nrowWord <- ncol(resultado)

  for(k in 1:(nrowMessages) ) 
  { 
    # Del dataset nos quedamos con una tupla, o un mensaje de twitter
    texto_primera_fila <- word.freq(twit[k, 1])
  
    nrowPrimeraFila <- nrow(texto_primera_fila)

    for (i in 1:nrowPrimeraFila )
    {
      # Por cada mensaje, lo subdividimos en palabras
      palabra <- as.String(texto_primera_fila[i,1])
     
      for(j in 1:nrowWord)
      {

        if(palabra == as.String(resultado[1,j]))
        {
          resultado[k+2, j] <- resultado[2,j]
        }
      }
    }
  }
  
  resultado <- resultado[3:nrow(resultado),] #Eliminamos las dos primeras filas porque teniamos valores que no nos servían

  # Eliminamos los NA generados

  for( i in 1:nrow(resultado))
  {
    for( j in 1:ncol(resultado))
    {
      if( as.String(resultado[i,j]) == as.String("NA"))
      {
        resultado[i,j] = 0
      }
    }
  }
  
  print(nrow(resultado))
  
  ruta_relativa = paste0("./data/dataset_", palabras)
    
  write.table(resultado, file = toString(paste(ruta_relativa, tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")  
}


Messages <- read.csv("./data/Dataset.csv")

palabras = 20

for( tema in levels(Messages$Corpus))
{
  # tema = "Twitter"
  ruta_relativa = paste0("./data/word_", palabras)
  word <- read.csv(file = toString(paste(ruta_relativa, tema ,".csv", sep = " ")))
  resultado <- data.frame(matrix(ncol = palabras, nrow = 502), stringsAsFactors=FALSE)

  # Escribimos todas las palabras posibles en la primera fila de la matriz resultado
  # combinacion_palabras_posibles(word, resultado)

  # Añadimos el tf idf debajo de cada palabra correspondiente para poder hacer posteriormente su comparación
  for(i in 1: nrow(word))
  {
    resultado[1, i] <- toString(word[i, 1])
    resultado[2, i] <- toString(word[i, 2])
  }

  write.table(resultado, file = toString(paste("./data/matrizTFIDF", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")

  matriz <- read.csv(file = toString(paste("./data/matrizTFIDF", tema ,".csv", sep = " ")), stringsAsFactors=FALSE)
  
  mensajesDelTema = as.data.frame(Messages$Text[Messages$Corpus == tema]) 
  preparaDatos(mensajesDelTema, matriz, tema, palabras)
}

