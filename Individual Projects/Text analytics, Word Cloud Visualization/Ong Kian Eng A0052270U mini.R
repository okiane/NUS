#Ong Kian Eng A0052270U

#Set working directory
  setwd('E:/NUS/EBAC/EBA5002 Business Analytics Practice/3 TA Text Analytics/mini project')
  
#Load package and initialization
  pacman::p_load(topicmodels, tm, ggplot2, SnowballC, wordcloud, RColorBrewer, tidyverse, rJava, coreNLP, wordnet, xlsx, tidytext, rcorpora)
  
  #Java package
    # https://java.com/en/download/manual.jsp
    Sys.setenv(JAVA_HOME="") #Find Java on machine
  
  
  #coreNLP package
    #coreNLP::downloadCoreNLP() #Java application
    #http://corenlp.run/
    
    # initialize CoreNLP with 'fast' settings, only using the annotators we need
    # (This requires changing the properties file which can be found in the directory
    # where coreNLP package is located,
    # e.g. "C:\Users\username\Documents\R\win-library\3.4\coreNLP\extdata"): 
    # annotators = tokenize, ssplit, pos, lemma, ner
    # suitable when your machine doesn't have much memory (typically need >=4g)
    
    #Add ner (Named Entity Recognition) to english_fast
    #Lemmatization - bring to base forms
    
    initCoreNLP(type="english_fast") #Can choose english_all but will be memory intensive

  
  #Word cloud parameters
      dark2 <- brewer.pal(6, "Dark2")
  
      
  #WordNet parameters
    #Set Dictionary Directory
    #R will complain it can not find WordNet "dict" directory. Don't worry. We set it here.
    setDict("C:/Program Files (x86)/WordNet/2.1/dict")

#====Preparation for Document Term Matrix (DTM)====  
#User-defined function Hyponym 
  #how to get hyponyms(children words) of a term 
  hyponyms <- function(x){
    filter <- getTermFilter("ExactMatchFilter", word = x, ignoreCase = TRUE) #Exact match of terms for search
    terms <- getIndexTerms("NOUN", 1, filter) #Obtain nouns
    synsets <- getSynsets(terms[[1]]) #Obtain sets of words
    related <- tryCatch(
      getRelatedSynsets(synsets[[1]], "~"),
      error = function(condition) {
        if (condition$message == "RcallMethod: invalid object parameter")
          message("No direct hyponyms found")
        else
          stop(condition)
        return(NULL)
      }
    )
    
    if (is.null(related))
      return(NULL)
    return(unlist(sapply(related, getWord)))
  }

  
  #Create list of words for Valid Term Matrix
    occ <- unique(c(hyponyms("worker"), hyponyms("job"), hyponyms("employee"), hyponyms("worker"), "electrician")) #Unique to consolidate duplicates
    
    bp <- unique(c(hyponyms("external body part"), hyponyms("body part"), hyponyms("extremity"), hyponyms("limb"), hyponyms("hand"), hyponyms("finger"), hyponyms("organ"), hyponyms("organs"), hyponyms("bone"), hyponyms("eye")))
    
    acc <- unique(c(hyponyms("accident"), hyponyms("injury"), hyponyms("abrasion"), "abrasion", "fall"))
    
  #Find synonyms
    occS = list()
    bpS = list()
    accS = list()
    
    for (i in 1:(length(occ))) {
      occS <- c(occS, synonyms(occ[i], "N"))
    }
    occ <- unique(c(occ, occS))
    rm(occS)
    
    
    for (i in 1:(length(bp))) {
      bpS <- c(bpS, synonyms(bp[i], "N"))
    }
    bp <- unique(c(bp, bpS))
    rm(bpS)
    
    
    for (i in 1:(length(acc))) {
      accS <- c(accS, synonyms(acc[i], "N"))
    }
    acc <- unique(c(acc, accS))
    rm(accS)
    
    
  #Check if certain terms are found in the list
    occ[grep("electrician", occ)] #Search terms
    bp[grep("leg", bp)] #Search terms
    acc[grep("fall", acc)] #Search terms
    
    
#====Load data====
  #Load data
    textdata <- read.delim("osha.txt", header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
    


#====Create annotation of text based on nouns or verbs====
  #Get source column to annotate
    text <- textdata[, "V3"] #First 100 records
    #text <- split(textdata[, "V3"], 1:10000)
    
  #Load annotated text, Else create annotation
    if (file.exists("annotated text.csv")) {
      print ("Yays! Annotated text exists. Opening file.")
      
      #Import annotation from file
      tok <- read.csv("annotated text.csv", header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
      
    } else {
      
      #Export annotation into a file to save memory
      for (i in 1:(length(text))) {
        #tok = getToken(annotateString(text[1:100])) #Get annotations of tokens
        #tok = rbind(tok, getToken(annotateString(text[(1+100*(i-1)):100*i]))) #Get annotations of tokens
        
        write.table(getToken(annotateString(text[i])), file = "annotated text.csv", sep=",", append = TRUE, row.names = FALSE, col.names = FALSE) 
    
      }
      tok <- read.csv("annotated text.csv", header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
    }
    
  
  #Convert column names  
    colnames(tok) <- c("sentence", "id", "token", "lemma", "CharacterOffsetBegin", "CharacterOffsetEnd", "POS", "NER", "Speaker") #Add column names
    
  #Remove unnecessary information on the position of the word 
    tok <- select(tok, -CharacterOffsetBegin, -CharacterOffsetEnd, -NER, - Speaker) 
    #NER O(Outside) if the word is not a named entity
    
    
  # # Then, create a utility function to combine together tokens belonging to the same entity
  # getEntities = function(ner) {
  #   ner <- ner[ner[, "NER"] != "O",c(2, 3, 8)]
  #   b4 <- ner[1, ]
  #   cur <- ner[1, ]
  #   entity <- ""
  #   entities <- data.frame(entity="", type="", stringsAsFactors=FALSE)
  #   
  #   for (i in 1:nrow(ner)) {
  #     cur <- ner[i, ]
  #     
  #     if (as.numeric(cur["id"]) - 1 == as.numeric(b4["id"]) & cur["NER"] == b4["NER"]) {
  #       entity <- paste(entity, cur["token"], sep = "_")
  #     } else {
  #       entities <- rbind(entities, c(entity, b4[1, "NER"]))
  #       entity <- cur[1, "token"]
  #       
  #     }
  #     b4 <- cur
  #   }
  #   entities <- rbind(entities, c(entity, b4[1, "NER"]))
  #   entities[entities[,"entity"]!= "", ]
  # }
  # 
  # # Get the entities!
  # ent <- getEntities(tok)
  # ent
  
    
  #User defined functions to get Nouns and Verbs
    # create a small utility function to annotate a string using CoreNLP, and get lemmas of nouns
    # !!!Remember, we can only call this function after CoreNLP is initialized
    # getNouns = function (x) {
    #   tok <- getToken(annotateString(x))
    #   lem <- unlist(tok[startsWith(tok[, "POS"], "N"), "lemma"])
    # }
    # 
    # getVerbs = function (x) {
    #   tok <- getToken(annotateString(x))
    #   lem <- unlist(tok[startsWith(tok[, "POS"], "V"), "lemma"])
    # }
  

  #Get lemmas from every comment
    #lemmaN <- sapply(ann_text, getNouns) #Nouns lemma
    #head(lemmaN)
    
    #lemmaV <- sapply(ann_text, getVerbs) #Verbs lemma
    #head(lemmaV)
    
    #Save files
      #write.csv(as.matrix(lemmaN), file = "lemmaN.csv")
      #write.csv(as.matrix(lemmaV), file = "lemmaV.csv")
  
    #Obtain list of lemmas
      lemmaN <- tok[startsWith(tok[, "POS"], "N"), "lemma"]
    
  #Create corpus using lemmas
    corpusN <- VCorpus(VectorSource(lemmaN)) 
  
#====Data Exploration====
  #Number of records
    glimpse(textdata)
    
    
  #Length of reports
    DocLen <- sapply(text, function(x) length(strsplit(x, " ")[[1]]))
    table(DocLen) #Number of words #Number of reports
    summary(DocLen)
    hist(DocLen)
    

#====Accidents====  
  #Create stopwords to remove common words
    stopwords_acc <- c(stopwords("english"), "will", "also", "etc", "else", "can", "even", "within", "without", "well", "say", "year", "must", "need", "never", "now", "want", "still", "time", "therefore", "send", "today", "may", "many", "make", "whose", "however", "get", "have", "just", "him", "employe", "employee", "employ", "work", "cowork", "approxim", "area", "hospit", "injur", "use", "area", "system", "index", "left", "right", "contractor", "process", "bottom", "seat", "member", "structure", "injury", "wrench", "pinch", "break")
    
    
  #create valid document term matrix, without stemming (No need tm_map)
    dtm_acc <- DocumentTermMatrix(corpusN, control = list(dictionary = acc, #Find matching words in column
                                                         removeNumbers = TRUE,
                                                         removePunctuation = TRUE,
                                                         stripWhitespace = TRUE,
                                                         tolower = TRUE,
                                                         stopwords = stopwords_acc,
                                                         stemming = FALSE,
                                                         weighting = weightBin)) #Binary indexing
    
  #corpus <- tm_map(corpus, content_transformer(tolower), removeNumbers, removePunctuation, stripWhitespace, stemDocument, removeWords, my_stopwords)
    
    list_acc <- sort(colSums(as.matrix(dtm_acc)), decreasing = TRUE) #Sum each column
    
    wordcloud(names(list_acc), list_acc, max.words = 20, colors = dark2, min.freq = 5) #Display top 20
    #scale = c(2, .04)
    
    
    #Create bar charts
      data.frame(term = names(list_acc), freq = list_acc) %>%
        filter(freq > 0) %>%
        arrange(freq) %>%
        top_n(10) %>%
        
        ggplot(aes(x = reorder(term, freq), y = freq)) + 
        geom_bar(stat= "identity") + 
        geom_text(aes(label = freq,hjust=-0.15)) +
        labs(x = "Type", y = "Count") +
        coord_flip()
  
  #Save DTM
    if (file.exists("dtm_acc.csv")) {
      print("Yays! DTM file exists. No file saved.")
    } else {
        write.csv(as.matrix(dtm_acc), file = "dtm_acc.csv")
    }
    
      
#====Occupation====
  #Create stopwords to remove common words
    stopwords_occ <- c(stopwords("english"), "will", "also", "etc", "else", "can", "even", "within", "without", "well", "say", "year", "must", "need", "never", "now", "want", "still", "time", "therefore", "send", "today", "may", "many", "make", "whose", "however", "get", "have", "just", "him", "employe", "employee", "employ", "work", "cowork", "approxim", "area", "hospit", "injur", "use", "area", "system", "index", "left", "right", "process", "bottom", "seat", "member", "structure", "position", "place", "post", "help", "spot", "office", "situation")
  
  
  #create valid document term matrix, without stemming (No need tm_map)
    dtm_occ <- DocumentTermMatrix(corpusN, control = list(dictionary = occ, #Find matching words in column
                                                      removeNumbers = TRUE,
                                                      removePunctuation = TRUE,
                                                      stripWhitespace = TRUE,
                                                      tolower = TRUE,
                                                      stopwords = stopwords_occ,
                                                      stemming = FALSE,
                                                      weighting = weightBin)) #Binary indexing
  
    #corpus <- tm_map(corpus, content_transformer(tolower), removeNumbers, removePunctuation, stripWhitespace, stemDocument, removeWords, my_stopwords)
    
    list_occ <- sort(colSums(as.matrix(dtm_occ)), decreasing = TRUE) #Sum each column
    
    wordcloud(names(list_occ), list_occ, max.words = 20, colors = dark2, min.freq = 5) #Display top 20
    #scale = c(2, .04)
    
    
    #Create bar charts
      data.frame(term = names(list_occ), freq = list_occ) %>%
        filter(freq > 0) %>%
        arrange(freq) %>%
        top_n(10) %>%
        
        ggplot(aes(x = reorder(term, freq), y = freq)) + 
        geom_bar(stat= "identity") + 
        geom_text(aes(label = freq,hjust=-0.15)) +
        labs(x = "Type", y = "Count") +
        coord_flip()
    
    #Save DTM
      if (file.exists("dtm_occ.csv")) {
        print("Yays! DTM file exists. No file saved.")
      } else {
        write.csv(as.matrix(dtm_occ), file = "dtm_occ.csv")
      }   
      
#====Body parts====  
  #Create stopwords to remove common words
    stopwords_bp <- c(stopwords("english"), "will", "also", "etc", "else", "can", "even", "within", "without", "well", "say", "year", "must", "need", "never", "now", "want", "still", "time", "therefore", "send", "today", "may", "many", "make", "whose", "however", "get", "have", "just", "him", "employe", "employee", "employ", "work", "cowork", "approxim", "area", "hospit", "injur", "use", "area", "system", "index", "left", "right", "contractor", "process", "bottom", "seat", "member", "structure")
    
    
  #create valid document term matrix, without stemming (No need tm_map)
    dtm_bp <- DocumentTermMatrix(corpusN, control = list(dictionary = bp, #Find matching words in column
                                                       removeNumbers = TRUE,
                                                       removePunctuation = TRUE,
                                                       stripWhitespace = TRUE,
                                                       tolower = TRUE,
                                                       stopwords = stopwords_bp,
                                                       stemming = FALSE,
                                                       weighting = weightBin)) #Binary indexing
    
    #corpus <- tm_map(corpus, content_transformer(tolower), removeNumbers, removePunctuation, stripWhitespace, stemDocument, removeWords, my_stopwords)
  
    list_bp <- sort(colSums(as.matrix(dtm_bp)), decreasing = TRUE) #Sum each column
  
    wordcloud(names(list_bp), list_bp, max.words = 20, colors = dark2, min.freq = 5) #Display top 20
    #scale = c(2, .04)
    
  
    #Create bar charts
      data.frame(term = names(list_bp), freq = list_bp) %>%
        filter(freq > 0) %>%
        arrange(freq) %>%
        top_n(10) %>%
        
        ggplot(aes(x = reorder(term, freq), y = freq)) + 
        geom_bar(stat= "identity") + 
        geom_text(aes(label = freq,hjust=-0.15)) +
        labs(x = "Type", y = "Count") +
        coord_flip()

    #Save DTM
      if (file.exists("dtm_bp.csv")) {
        print("Yays! DTM file exists. No file saved.")
      } else {
        write.csv(as.matrix(dtm_bp), file = "dtm_bp.csv")
      }

  
#====Topic modeling=========
  #Open DTM, Else create DTM
      if (file.exists("dtm_acc_freq.csv")) {
        print("Yays! DTM file exists")
        dtm_acc_freq <- read.csv("dtm_acc_freq.csv")
      } else {
        
      #Create DTM using original text to find out association of words based on frequencies
        dtm_acc_freq <- DocumentTermMatrix(VCorpus(VectorSource(text)), control = list(
          removeNumbers = TRUE,
          removePunctuation = TRUE,
          stripWhitespace = TRUE,
          tolower = TRUE,
          stopwords = stopwords_acc,
          stemming = FALSE))
        
      #Save DTM
        write.csv(as.matrix(dtm_acc_freq), "dtm_acc_freq.csv")
        dtm_acc_freq <- read.csv("dtm_acc_freq.csv")
      }
      
      
  #Latent Dirichlet allocation (LDA) is a Bayesian mixture model.
  #Two estimation methods are available for LDA: VEM and Gibbs.
    num_topics = 10
    LDAGtopics <- LDA(dtm_acc_freq, num_topics, method="Gibbs") #Display cluster of words for different topics


  #---Find out different topics---
  #Look at most frequent terms for each topic
    terms(LDAGtopics, 10) #Results (ranked words) for each topic
  
  #Function logLik() gives us the log-likelihood of the model, which is the sum over the log-likelihoods of all documents, maximized during maximum likelihood estimation of the model
    logLik(LDAGtopics) #Random process (Final Log likelihood) --> Remove log to get probability (distribution)
    LDAGtopics@terms[1:10] #Find out first 10 terms in columns
    LDAGtopics@beta[1, 1:10] #Logarithmized likelihood parameters of word for word in topic 3

  #We can produce wordcloud for more intuitive visualisation of topics.
  #Let's create a helper function which takes in a topic model and the index of a topic and generates a wordcloud for this topic.
  #Get matrix of probabilities of words over topics - beta
  #Name columns of matrix with corresponding terms
  #Get topic (a vector of word probabilities) and sort them in decreasing order
  #Display top 20 most frequent words in wordcloud

    for (i in 1:num_topics){
      ShowWordCloud(LDAGtopics, i) #show cloud for topic_1
    }

    ShowWordCloud = function (LDAsrc, LDAtopicnum) {
      LDAbeta <- LDAsrc@beta
      colnames(LDAbeta) <- LDAsrc@terms
      top <- sort(LDAbeta[LDAtopicnum, ], decreasing = TRUE)
      wordcloud(names(top[1:20]), 2^top[1:20], scale = c(2, .04), rot.per = 0.3, colors = dark2)
    }
  
    
  #Topic ranking
    sorted_topics <- data.frame(sort(table(topics(LDAGtopics)), decreasing = TRUE)) #Topic distribution for each document
    #which.max(tabulate(topics(LDAGtopics))) #Display topic with largest number of documents
    #t(topics(LDAGtopics, 3))[1:10,] #Display top 3 topics for each document for the first ten documents (ranked in terms of likelihood)
    
    tidy(LDAGtopics, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(x = term, y = beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()
  
      
      sorted <- sort(colSums(as.matrix(dtm_acc_freq)), decreasing = TRUE)
      sorted_df <- data.frame(term = names(sorted), freq = sorted)

      stopwords_acc_bc = c("approximately", "working", "employees", "hospitalized", "coworker", "hospital", "truck", "two", "machine", "one", "line", "transported", "medical", "ground", "using", "side", "floor", "sustained", "injuries", "accident", "number", "center", "employer", "top", "treated", "water", "operator", "used", "concrete", "ladder", "onto", "emergency", "power", "operating", "metal", "tank", "company", "crane", "building", "three", "air", "caught", "lift", "treatment", "taken", "end", "released", "day", "equipment", "forklift", "july", "pipe", "second", "inc", "suffered", "saw", "went", "coworkers", "later", "inside", "june", "august", "site", "located", "first", "days", "another", "roof", "september", "front", "door", "standing", "causing", "found", "trailer", "began", "received", "removed", "october", "crew", "became", "steel", "leg", "november", "press", "supervisor", "construction", "came", "placed", "incident", "caused", "called", "room", "around", "services", "worker", "march", "bucket", "started", "tree", "wall", "pulled", "boom", "driver", "body", "gas", "model", "load", "cable", "april", "occurred", "control", "department")
  
      sorted_df[!(sorted_df$term %in% stopwords_acc_bc), ] %>%
        arrange(-freq) %>%
        top_n(10) %>%
        ggplot(aes(x = reorder(term, freq), y = (freq))) + 
        geom_bar(stat= "identity") + 
        labs(x = "Type of injury", y = "Count") + 
        geom_text(aes(label = freq,hjust=-0.15)) +
        coord_flip() 


    #LDAGtopics@gamma[1,] #Probabilities of first document belonging to each topic
    #barplot(LDAGtopics@gamma[1,], names.arg=1:10, main = "Topic distribution of Story 1")
  
  #If apply SVD, no more words (no columns), hence cannot use. Therefore, use frequency to reduce sparsity.
  
  #We may also build topic models using a matrix with its sparsity reduced.
  #However, with more sparse terms removed(e.g. at 0.995), some documents with fewer words may end up having no non-zero values in the row, causing LDA() unable to run
    #LDAGtopics_Skinny <- removeSparseTerms(dtm_acc_freq, 0.995)
    #LDAGtopics_Skinny
    #LDA(LDAGtopics_Skinny, 10, method="Gibbs")
  
  #Error might be observed whereby each row of input matrix did not contain at least one non-zero entry because some documents removed (because 0 for all columns - no matching word) during dimension reduction. Therefore, remove those 'empty' rows from matrix first by removing those short documents.

    #rowTotals <- apply(dtm_acc_freq, 1, sum) #Find total count of words in each Document
    #dtm_acc_skinny <- dtm_acc_freq[rowTotals> 0, ] #Remove all docs with 0 words due to RemoveSparse
    #dtm_acc_skinny
    
  #Since it's much faster, let's try getting more topics.
    #LDAGtopics_Skinny <- LDA(dtm_acc_skinny, num_topics, method="Gibbs") #Try out different numbers of topics
    #terms(LDAGtopics_Skinny, 10)
  
  
    #freq <- colSums(as.matrix(dtm_acc_skinny)) #Sum each column
    #length(freq)
    #sor<-as.matrix(sort(freq,decreasing = TRUE))
    #head(sor)
  
