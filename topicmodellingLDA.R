library(tm)
library(topicmodels)
library(lda)
#stop_words <- stopwords("SMART")
library(LDAvis)
library(xlsx)

avtar_OP=read.xlsx('Jun41102015.xlsx',startRow=24,sheetIndex=1,header=TRUE)
avtar_IP=read.xlsx('Jun2102015.xlsx',startRow=24,sheetIndex=1,header=TRUE)
avtar_ED=read.xlsx('Jun3102015.xlsx',startRow=24,sheetIndex=1,header=TRUE)
df_hcahps=read.xlsx('Jun4102015.xlsx',startRow=24,sheetIndex=1,header=TRUE)

documents <- rbind(avtar_OP, avtar_IP, avtar_ED, df_hcahps)
Outpatient <- documents[documents$Department == "Outpatient Services" | documents$Department == "Outpatient" | documents$Department == "Outpatient Units" | documents$Department == "Chandler Outpatient Services" | documents$Department == "Outpatient Locations" | documents$Department == "Woodland Outpatient", ]
Inpatient <- documents[documents$Department == "Inpatient Locations" | documents$Department == "Mercy Southwest Inpatient Units"  | documents$Department == "Inpatient Units" | documents$Department == "Mercy Inpatient Units" ,]
Emergency <- documents[documents$Department == "Emergency Department" | documents$Department == "Bakersfield Emergency Department" | documents$Department == "Emergency Services" | documents$Department == "Mercy Southwest Emergency Services",]
Imaging   <- documents[documents$Department == "Marian Breast Imaging Center" | documents$Department == "Mercy Imaging Center" | documents$Department == "Plaza Diagnostic Imaging Center" | documents$Department == "Marian Imaging Services Marian Physical" | documents$Department == "Imaging Services" | documents$Department == "Parkway Lab and Imaging",]

############
reviews <- Imaging$Text #### change subset
reviewCorpus <- Corpus(VectorSource(reviews))
#print(summary(reviewCorpus))
review.collection <- reviewCorpus
review.collection <- tm_map(review.collection, stripWhitespace)
review.collection <- tm_map(review.collection, content_transformer(tolower))
review.collection <- tm_map(review.collection, removeNumbers)
review.collection <- tm_map(review.collection, removePunctuation)
review.collection <- tm_map(review.collection, removeWords, stopwords("english"))
####review.collection <- tm_map(review.collection, stemDocument, language = "english")
initial.tdm <- TermDocumentMatrix(review.collection, control = list(wordLength = c(3, Inf)))
initial.tdm <- removeSparseTerms(initial.tdm, 0.99)
dtm <- t(initial.tdm)
index <- which(apply(dtm, 1, sum) != 0)
final.dtm <- dtm[index, ]
######################### Summary statistics of data #############################################
documents <- dtm2ldaformat(final.dtm, omit_empty = TRUE)$documents
vocab <- dtm2ldaformat(final.dtm, omit_empty = TRUE)$vocab
D <- length(documents) #### length of documents 3343
W <- length(vocab)  # number of terms in the vocab 404
doc.length <- sapply(documents, function(x) sum(x[2, ]))  ######number of tokens per document 24, 27, 8, 80, 25, ....
N <- sum(doc.length)  #### # total number of tokens in the data 51536
term.frequency <- as.integer(apply(final.dtm, 2, sum))
#################################################################################################
K <- 10
G <- 5000
alpha <- 0.02
eta <- 0.02
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)


###coefs <- data.frame(coef(summary(fit$model)))
Topics <- apply(top.topic.words(fit$topics, 10, by.score = TRUE), 
                2, paste, collapse = " ")


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))  #### Document toic distribution matrix (D = 3343 X K = 10)
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))  ##### Topic Term distribution matrx (K = 10 X W = 404)

HospitalReviews <- list(phi = phi,
                        theta = theta,
                        doc.length = doc.length,
                        vocab = vocab,
                        term.frequency = term.frequency)


json <- createJSON(phi = HospitalReviews$phi, 
                   theta = HospitalReviews$theta, 
                   doc.length = HospitalReviews$doc.length, 
                   vocab = HospitalReviews$vocab, 
                   term.frequency = HospitalReviews$term.frequency)

serVis(json, out.dir = 'Imaging', open.browser =TRUE)

###########################################################

top.words <- top.topic.words(fit$topics, num.words = 30, by.score=TRUE)

top.documents <- top.topic.documents(fit$document_sums, num.documents = 10, alpha = 0.1)
wc <- word.counts(documents, vocab)

####################################################################




