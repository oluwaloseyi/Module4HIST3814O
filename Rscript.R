## Name: Shay Ishola
## RScript for HIST3814O
## Date: June 09, 2018
## Colonial Newspapers

install.packages("mallet")
library("mallet")
install.packages("RCurl")
library("RCurl")

x <- getURL("https://raw.githubusercontent.com/shawngraham/exercise/gh-pages/CND.csv", .opts = list(ssl.verifypeer = FALSE))

documents <- read.csv(text = x, col.names=c("Article_ID", "Newspaper Title", "Newspaper City", "Newspaper Province", "Newspaper Country", "Year", "Month", "Day", "Article Type", "Text", "Keywords"), colClasses=rep("character", 3), sep=",", quote="")

counts <- table(documents$Newspaper.City)
barplot(counts, main="Cities", xlab="Number of Articles")

years <- table(documents$Year)
barplot(years, main="Publication Year", xlab="Year", ylab="Number of Articles")

mallet.instances <- mallet.import(documents$Article_ID, documents$Text, "en.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

documents <- mallet.read.dir("/home/shayishola/war-diary-txt")

#set the number of desired topics
num.topics <- 7
topic.model <- MalletLDA(num.topics)
topic.model$loadDocuments(mallet.instances)
## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)
write.csv(word.freqs, "word-freqs.csv" )
## Optimize hyperparameters every 7 iterations,
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(7, 20)
## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. Here we'll use a large-ish round number.
## When you run the next line, a *lot* of information will scroll through your console.
## Just be patient and wait til it hits that 100 iteration.
topic.model$train(100)
## Run through a few iterations where we pick the best topic for each token,
## rather than sampling from the posterior distribution.
topic.model$maximize(10)
## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

mallet.top.words(topic.model, topic.words[7,])
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "topics-docs.csv" )
# that file enables you to see what topics are most present in what issues/documents

## Get a vector containing short names for the topics
topics.labels <- rep("", num.topics)
for (topic in 1:num.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels

write.csv(topics.labels, "topics-labels.csv")

plot(hclust(dist(topic.words)), labels=topics.labels)
plot(hclust(dist(topic.words)))

topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$article_id

install.packages("cluster")
library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0

# once installed, call it:
library(igraph)

# we transform the information from the previous code block into a network
g <- as.undirected(graph.adjacency(topic_df_dist))

# then we specify the layout and the number of iterations to make it pretty
layout1 <- layout.fruchterman.reingold(g, niter=100)

#then we plot it out
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)
write.graph(g, file="cnd.graphml", format="graphml")
