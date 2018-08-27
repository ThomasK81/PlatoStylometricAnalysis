# dependencies

library(data.table)
library(philentropy)
library(ape)
library(geiger)
library(cld2)
library(stylo)
library(reshape2)
library(ggplot2)
library(Rtsne)
library(spatstat)
library(gridExtra)
library(grid)
library(ggthemes)
theme_set(theme_tufte())  # from ggthemes
library(stringr)
library(ggpubr)

library(doParallel)
library(foreach)
library(ngram)

# platocolor is for tsne. Number of the colors has to be the same like number of workgroups

platocolor <- c("#c42953",
                "#3cc755",
                "#8356d7",
                "#65b324",
                "#245ada",
                "#b0c323",
                "#4577f9",
                "#99c840",
                "#8e38b1",
                "#69c856",
                "#ba61df",
                "#3f931e",
                "#e372e6",
                "#39cc7a",
                "#f82387",
                "#47da9b",
                "#e242a8",
                "#4cb962",
                "#ab46b5",
                "#90d15e",
                "#6c48b5",
                "#87aa23",
                "#4553c2",
                "#d2b837",
                "#7377ec",
                "#afba3b",
                "#a277f0",
                "#69ac43",
                "#b92f97",
                "#289a44",
                "#e156bc",
                "#83ca6c",
                "#d481f1",
                "#388027",
                "#bd68d1",
                "#9bb64b",
                "#436cd5",
                "#f1af3f",
                "#5786e6",
                "#ee8d27",
                "#3861b7",
                "#db9a26",
                "#8c7ce2",
                "#768f22",
                "#e571cf",
                "#27b378",
                "#df3891",
                "#24c99e",
                "#e32851",
                "#15dec5",
                "#f3478d",
                "#74c479",
                "#b52b76",
                "#70cb92",
                "#cc3373",
                "#33d4d1",
                "#be321e",
                "#34c7dd",
                "#e75b2b",
                "#499ae1",
                "#d06e15",
                "#6450a8",
                "#aa9b2b",
                "#9261bf",
                "#93bf66",
                "#813f91",
                "#52a15a",
                "#a850a5",
                "#257c3f",
                "#e84675",
                "#52be98",
                "#e44849",
                "#5bcfbb",
                "#b13339",
                "#4fbbbc",
                "#ed7932",
                "#7e9cea",
                "#c08118",
                "#9a9af0",
                "#897916",
                "#c186e6",
                "#577620",
                "#e994e9",
                "#779944",
                "#a887de",
                "#c7bb5e",
                "#847ac9",
                "#b4c874",
                "#a1468a",
                "#46a26c",
                "#f668a1",
                "#3e7238",
                "#dd64a1",
                "#8cbf79",
                "#d676bc",
                "#99ad5a",
                "#704889",
                "#e49149",
                "#4e5d9d",
                "#bb8333",
                "#3077ab",
                "#ce6132",
                "#56b2e0",
                "#a64017",
                "#279e94",
                "#ee7352",
                "#3a9371",
                "#e66166",
                "#206e54",
                "#f789b3",
                "#296437",
                "#e997d7",
                "#535e0f",
                "#c99fed",
                "#9b6614",
                "#b97dc9",
                "#5a9057",
                "#b8497b",
                "#7bbd94",
                "#a43054",
                "#9ab474",
                "#963a6d",
                "#738c4e",
                "#df6a92",
                "#4d662b",
                "#bba8e9",
                "#924410",
                "#8ea2d9",
                "#b25e25",
                "#7e7eba",
                "#bc9c4d",
                "#775792",
                "#e1b670",
                "#9968a3",
                "#989854",
                "#b06098",
                "#bfbb81",
                "#dc627b",
                "#646831",
                "#d89fd7",
                "#776928",
                "#c187b7",
                "#885622",
                "#ec9cc1",
                "#a47f47",
                "#bd739c",
                "#cb7e4b",
                "#894b67",
                "#f3a276",
                "#933f53",
                "#cf996f",
                "#d6768e",
#                "#96663f",
#                "#e59295",
#                "#9e483a",
#                "#e58577",
#                "#b25d6a",
                "#c36756")


## produce corpus

# without Plato
corpus1 <- fread("corpusParsed.csv", sep = "#")
corpus1$identifier <- paste0("urn:cts:greekLit:", corpus1$identifier)

# add Plato
corpus2 <- fread("corpusplatonicum.csv")
corpus2$wordcount <- NULL

# bind
corpus_all <- rbind(corpus2, corpus1)
corpus_all$identifier[is.na(corpus_all$identifier)]

extractWork <- function(x) {
  result <- unlist(strsplit(x, ":"))[4]
  return(result)
}

corpus_all$Work <- sapply(corpus_all$identifier, extractWork)

combineText <- function(x) {
  result <- paste(x, collapse = " ")
  return(result)
}

alltexts <-corpus_all[, lapply(.SD, combineText), .SD = "text", by=Work]

deleteNonGreek <- function(x) {
  text <-  gsub('[[:punct:] ]+',' ',x)
  words <- unlist(strsplit(text, " "))
  index <- vector()
  indindex <- 0
  for (i in 1:length(words)) {
    testresult <- detect_language(words[i]) != "el"
    if (is.na(testresult)) {
      testresult <- T
    }
    if (length(unlist(strsplit(words[i], ""))) < 2) {
      testresult <- T
    }
    if (testresult) {
      indindex <- indindex + 1
      index[indindex] <- i
      }
  }
  words <- words[-index]
  result <- paste(words, collapse = " ")
  return(result)
}

max_cores <- detectCores() - 1 # let's do everything a bit quicker, but leave one core for answering emails etc.
registerDoParallel(cores = max_cores)

alltexts$text <- foreach(i = alltexts$text, .combine = c) %dopar% deleteNonGreek(i) 
alltexts$wordcount <- foreach(i = alltexts$text, .combine = c) %dopar% wordcount(i) 
registerDoSEQ()

# remove works < 1500, smallest CP work just over 1500
alltexts <- alltexts[-which(alltexts$wordcount < 1500),]

# Find duplicate versions

changeID <- function(x) {
  paste(unlist(strsplit(x, ".", fixed = T))[1:2], collapse = "_")
}
duplicateids <- gsub("_", ".", unname(sapply(alltexts$Work[duplicated(sapply(alltexts$Work, changeID))], changeID)), fixed = T)

dupvector <- vector()
count <- 0
for (i in 1:length(duplicateids)) {
  potdup <- grep(duplicateids[i], alltexts$Work)
  maxscores <- which(alltexts$wordcount[potdup] == max(alltexts$wordcount[potdup]))
  if (length(maxscores) > 1) {
    maxscores <- maxscores[1]
  }
  potdup <- potdup[-maxscores]
  for (j in 1:length(potdup)) {
    count <- count + 1
    dupvector[count] <- potdup[j]
  }
}
dupvector <- unique(dupvector)

# remove duplicates

alltexts <- alltexts[-dupvector,]

for (i in 1:length(alltexts$Work)) {
  filestring <- paste(unlist(strsplit(alltexts$Work[i], ".", fixed = T))[1:2], collapse = "_")
  filename <- paste0("corpus/", filestring, ".txt")
  write(alltexts$text[i], filename, sep="\n")
}

# now produce feature sets in stylo
stylo()

# generated 5000 MF4Grams saved as Stylo_4Gram_frequencies.txt
# generated 5000 MFWs saved as Stylo_Word_frequencies.txt

frequencies <- fread("Stylo_4Gram_frequencies.txt")
frequencies <- fread("Stylo_Word_frequencies.txt")

# generate words that only occur in Menexenus

platoColumns <- grep(pattern = "tlg0059_tlg028", colnames(frequencies))
frequencies <- data.frame(frequencies)
frequencies <- frequencies[which(frequencies[,platoColumns[1]] > 0),]


# create tables for Weka that include class

tfreq <- fread("Stylo_4Gram_frequencies.txt")
tfreq <- fread("Stylo_Word_frequencies.txt")

# reduce to features that occur in Plato
tfreq <- data.frame(tfreq)
temp <- tfreq$feature
tfreq$feature <- NULL
platoColumns <- grep(pattern = "tlg0059_", colnames(tfreq))
test.df <- tfreq[, platoColumns]
indexvec <- vector()
count <- 0

for (i in 1:length(temp)) {
  if (sum(test.df[i,]) == 0) {
    count <- count + 1
    indexvec[count] <- i
  }
}

tfreq <- tfreq[-indexvec,]
temp <- temp[-indexvec]

tfreq <- as.data.frame(t(tfreq))
colnames(tfreq) <- temp
tfreq$class <- gsub("(.*)_.*", "\\1", row.names(tfreq), perl = T)

write.csv(file = "Weka4Gram.csv", x = tfreq, quote = T)
write.csv(file = "WekaWord.csv", x = tfreq, quote = T)

## Weka 4GRam results, simple classifying (10fold cross-validation)
# ZeroR 12.13%, always predicts tlg2200
# OneR "α ὶ   ἐ", 22.88%
# J48  (131 leaves, size of tree 265), 50.75%, precision tlg0059 0.690, recall 0.556
# Naive Bayes, 65.88%, precision tlg0059 0.814, recall 0.972 
# Feauture selection Naive Bayes, Threshold 0 (better in both)

## turn weka back to frequencies

feature2 <- fread("Weka4GramFeaturesSeleted.csv")
feature2 <- data.frame(feature2)
feature2$class <- NULL
feature2 <- t(feature2)
feature2 <- data.frame(feature2)
colnames(feature2) <- colnames(frequencies)[2:length(colnames(frequencies))]
feature2$feature <- row.names(feature2)
feature2$feature <- gsub("X.", "", feature2$feature, fixed = T)
feature2 <- feature2[c(length(colnames(feature2)), c(1:length(colnames(feature2))-1))]
frequencies <- feature2

## Weka Word results, simple classifying (10fold cross-validation)
# ZeroR 12.13%, always predicts tlg2200
# OneR "μὲν", 20%
# J48  (130 leaves, size of tree 259), 50.5%, precision tlg0059 0.800, recall 0.778
# Naive Bayes, 64.75%, precision tlg0059 0.4, recall 1.000 
# Feauture selection J48 (much higher precision)

## turn weka back to frequencies

feature2 <- fread("WekaWordFeaturesSeleted.csv")
feature2 <- data.frame(feature2)
feature2$class <- NULL
feature2 <- t(feature2)
feature2 <- data.frame(feature2)
colnames(feature2) <- colnames(frequencies)[2:length(colnames(frequencies))]
feature2$feature <- row.names(feature2)
feature2$feature <- gsub("X.", "", feature2$feature, fixed = T)
feature2 <- feature2[c(length(colnames(feature2)), c(1:length(colnames(feature2))-1))]
frequencies <- feature2

# testfrequencies usually 1000, 500, 300, 200,100 
# but with MenexenusMFW it is 810, 500, 300, 200, 100
# with Weka4Grams it's 859,500,300,200,100

filebase <- "MenexMFW"
testfrequencies <- c(810, 500, 300, 200, 100)


## big test


frequenciesnames <- frequencies$feature
frequencies <- data.frame(frequencies)
works <- colnames(frequencies)
works <- works[2:length(works)]
authors <- unique(sapply(works, function(x) {unlist(strsplit(x, "_"))[1]}))
platoworks <- works[grep("tlg0059_", works)]
platomatrix <- matrix(0L, nrow = length(platoworks), ncol = length(platoworks))
rownames(platomatrix) <- platoworks
colnames(platomatrix) <- platoworks
measurenames <- c("Euclidean", "Jaccard", "BurrowsDelta", "EdersSimple", "Cosine", "CosineDelta", "EdersDelta", "ArgamonsDelta", "t-SNE")
measurematrix <- matrix(, nrow = length(testfrequencies), ncol = length(measurenames))
rownames(measurematrix) <- paste(testfrequencies, filebase, sep="")
colnames(measurematrix) <- measurenames
platodetected <- measurematrix
menexenuscompanion <- measurematrix

for (j in 1:length(testfrequencies)) {
  testdf <- frequencies[1:testfrequencies[j],]
  testnames <- frequenciesnames[1:testfrequencies[j]]
  tester <- t(testdf[,c(2:length(testdf[1,]))])
  
  d <- dist(tester, method = "euclidean") # distance matrix
  dd <- distance(tester, method = "jaccard")
  delta <- dist.delta(tester, scale = T)
  colnames(dd) <- rownames(tester)
  rownames(dd) <- rownames(tester)
  dd <- as.dist(dd)
  edersimple <- dist.simple(tester)
  cosine <- dist.cosine(tester)
  cosineDelta <- dist.wurzburg(tester)
  ederDelta <- dist.eder(tester)
  argamonDelta <- dist.argamon(tester)
  
  set.seed(9)
  tsne_model_1 = Rtsne(tester, check_duplicates=FALSE, pca=TRUE, perplexity=50, theta=0.5, dims=2)
  d_tsne_1 = as.data.frame(tsne_model_1$Y)
  
  tsneMat <- tsne_model_1$Y
  rownames(tsneMat) <- works
  
  fit1 <- hclust(d, method="ward.D") 
  fit2 <- hclust(dd, method="ward.D")
  fit3 <- hclust(delta, method="ward.D")
  fit4 <- hclust(edersimple, method = "ward.D")
  fit5 <- hclust(cosine, method = "ward.D")
  fit6 <- hclust(cosineDelta, method = "ward.D")
  fit7 <- hclust(ederDelta, method = "ward.D")
  fit8 <- hclust(argamonDelta, method = "ward.D")
  fit9 <- hclust(dist(tsneMat), method = "ward.D")
  
  clusters <- vector("list", 8)
  clusters[[1]] <- fit1
  clusters[[2]] <- fit2
  clusters[[3]] <- fit3
  clusters[[4]] <- fit4
  clusters[[5]] <- fit5
  clusters[[6]] <- fit6
  clusters[[7]] <- fit7
  clusters[[8]] <- fit8
  clusters[[9]] <- fit9
  
  for (m in 1:length(clusters)) {
    testfit <- clusters[[m]]
    fileName <- paste0("plots/", filebase, testfrequencies[j], measurenames[m], ".png")
    png(filename = fileName, width = 6400, height = 4930)
    
    if (m != 9) {
    phylo_fit <- as.phylo(testfit)
    
    phylo_fit$tip.label <- testfit$labels
    
    authors <- unique(sapply(phylo_fit$tip.label, function(x) {unlist(strsplit(x, "_"))[1]}))
    colorpalette <- rainbow(length(authors))
    assigncolors <- function(x) {
      author <- unlist(strsplit(x, "_"))[1]
      result <- colorpalette[which(authors == author)]
      names(result) <- x
      return(result)
    }
    
    
    phylo_fit$tip.color <- sapply(phylo_fit$tip.label, assigncolors) 
    
    cladePlatobranches <- which.edge(phylo_fit, phylo_fit$tip.label[grep("tlg0059_", phylo_fit$tip.label)])
    clcolr <- rep("darkgrey", dim(phylo_fit$edge)[1])
    clcolr[cladePlatobranches] <- "red"
    
    plot(phylo_fit, tip.color = phylo_fit$tip.color, type = "fan", edge.color=clcolr, edge.width=4)
    nodenames <- vector()
    
    } else {
      
      worksTSNE <- rownames(tester)
      Legend <- gsub("(.*)_.*", "\\1", worksTSNE, perl = T)
      alphascale <- grepl(pattern = "tlg0059", x = unique(Legend))
      worksTSNE <- gsub(pattern = "tlg0059_", replacement = "PLATO_", worksTSNE)
      
      
      for (i in 1:length(platocolor)) {
        if (alphascale[i] == TRUE) {
          platocolor[i] <- "#000000FF"
        } else {
          platocolor[i] <- alpha(platocolor[i], .25)
        }
      }
      
      ggplotgraph <- ggplot(d_tsne_1, aes(x=V1, y=V2, color = Legend)) +
        geom_point(size=4) +
        geom_text(aes(label=worksTSNE), vjust=3, hjust=0.5) + 
        guides(colour=guide_legend(override.aes=list(size=4))) +
        xlab("") + ylab("") +
        theme_light(base_size=20) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank()) +
        theme(legend.position="none") +
        scale_colour_manual(values=platocolor)
      print(ggplotgraph)
      
    }
    
    dev.off()
    
    groups <- cutree(testfit, k=length(authors))
    menexenusGroup <- groups["tlg0059_tlg028"]
    tempnames <-names(groups)
    names(groups) <- gsub("(.*)_.*", "\\1", names(groups), perl = T)
    PlatoGroups <- groups[which(names(groups) == "tlg0059")]
    PlatoGroups <- unique(PlatoGroups)
    
    
    clusterMain <- vector()
    clusterMainPercentage <- vector()
    clusterMemberCount <- vector()

    for (i in 1:length(authors)) {
      subgroup <- table(names(which(groups == i)))
      count <- sum(subgroup)
      clusterMain[i] <- names(subgroup[which(subgroup == max(subgroup))])[1]
      clusterMainPercentage[i] <- max(subgroup) / count * 100
      clusterMemberCount[i] <- count
      if (i == menexenusGroup) {
        platodetected[j,m] <- length(grep("tlg0059", names(which(groups == i)))) - 1
        menexvalue <- paste0(names(subgroup), "(", subgroup, ")", collapse = "/")
        menexenuscompanion[j,m] <- menexvalue
      }
    }
    averageCluster <- clusterMainPercentage * clusterMemberCount
    methodScore <- sum(averageCluster) / sum(clusterMemberCount)
    measurematrix[j,m] <- methodScore
    
    names(groups) <- tempnames
    for (q in 1:length(PlatoGroups)) {
      members <- names(which(groups == PlatoGroups[q]))
      members <- members[grep("tlg0059_", members)]
      for (p in 1:length(members)) {
        rowIndex <- which(rownames(platomatrix) == members[p])
        for (o in 1:length(members)) {
          colIndex <- which(rownames(platomatrix) == members[o])
          value <- platomatrix[rowIndex, colIndex]
          value <- value + 1
          platomatrix[rowIndex, colIndex] <- value
        }
      }
    }
  }
}

fileName <- paste0("plots/", filebase, "CorrelationA", ".png")
png(filename = fileName, width = 640, height = 493)

melted_plato <- melt(platomatrix)
melted_plato$value <- melted_plato$value / 45 
melted_plato$Var1 <- gsub("tlg0059_", "", melted_plato$Var1)
melted_plato$Var2 <- gsub("tlg0059_", "", melted_plato$Var2)

ggplotgraph <- ggplot(melted_plato , aes(x = Var1, y = Var2)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradient2(low="white", mid="grey", high="black", 
                       midpoint=0.5, limits=range(melted_plato$value)) +
  ylab("") + xlab("") +
  labs(fill = "Correlation") + 
  theme_tufte(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(ggplotgraph)

dev.off()

fileName <- paste0("plots/", filebase, "MeanCorrelation", ".png")
png(filename = fileName, width = 640, height = 493)

platoworksScore <- vector("numeric", length(platoworks))
names(platoworksScore) <- platoworks
for (i in 1:length(platoworks)) {
  platoworksScore[platoworks[i]] <- mean(platomatrix[platoworks[i],]/40)
}

platoworksScore <- sort(platoworksScore, decreasing = F)
datNamedVec <- data.frame(work = names(platoworksScore), score = platoworksScore)
datNamedVec$work <- factor(datNamedVec$work, levels = datNamedVec$work) 

ggplotgraph <- ggplot(datNamedVec, aes(x=factor(work), weight=score)) + geom_bar() + coord_flip() +
  ylab("Mean Correlation") + xlab("Corpus Platonicum") +
  theme_tufte(base_size = 12)

print(ggplotgraph)

dev.off()

fileName <- paste0("plots/", filebase, "CorrelationB", ".png")
png(filename = fileName, width = 640, height = 493)

corRaw <- cor(platomatrix)
 
plot(im(corRaw[nrow(corRaw):1,]), main="Correlation Matrix Map")

dev.off()

fileName <- paste0("plots/", filebase, "CorrelationBggplot", ".png")
png(filename = fileName, width = 640, height = 493)

melted_plato <- melt(corRaw)
melted_plato$value <- melted_plato$value 
melted_plato$Var1 <- gsub("tlg0059_", "", melted_plato$Var1)
melted_plato$Var2 <- gsub("tlg0059_", "", melted_plato$Var2)

ggplotgraph <- ggplot(melted_plato , aes(x = Var1, y = Var2)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradient2(low="red", mid="white", high="blue", 
                       midpoint=0, limits=range(melted_plato$value)) +
  ylab("") + xlab("") +
  labs(fill = "Correlation") + 
  theme_tufte(base_size = 12) +
theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(ggplotgraph)

dev.off()

fileName <- paste0("plots/", filebase, "Dissimilarity", ".png")
png(filename = fileName, width = 640, height = 493)

dissimilarity <- 1 - abs(cor(platomatrix))
distance <- as.dist(dissimilarity)

plot(hclust(distance), 
    main="Dissimilarity (1 - Abs(Correlation))", xlab="")

dev.off()

fileName <- paste0("plots/", filebase, "MeasureBoxPlot", ".png")
png(filename = fileName, width = 640, height = 493)

ggplotgraph <- ggplot(stack(data.frame(measurematrix)), aes(x = ind, y = values)) +
  xlab("Measure") + ylab("K-Score") +
  geom_tufteboxplot() + 
  coord_flip() +
  theme_tufte(base_size = 15)

print(ggplotgraph)

dev.off()

fileName <- paste0("plots/", filebase, "MeasureTable", ".png")
measurematrix <- round(measurematrix, digits = 2)
measure.df <- data.frame(measurematrix)
png(filename = fileName, height=35*nrow(measure.df), width=150*ncol(measure.df))
ggtexttable(measure.df, theme = ttheme(base_style = "minimal"))
dev.off()

fileName <- paste0("tables/", filebase, "MeasureTable", ".csv")
write.table(measurematrix, file = fileName)


fileName <- paste0("plots/", filebase, "MenexenusWithPlato", ".png")
measure.df <- data.frame(platodetected)
png(filename = fileName, height=35*nrow(measure.df), width=150*ncol(measure.df))
ggtexttable(measure.df, theme = ttheme(base_style = "minimal"))
dev.off()

fileName <- paste0("tables/", filebase, "MenexenusWithPlato", ".csv")
write.table(platodetected, file = fileName)

fileName <- paste0("plots/", filebase, "MenexenusCluster", ".png")
measure.df <- data.frame(menexenuscompanion)

for (i in 1:length(measure.df[1,])) {
  measure.df[,i] <- gsub("/", "\n", measure.df[,i])
}

png(filename = fileName, height=200*nrow(measure.df), width=150*ncol(measure.df))
ggtexttable(measure.df, theme = ttheme(base_style = "minimal"))
dev.off()

fileName <- paste0("tables/", filebase, "MenexenusCluster", ".csv")
write.table(menexenuscompanion, file = fileName)

###### Viz for PlatoFrequencies

vizboxtest <- frequencies
viztemp <- vizboxtest$feature
platocol <- grep("tlg0059_", colnames(vizboxtest))
vizboxtest <- vizboxtest[,platocol]
vizboxtest$feature <- NULL

# reduce to only where menexenus is outlier

mcol <- which(colnames(vizboxtest) == "tlg0059_tlg028")
mcolind <- vector()
count <- 0

for (i in 1:length(vizboxtest[,1])) {
  outliervalues <- boxplot.stats(as.numeric(vizboxtest[i,]))$out
  for (j in 1:length(outliervalues)) {
    candidates <- which(as.numeric(vizboxtest[i,]) == outliervalues[j])
    for (k in 1:length(candidates)) {
      if (length(candidates[k]) > 0) {
          if (is.na(candidates[k]) == FALSE) {
            if (mcol == candidates[k]) {
            count <- count + 1
            mcolind[count] <- i
          }
        }
      }
    }
  }
}


## for words mcolind has the length of 251

vizboxtest <- vizboxtest[mcolind,]
viztemp <- viztemp[mcolind]

platosums <- vector()

for (i in 1:length(vizboxtest[,1])) {
  platosums[i] <- sum(vizboxtest[i,]) / 36
}

# reduce to what occurs every 500 tokens (mean frequency 0.2)
vizboxtest <- data.frame(vizboxtest)
vizboxtest <- vizboxtest[which(platosums >= 0.2),]
viztemp <- viztemp[which(platosums >= 0.2)]

menexfreqvec <- vizboxtest$tlg0059_tlg028
names(menexfreqvec) <- viztemp
vizboxtest <- t(vizboxtest)
colnames(vizboxtest) <- viztemp
vizboxtest <- data.frame(vizboxtest)

fileName <- paste0("plots/", "FeatureFrequencyPlatoWord", ".png")
png(filename = fileName, height=649, width=488)

ggplot(stack(vizboxtest), aes(x = ind, y = values)) +
  xlab("Feature") + ylab("Frequency") +
  geom_boxplot(outlier.size = 0.5) +
  geom_point(data = data.frame(x = factor(names(menexfreqvec)), y = menexfreqvec),
             aes(x=x, y=y),
             color = 'red',
             shape = 25) +
  coord_flip() +
  theme_tufte(base_size = 15)

dev.off()

### Combine Tables

measureTable <- fread("tables/MFWMeasureTable.csv")
measureTable <- data.frame(measureTable)

temp <- fread("tables/MF4GramMeasureTable.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/WekaWordMeasureTable.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/Weka4GramMeasureTable.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/MenexMFWMeasureTable.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- measureTable$V1
measureTable$V1 <- NULL
rownames(measureTable) <- temp

fileName <- paste0("plots/", "AllMeasureBoxPlot", ".png")
png(filename = fileName, width = 640, height = 493)

ggplotgraph <- ggplot(stack(measureTable), aes(x = ind, y = values)) +
  xlab("Measure") + ylab("K-Score") +
  geom_tufteboxplot() + 
  coord_flip() +
  theme_tufte(base_size = 15)

print(ggplotgraph)

dev.off()

fileName <- paste0("plots/", "AllMeasurePlot", ".png")
png(filename = fileName, height=35*nrow(measureTable), width=150*ncol(measureTable))
ggtexttable(measureTable, theme = ttheme(base_style = "minimal"))
dev.off()

fileName <- paste0("tables/", "AllMeasureTable", ".csv")
write.table(measureTable, file = fileName)

##

measureTable <- fread("tables/MFWMenexenusWithPlato.csv")
measureTable <- data.frame(measureTable)

temp <- fread("tables/MF4GramMenexenusWithPlato.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/WekaWordMenexenusWithPlato.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/Weka4GramMenexenusWithPlato.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/MenexMFWMenexenusWithPlato.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- measureTable$V1
measureTable$V1 <- NULL
rownames(measureTable) <- temp

fileName <- paste0("plots/", "AllMenexenusWithPlatoPlot", ".png")
png(filename = fileName, height=35*nrow(measureTable), width=150*ncol(measureTable))
ggtexttable(measureTable, theme = ttheme(base_style = "minimal"))
dev.off()

fileName <- paste0("tables/", "AllMenexenusWithPlatoTable", ".csv")
write.table(measureTable, file = fileName)


# > 0, percWithPlato 11.56% of the tests (6.23 with just 1)
# > 1, percWithPlato 5.33
# = 7, percWithPlato only 1 instance, = 6 zero instances, 8 instances for 3,4,5

percWithPlato <- length(which(as.matrix(measureTable) > 6)) / length(as.matrix(measureTable)) * 100
percWithPlato <- round(percWithPlato, digits = 2)

###

measureTable <- fread("tables/MFWMenexenusCluster.csv")
measureTable <- data.frame(measureTable)

temp <- fread("tables/MF4GramMenexenusCluster.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/WekaWordMenexenusCluster.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/Weka4GramMenexenusCluster.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- fread("tables/MenexMFWMenexenusCluster.csv")
temp <- data.frame(temp)
measureTable <- rbind(measureTable, temp)

temp <- measureTable$V1
measureTable$V1 <- NULL
rownames(measureTable) <- temp

fileName <- paste0("tables/", "AllMenexenusCluster", ".csv")
write.table(measureTable, file = fileName)



