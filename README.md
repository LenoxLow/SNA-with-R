This tutorial used the sample data to demonstrate the basic processes of
performing social network analysis with R. The `igraph` library was used
to do the social network analysis. You should install the latest version
of `igraph` for R.

(A) Import Libraries
--------------------

    library("igraph")
    library("data.table")

(B) Import Data
---------------

    transaction <- read.csv("../Data/transaction_sample.csv", header=TRUE);
    head(transaction)

    ##   PARTY_A PARTY_B CALL_DURATION CALL_COUNT
    ## 1   82457   91010           134          3
    ## 2   82457   48305            11          1
    ## 3   82457   19255            53          1
    ## 4   82457   35885            36          1
    ## 5   82457   61112            73          1
    ## 6   82457   88243           156          1

(C) Data Preprocessing
----------------------

Extract the unique MSISDN from the transaction data, then calculate the
total incoming and outgoing call count and duration for each MSISDN. Two
functions were created to extract the MSISDNs.

### (I) Extract unique MSISDN from the transaction data

    ExtractUniqueMSISDNList <- function(target.df) {
      temp1 <- unique(target.df$PARTY_A)
      temp2 <- unique(target.df$PARTY_B)
      temp <- c(temp1, temp2)
      
      m.list <- unique(temp)
      m.list <- data.frame(m.list)
      colnames(m.list)[1] <- "MSISDN"
      
      return(m.list)
    }
    subs <- ExtractUniqueMSISDNList(transaction)

### (II) Extract the transaction reecords of defined MSISDN.

    ExtractTransactionByMSISDN <- function(msisdn, target.df){
      sub.trans <- target.df[target.df$PARTY_A == msisdn | target.df$PARTY_B == msisdn, ]
      
      return(sub.trans)
    }

Calculate the total call count and duration

    for(i in 1:nrow(subs)){
      subs$CALL_IN_DUR[i] <- sum(transaction$CALL_DURATION[transaction$PARTY_B == subs$MSISDN[i]]) 
      subs$CALL_OUT_DUR[i] <- sum(transaction$CALL_DURATION[transaction$PARTY_A == subs$MSISDN[i]]) 
      
      subs$CALL_IN_COUNT[i] <- sum(transaction$CALL_COUNT[transaction$PARTY_B == subs$MSISDN[i]]) 
      subs$CALL_OUT_COUNT[i] <- sum(transaction$CALL_COUNT[transaction$PARTY_A == subs$MSISDN[i]]) 
      
      subs$RELATION_COUNT[i] <- nrow(ExtractUniqueMSISDNList(ExtractTransactionByMSISDN(subs$MSISDN[i], transaction))) - 1 
      subs$IN_RELATION[i] <- nrow(transaction[transaction$PARTY_B == subs$MSISDN[i], ])
      subs$OUT_RELATION[i] <- nrow(transaction[transaction$PARTY_A == subs$MSISDN[i], ])
    }

    subs$TOTAL_CALL_COUNT <- subs$CALL_IN_COUNT + subs$CALL_OUT_COUNT

    head(subs)

    ##   MSISDN CALL_IN_DUR CALL_OUT_DUR CALL_IN_COUNT CALL_OUT_COUNT
    ## 1  82457       12741         8278           218            172
    ## 2  67686           0           68             0              2
    ## 3  40428           0           89             0              1
    ## 4  38865           0          121             0              3
    ## 5   3263           0           27             0              1
    ## 6  79679           0           28             0              1
    ##   RELATION_COUNT IN_RELATION OUT_RELATION TOTAL_CALL_COUNT
    ## 1             60          38           49              390
    ## 2              1           0            1                2
    ## 3              1           0            1                1
    ## 4              1           0            1                3
    ## 5              1           0            1                1
    ## 6              1           0            1                1

### (I) Data Cleansing

Set the boundary value for the cleansing criteria

    call.relation <- 30
    call.count <- 30
    call.mean <- mean(subs$TOTAL_CALL_COUNT)
    call.duration <- 15

Label every MSIDSN based on the criteria stated below. CALL CENTER -
Incoming call is greater than call count boundary No outgoing call Call
relation count is greater than call relation count boundary TELESALES -
Outgoing call is greater than call count boundary No incoming call
Outgoing call divided by call relation count is less than the mean of
the total call count NOISY - Incoming call count is 1 No outgoing call
The call duration is less than the call duration boundary

    for(i in 1: nrow(subs)){
      if(subs$CALL_IN_COUNT[i] > call.count && subs$CALL_OUT_COUNT[i] == 0 && subs$RELATION_COUNT[i] > call.relation) {
        subs$TYPE[i] <- "CALL CENTER"
      } else if (subs$CALL_IN_COUNT[i] == 0 && subs$CALL_OUT_COUNT[i] > call.count && subs$CALL_OUT_COUNT[i] / subs$RELATION_COUNT[i] < call.mean) {
        subs$TYPE[i] <- "TELESALES"
      } else if (subs$CALL_IN_COUNT[i] == 1 && subs$CALL_OUT_COUNT[i] == 0 && subs$CALL_IN_DUR[i] < call.duration) {
        subs$TYPE[i] <- "NOISY"
      } else {
        subs$TYPE[i] <- "NORMAL"
      }
    }

    normal.msisdn <- subs[subs$TYPE == "NORMAL",]

Get the transaction records of normal MSISDN only and calculate average
duration per call (Weight) betwen two MSISDNs.

    filtered.transaction <- transaction[transaction$PARTY_A %in% normal.msisdn$MSISDN & transaction$PARTY_B %in% normal.msisdn$MSISDN,]
    filtered.transaction$Weight <- filtered.transaction$CALL_DURATION / filtered.transaction$CALL_COUNT
    head(filtered.transaction)

    ##   PARTY_A PARTY_B CALL_DURATION CALL_COUNT    Weight
    ## 1   82457   91010           134          3  44.66667
    ## 3   82457   19255            53          1  53.00000
    ## 4   82457   35885            36          1  36.00000
    ## 5   82457   61112            73          1  73.00000
    ## 6   82457   88243           156          1 156.00000
    ## 7   82457   83190            26          1  26.00000

### (II) Transform Data Frame to Matrix

    msisdn.list <- ExtractUniqueMSISDNList(filtered.transaction)
    ma <- matrix(0, ncol=nrow(msisdn.list), nrow=nrow(msisdn.list))
    madf <- data.frame(ma)

    colnames(madf) <- msisdn.list$MSISDN
    row.names(madf) <- msisdn.list$MSISDN

    filtered.transaction$PARTY_A <- as.character(filtered.transaction$PARTY_A)
    filtered.transaction$PARTY_B <- as.character(filtered.transaction$PARTY_B)

    for(i in 1:nrow(filtered.transaction)) {
      madf[filtered.transaction[i,"PARTY_A"], filtered.transaction[i,"PARTY_B"]] <- filtered.transaction[i, "Weight"]
    }
    ma <- as.matrix(madf)
    ma[1:6, 1:6]

    ##          82457 67686 40428 38865 3263 79679
    ## 82457 55.69231     0     0     0    0     0
    ## 67686 34.00000     0     0     0    0     0
    ## 40428 89.00000     0     0     0    0     0
    ## 38865 40.33333     0     0     0    0     0
    ## 3263  27.00000     0     0     0    0     0
    ## 79679 28.00000     0     0     0    0     0

(D) Social Network Analysis (SNA)
---------------------------------

### (I) Build the network

    net <- graph.adjacency(ma, mode="directed", weighted=TRUE, diag=FALSE)

### (II) Generate Result

#### (1) Closeness Centrality - how fast can a node reach everyone in the network.

    res.closeness <- closeness(net)

    ## Warning in closeness(net): At centrality.c:2617 :closeness centrality is
    ## not well-defined for disconnected graphs

    res <- setDT(data.frame(res.closeness), keep.rownames = TRUE)[]
    colnames(res)[1] <- "MSISDN"
    colnames(res)[2] <- "Closeness"
    res$MSISDN <- as.numeric(res$MSISDN)

    res.closeness[1:5]

    ##        82457        67686        40428        38865         3263 
    ## 0.0003509461 0.0002314579 0.0001471575 0.0002171345 0.0002496605

#### (2) Degree Centrality - the number of links connected to a node.

    res <- cbind(res, degree(net))
    colnames(res)[ncol(res)] <- "Degree"

    degree(net)[1:5]

    ## 82457 67686 40428 38865  3263 
    ##    79     1     1     1     1

#### (3) Betweenness Centrality - the extent to which a node lies on shortest paths between other nodes.

    res <- cbind(res, betweenness(net))
    colnames(res)[ncol(res)] <- "Betweenness"

    betweenness(net)[1:5]

    ## 82457 67686 40428 38865  3263 
    ##  1519     0     0     0     0

#### (4) Eigenvector Centrality - influence of a node in a network.

    res <- cbind(res, eigen_centrality(net)$vector)
    colnames(res)[ncol(res)] <- "Eigenvector"

    eigen_centrality(net)$vector[1:5]

    ##      82457      67686      40428      38865       3263 
    ## 1.00000000 0.05264161 0.13779715 0.06244740 0.04180363

Result overview

    head(res)

    ##    MSISDN    Closeness Degree Betweenness Eigenvector
    ## 1:  82457 0.0003509461     79        1519  1.00000000
    ## 2:  67686 0.0002314579      1           0  0.05264161
    ## 3:  40428 0.0001471575      1           0  0.13779715
    ## 4:  38865 0.0002171345      1           0  0.06244740
    ## 5:   3263 0.0002496605      1           0  0.04180363
    ## 6:  79679 0.0002468868      1           0  0.04335191

#### (5) Density - total number of edges over total edges

    density <- edge_density(net, loops=FALSE)
    density

    ## [1] 0.02367037

#### (6) Reciprocity - the ratio of edges which are dual-directed over total edges

    reciprocity <- reciprocity(net)
    reciprocity

    ## [1] 0.5185185

Plots the network
-----------------

    V(net)$color=V(net)$name 
    V(net)$color=gsub("82457","red","orange")
    plot(net, edge.arrow.size=.4,vertex.label=NA)

![](SNA_files/figure-markdown_strict/unnamed-chunk-19-1.png)
