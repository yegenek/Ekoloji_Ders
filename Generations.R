library(tidyverse)
primary <- function(sample_size,freq_A = 0.5){
        freq_a <- 1-freq_A
        population <- data.frame()
        #names(population) <- c("F1","F2","F3","F4","F5")
        # İlkin populasyon
        for(i in seq_len(sample_size)){
                ind = sample(c("a","A"),size = 2,replace = TRUE,
                             prob = c(freq_a,freq_A))
                ind_1 = paste(ind[1],ind[2],sep = ".")
                if(ind_1 == "a.A"){ind_1 = "A.a"}
                ind_1 = data.frame("Inds" = ind_1,"Generation" = 0)
                population <- rbind(population,ind_1)
        }
        return(population)
}


library(tidyverse)

Changing <- function(sample_size,freq_A,generations = 1:5){
        
        # Sistem 1 
        population <- primary(sample_size,freq_A)
        for (i in generations){
                generation <- subset(population,Generation == i-1)
                new_pop <- data.frame()
                for (j in 1:sample_size){
                        sample_1 <- unlist(strsplit(as.character(sample(generation$Inds,1)),split =".",fixed = TRUE))
                        sample_2 <- unlist(strsplit(as.character(sample(generation$Inds,1)),split =".",fixed = TRUE))
                        ind <- paste(sample(sample_1,1),sample(sample_2,1),sep =".")
                        if(ind == "a.A"){ind = "A.a"}
                        ind = data.frame("Inds" = ind,"Generation" = i)
                        new_pop <- rbind(new_pop, ind)
                }
                population <- rbind(population, new_pop)
                
        }
        population$Generation <- population$Generation
        return(population)
}

pop_1 <- Changing(100,0.7,1:5)

table_1 <- table(pop_1$Inds,pop_1$Generation)
barplot(table_1)



barplot(table_1, main="Genotip Frekanslarının Değişimi",
        xlab="Kuşaklar", col=c("darkblue","red","green"),beside = TRUE,
        legend = rownames(table_1))

barplot(table_1, main="Genotip Frekanslarının Değişimi",
        xlab="Kuşaklar", col=c("darkblue","red","green"),beside = TRUE,
        legend = rownames(table_1))

ggplot(pop_1,aes(x=Generation,fill = Inds)) +
        geom_bar(position = "dodge") +
        xlab("Kuşaklar")+
        ylab("Sayı")+
        scale_x_continuous(breaks=c(0:5))+
        labs(fill = "Genotip") +
        ggtitle("Genotip Frekanslarının Değişimi")

frequency <- function(pop,generation){
        gen <- subset(pop,Generation == generation)
        freq_A = (2*sum(gen$Inds == "A.A")+sum(gen$Inds == "A.a"))/(2*dim(gen)[1])
        freq_a = (2*sum(gen$Inds == "a.a")+sum(gen$Inds == "A.a"))/(2*dim(gen)[1])
        print(freq_A)
        print(freq_a)
}

frequency(pop_1,4)
pop_1$Generation <- as.factor(pop_1$Generation)
tapply(pop_1,pop_1$Generation,frequency)
tapply(pop_1$Inds,pop_1$Generation,frequency)
