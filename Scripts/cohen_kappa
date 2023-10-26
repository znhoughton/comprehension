library(tidyverse)
library(fuzzyjoin)
library(psych)

Alex=read.csv("/Users/Lucas/documents/fluentpet/data/Alex_CSC_Combined_Observations_Updated_2023.csv")
Lisa=read.csv("/Users/lucas/Documents/fluentpet/data/Lisa_CSC_Combined_Observations_Updated_2023.csv")

Alex$Source= gsub('.csv', '', Alex$Source)
Lisa$coder="Lisa"
Alex$coder="Alex"
colnames(Lisa)=colnames(Alex)

#selecting only the trials they both share

Alex=Alex %>%
  filter(Source%in% Lisa$Source) %>%
  mutate(Start=Time-1,Stop=Time+1, observation_id_Alex= 1:n())
Lisa=Lisa %>%
  filter(Source%in% Alex$Source) %>%
  mutate(Start=as.numeric(Time)-1,Stop=as.numeric(Time)+1, observation_id_Lisa = 1:n())
#finding matching behaviors coded withing an interval of 2 seconds
matched_behaviors = interval_inner_join(Lisa,Alex, by=c("Start","Stop"))%>% 
  filter(Source.x==Source.y, Behavior.x==Behavior.y, Modifiers.x==Modifiers.y) 

#in the matched_behaviors df, some observations are repeated, because they fall within more than one interval. 
#the following loop looks for a list of matching observations with the constraint that they cannot be repeated
set.seed(123)
list_of_dataframes <- list()
for(j in 1:50) {
  df_new <- data.frame(data.frame(matrix(ncol = 36,nrow=0)))
  colnames(df_new) = colnames(matched_behaviors)
  shuffle_df = matched_behaviors[sample(nrow(matched_behaviors)),]
  for(i in 1:nrow(shuffle_df)) {
    
    if((!shuffle_df[i, "observation_id_Alex"] %in% df_new[,"observation_id_Alex"]) & (!shuffle_df[i, "observation_id_Lisa"] %in% df_new[, "observation_id_Lisa"]))
      df_new <- rbind(df_new, shuffle_df[i,])
  }
  list_of_dataframes[[j]]=df_new
}

df_length=lapply(list_of_dataframes,nrow)

#we look for the df with the maximum number of rows (in this case many df's have the maximum, 158, we pick df with index=12) )

ordered_list = data.frame(rows=unlist(df_length), df_index= 1:length(df_length)) %>%
  arrange(-rows)

longest_dfs = ordered_list[ordered_list$rows==max(ordered_list$rows),]$df_index
longest_dfs = list_of_dataframes[[4]]


JoinObservations = function(x){
  longest_df = list_of_dataframes[[4]]
  colnames(longest_df) = gsub("\\.x$","_Lisa",colnames(longest_df))
  colnames(longest_df) = gsub("\\.y$","_Alex",colnames(longest_df))
  matches_df = longest_df %>% select(Source=Source_Lisa, Time_Lisa,Behavior_Lisa,observation_id_Lisa,observation_id_Alex, Modifiers_Lisa,Time_Alex,Behavior_Alex, Modifiers_Alex)
  # we now look for the observations that happened at the same time but do not match
  joined_missmatches_full = interval_inner_join (Lisa, Alex, by=c("Start","Stop")) %>% 
    filter(Source.x==Source.y & ((Behavior.x != Behavior.y)|(Modifiers.x != Modifiers.y))) %>% 
    filter(!observation_id_Lisa %in% longest_df$observation_id_Lisa & !observation_id_Alex %in% longest_df$observation_id_Alex)
  colnames(joined_missmatches_full) = gsub("\\.x$","_Lisa",colnames(joined_missmatches_full))
  colnames(joined_missmatches_full) = gsub("\\.y$","_Alex",colnames(joined_missmatches_full))
  joined_missmatches = joined_missmatches_full %>%
    select(Source = Source_Lisa, Time_Lisa, Behavior_Lisa, Modifiers_Lisa, Time_Alex, Behavior_Alex, Modifiers_Alex,observation_id_Lisa, observation_id_Alex)
  #this df has all the observations that Alex and Lisa made at the same time
  coincident_observations = rbind(matches_df, joined_missmatches)
  return (coincident_observations)
  }
length_df=data.frame(data.frame(matrix(ncol = 2,nrow=0)))
for (k in longest_dfs){
  coincident_observations=JoinObservations(k)
  length(coincident_observations$observation_id_Lisa)
  new <- c(k, length(coincident_observations$observation_id_Lisa))                   # Create new row
  length_df[nrow(length_df) + 1, ] <- new  
  }
length_df = length_df %>% arrange(-X2)
coincident_observations = JoinObservations(length_df[1,1])

write.csv(coincident_observations,"AlexLisaObs.csv")


#we now look at the observations that don't have anything to compare to
lisacols = c("Behavior_Lisa","Modifiers_Lisa")
alexcols = c("Behavior_Alex","Modifiers_Alex")

Left_observations_Lisa = Lisa %>%
  filter(!(observation_id_Lisa %in% coincident_observations$observation_id_Lisa)) %>%
  select(Source, Time_Lisa = Time,Behavior_Lisa = Behavior, Modifiers_Lisa=Modifiers,observation_id_Lisa)

Left_observations_Alex = Alex %>%
  filter(!(observation_id_Alex %in% coincident_observations$observation_id_Alex)) %>% 
  select(Source, Time_Alex = Time, Behavior_Alex=Behavior, Modifiers_Alex=Modifiers, observation_id_Alex)

write.csv(Left_observations_Lisa,"Lisa_extra_observations.csv")
write.csv(Left_observations_Alex,"Alex_extra_observations.csv")

left_Lisa = data.frame(A = do.call(paste, c(Left_observations_Lisa[lisacols], sep = "-")),B = rep("None",length(Left_observations_Lisa$Source)))
left_Alex = data.frame(A = rep("None",length(Left_observations_Alex$Source)),B=do.call(paste, c(Left_observations_Alex[alexcols], sep="-")))



#first we do the partial cohen kappa on coincident observations

cohen_kappa_coincidence= data.frame(A=do.call(paste, c(coincident_observations[lisacols], sep="-")),B=do.call(paste, c(coincident_observations[alexcols], sep="-")))
partial<-cohen.kappa(x=cohen_kappa_coincidence)
partial

#now we do the complete cohen kappa, including observations without counterpart (reported on the text)

complete_kappa_df= rbind(cohen_kappa_coincidence, left_Lisa, left_Alex)
total<-cohen.kappa(x=complete_kappa_df)
total


# now we look at individual Behaviors
unique(complete_kappa_df$A)
Toy_Behaviors = cohen.kappa(complete_kappa_df %>%
  filter(grepl("Toy-Directed Behaviors" ,A) |grepl("Toy-Directed Behaviors" , B)))
Toy_Behaviors

Food_Behaviors = cohen.kappa(complete_kappa_df %>%
                              filter(grepl("Food-Directed" ,A) |grepl("Food-Directed" , B)))
Food_Behaviors

Door_Behaviors = cohen.kappa(complete_kappa_df %>%
                              filter(grepl("Door-Directed" ,A) |grepl("Door-Directed" , B)))
Door_Behaviors

Head_Behaviors = cohen.kappa(complete_kappa_df %>%
                                               filter(grepl("Head", A) |grepl("Head", B)))
Head_Behaviors
Licking_Behaviors = cohen.kappa(complete_kappa_df %>%
                                  filter(grepl("Lip/Nose", A) |grepl("Lip/Nose", B)))
Licking_Behaviors

trials=unique(Alex$Source)
Alex$Time=as.numeric(Alex$Time)
Lisa$Time=as.numeric(Lisa$Time)

  plotlist=list()
  for (i in trials){
    df = rbind(Alex[Alex$Source==i,1:12],Lisa[Lisa$Source==i,1:12])
    plot = ggplot(df,aes(x=Time, y= coder, color= Behavior)) + geom_point(size=2, shape=4,stroke = 2) + theme_linedraw() + labs(title=i)+ scale_colour_brewer(palette = "Set1")
    plotlist[[i]]=plot}



library(gridExtra)
ggsave(
  filename = "LisaAlexPlots.pdf", 
  plot = marrangeGrob(plotlist, nrow=5, ncol=1), 
  width = 11, height = 15
)

#now we run cohen kappa for a random 10% sample of data

set.seed(123)
random_sample=complete_kappa_df %>% sample_frac(.10)

random_kappa <-cohen.kappa(x=random_sample)
random_kappa
