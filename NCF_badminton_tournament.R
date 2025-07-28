library(tidyverse)

teams_base = data.frame(A = c("Kevin","Pritha","Rajat","Rohit","Suhel"),
                        B = c("Ashwin","Badush","Havisha","Mayank","Mayuresh"),
                        C = c("Bibi","Devica","Kullu","Evan","Soumya"),
                        D = c("Aryan","Misha","Priti","Siddharth","Tamoghna"))

fit = 0

for (k in 1:200)
{
  print(fit)
  
  teamA = expand.grid(P1 = teams_base$A, P2 = teams_base$A) %>%
    mutate(P1 = as.character(P1),P2 =  as.character(P2)) %>%
    filter(P1 != P2) %>%
    mutate(pair = paste(pmin(P1, P2), pmax(P1, P2), sep = " ")) %>%
    group_by(pair) %>% slice(1) %>% ungroup %>%
    mutate(team.no = 1, played = 0)
  
  teamB = expand.grid(P1 = teams_base$B, P2 = teams_base$B) %>%
    mutate(P1 = as.character(P1),P2 =  as.character(P2)) %>%
    filter(P1 != P2) %>%
    mutate(pair = paste(pmin(P1, P2), pmax(P1, P2), sep = " ")) %>%
    group_by(pair) %>% slice(1) %>% ungroup %>%
    mutate(team.no = 2, played = 0)
  
  teamC = expand.grid(P1 = teams_base$C, P2 = teams_base$C) %>%
    mutate(P1 = as.character(P1),P2 =  as.character(P2)) %>%
    filter(P1 != P2) %>%
    mutate(pair = paste(pmin(P1, P2), pmax(P1, P2), sep = " ")) %>%
    group_by(pair) %>% slice(1) %>% ungroup %>%
    mutate(team.no = 3, played = 0)
  
  teamD = expand.grid(P1 = teams_base$D, P2 = teams_base$D) %>%
    mutate(P1 = as.character(P1),P2 =  as.character(P2)) %>%
    filter(P1 != P2) %>%
    mutate(pair = paste(pmin(P1, P2), pmax(P1, P2), sep = " ")) %>%
    group_by(pair) %>% slice(1) %>% ungroup %>%
    mutate(team.no = 4, played = 0)
  
  teams = list(teamA, teamB, teamC, teamD)
  teams_updated = teams
  
  schedule  = data.frame(C1P1 = rep("",20), C1P2 = rep("",20), 
                         C2P1 = rep("",20), C2P2 = rep("",20))  
  participants = as.character()

  for (i in 1:10)
  {
    teams_updated[[1]] = teams[[1]] %>% filter(played == 0, !P1 %in% participants, 
                                               !P2 %in% participants)
    teams_updated[[2]] = teams[[2]] %>% filter(played == 0, !P1 %in% participants, 
                                               !P2 %in% participants)
    teams_updated[[3]] = teams[[3]] %>% filter(played == 0, !P1 %in% participants, 
                                               !P2 %in% participants)
    teams_updated[[4]] = teams[[4]] %>% filter(played == 0, !P1 %in% participants, 
                                               !P2 %in% participants)
    
    if (length(teams_updated[[1]]$played) == 0 &
        length(teams_updated[[2]]$played) == 0 &
        length(teams_updated[[3]]$played) == 0 &
        length(teams_updated[[4]]$played) == 0)
    {
      break
    }
    
    set = c(teams_updated[[1]]$team.no,teams_updated[[2]]$team.no,
            teams_updated[[3]]$team.no,teams_updated[[4]]$team.no)
    
    if (i == 1)
    {
      participants = as.character()
      for (j in 1:4)
      {
        a = sample(set,1)
        b = sample(teams_updated[[a]]$pair,1)
        
        schedule[,j][i] = b
        teams[[a]]$played[teams[[a]]$pair == b] = 1
        
        set = set[!set %in% a]
        participants = c(participants,as.character(teams[[a]][teams[[a]]$pair == b,1:2]))
      }
    }
    
    if (i != 1)
    {
      participants_next = as.character()
      for (j in 1:2)
      {
        if (length(set) == 0)
        {
          break
        }
        a = sample(set,1)
        if (length(teams_updated[[a]]$pair) == 0)
        {
          break
        }
        b = sample(teams_updated[[a]]$pair,1)
        
        schedule[,j][i] = b
        teams[[a]]$played[teams[[a]]$pair == b] = 1
        
        set = set[!set %in% a]
        participants = c(participants,as.character(teams[[a]][teams[[a]]$pair == b,1:2]))
        participants_next = c(participants_next,as.character(teams[[a]][teams[[a]]$pair == b,1:2]))
      }
      
      teams_updated[[1]] = teams[[1]] %>% filter(played == 0, !P1 %in% participants, 
                                                 !P2 %in% participants)
      teams_updated[[2]] = teams[[2]] %>% filter(played == 0, !P1 %in% participants, 
                                                 !P2 %in% participants)
      teams_updated[[3]] = teams[[3]] %>% filter(played == 0, !P1 %in% participants, 
                                                 !P2 %in% participants)
      teams_updated[[4]] = teams[[4]] %>% filter(played == 0, !P1 %in% participants, 
                                                 !P2 %in% participants)
      
      set = c(teams_updated[[1]]$team.no,teams_updated[[2]]$team.no,
              teams_updated[[3]]$team.no,teams_updated[[4]]$team.no)
      
      for (j in 3:4)
      {
        if (length(set) == 0)
        {
          break
        }
        a = sample(set,1)
        if (length(teams_updated[[a]]$pair) == 0)
        {
          break
        }
        b = sample(teams_updated[[a]]$pair,1)
        
        schedule[,j][i] = b
        teams[[a]]$played[teams[[a]]$pair == b] = 1
        
        set = set[!set %in% a]
        participants = c(participants,as.character(teams[[a]][teams[[a]]$pair == b,1:2]))
        participants_next = c(participants_next,as.character(teams[[a]][teams[[a]]$pair == b,1:2]))
      }
      
      participants = participants_next
    }
  }
  
  fit_new = length(teams[[1]]$played[teams[[1]]$played == 1])+
          length(teams[[2]]$played[teams[[2]]$played == 1])+
          length(teams[[3]]$played[teams[[3]]$played == 1])+
          length(teams[[4]]$played[teams[[4]]$played == 1])
  
  if (fit_new > fit)
  {
    fit = fit_new
    schedule_final = schedule
  }
  
  if (length(teams[[1]]$played[teams[[1]]$played == 1]) == 10 &
      length(teams[[2]]$played[teams[[2]]$played == 1]) == 10 &
      length(teams[[3]]$played[teams[[3]]$played == 1]) == 10 &
      length(teams[[4]]$played[teams[[4]]$played == 1]) == 10)
  {
    break
  }
}

write.csv(schedule_final, "NCF_badminton_tournament_schedule.csv", row.names = F)