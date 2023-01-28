
setwd("C:/Users/13204/Documents")
library(readxl)
library(lpSolve)
library(lpSolveAPI)
library(lubridate, warn.conflicts = FALSE)

######################################

#get data in

soccerPlayDatesXlsx = as.matrix(read_excel("soccerMensPlayDates.xlsx"))
namesOfPlayDates = soccerPlayDatesXlsx[,1]
dayOfWeek = wday(namesOfPlayDates)
#travelDistanceXlsx = as.matrix(read_excel("soccerTravelDistancesSmallerWithBye.xlsx"))
travelDistanceXlsx = as.matrix(read_excel("soccerMensTravelDistancesWithBye.xlsx"))
namesOfTeams = travelDistanceXlsx[,1]
namesOfTeamsWithoutBye = setdiff(namesOfTeams, c("Bye"))
namesOfPlayDatesMinusFirst = setdiff(namesOfPlayDates, c("2023-9-12"))

#distanceMatrix = matrix(as.numeric(travelDistanceXlsx[,2:ncol(travelDistanceXlsx)]))
#colnames(distanceMatrix) = colnames(travelDistanceXlsx)[2:(ncol(travelDistanceXlsx))]
#rownames(distanceMatrix) = colnames(travelDistanceXlsx)[2:(ncol(travelDistanceXlsx))]

index <- namesOfPlayDates

distanceMatrix = matrix(as.numeric(travelDistanceXlsx[,2:(ncol(travelDistanceXlsx))]),nrow = nrow(travelDistanceXlsx),ncol = ncol(travelDistanceXlsx) -1)
colnames(distanceMatrix) = namesOfTeams
rownames(distanceMatrix) = namesOfTeams

tableMatrix = matrix(0,length(namesOfTeams),length(namesOfPlayDates))
#tableMatrix = matrix(as.numeric(travelDistanceXlsx[,2:(ncol(travelDistanceXlsx))]),nrow = nrow(travelDistanceXlsx),ncol = ncol(travelDistanceXlsx) -1)
colnames(tableMatrix) = namesOfPlayDates
rownames(tableMatrix) = namesOfTeams
###

#Lets create variables in the spirit of our past ones

namesOfVariables = c()
namesOfVariablesWithBye = c()# empty list

for(home in namesOfTeams){
  for(visitor in namesOfTeams){
    for(playDate in namesOfPlayDates){
      if(home != visitor){
        newVariable = paste("x", home, visitor, playDate, wday(playDate), sep = ".")
        
        namesOfVariables = c(namesOfVariables, newVariable) # tack new one onto it
        
      }
    }
  }
}

# initializing a variable for bye week



# 
for(home in namesOfTeamsWithoutBye){
  
  for(playDate in namesOfPlayDatesMinusFirst){
    
    newVariable = paste("b", home,playDate, sep = ".")
    namesOfVariables = c(namesOfVariables, newVariable) # tack new one onto it
  }
}




#initialize objects to store stuff in
constraintMatrix = matrix(0,0,length(namesOfVariables))
colnames(constraintMatrix) = namesOfVariables
inequalities = matrix("",0,1)
rightHandSide = matrix(0,0,1)

#constraintMatrixPlus = matrix(0,0, length())



##########################

#Each team can only play each other once

for(home in namesOfTeams){
  for(visitor in namesOfTeams){
    if(home != visitor){
      
      newConstraint = matrix(0,1,length(namesOfVariables))
      colnames(newConstraint) = namesOfVariables
      
      regex = paste("x",home, visitor, ".*",".*", sep = ".")
      regex = paste0("^", regex, "$")
      indicesToModify = grep(pattern = regex, namesOfVariables)
      newConstraint[indicesToModify] = 1
      
      regex = paste("x",visitor, home, ".*",".*", sep = ".")
      regex = paste0("^", regex, "$")
      indicesToModify = grep(pattern = regex, namesOfVariables)
      newConstraint[indicesToModify] = 1
      
      constraintMatrix = rbind(constraintMatrix, newConstraint)
      inequalities = rbind(inequalities, "=")
      rightHandSide = rbind(rightHandSide,1)
      
      
    }
  }
}

#each team plays once per date

for(team in namesOfTeams){
  for(playDate in namesOfPlayDates){
    
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariablesWithBye
    
    regex = paste("x",team, ".*",playDate, ".*", sep = ".")
    regex = paste0("^", regex, "$")
    indicesToModify = grep(pattern = regex, namesOfVariables)
    newConstraint[indicesToModify] = 1
    
    regex = paste("x", ".*",team,playDate, ".*", sep = ".")
    regex = paste0("^", regex, "$")
    indicesToModify = grep(pattern = regex, namesOfVariables)
    newConstraint[indicesToModify] = 1
    
    constraintMatrix = rbind(constraintMatrix, newConstraint)
    inequalities = rbind(inequalities, "=")
    rightHandSide = rbind(rightHandSide,1)
    
  }
}


# 
# #Constraint that each Team has at least 1 home game in final 3
# 
for(home in namesOfTeamsWithoutBye){
  
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  
  indicesToGrab = paste(index[9:11], collapse = "|")
  regex1 = paste0("(", indicesToGrab, ")")
  
  regex = paste("x", home, ".*",regex1, ".*", sep = ".")
  regex = paste0("^", regex, "$")
  indicesToModify = grep(pattern = regex, namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  
  #update our constraint matrix, inequalities, and right hand side:
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,">=")
  rightHandSide = rbind(rightHandSide,1)
}
# # # this handles constraint that long trips be scheduled for a Saturday  when (CON, SMU, CSS) are home
bigM = 10000
farTeams = c("Concordia", "St Marys","St Scholastica")
##########################

#Objective Function
objectiveFunction = matrix(0,1, length(namesOfVariables))
colnames(objectiveFunction) = namesOfVariables

for(home in namesOfTeams){
  for(visitor in namesOfTeams){
    #dayOfWeek = wday(playDate)
    if(home != visitor){
      
      for(i in 1: length(namesOfPlayDates)){
        currentIndex <- i
        regex = paste("x", home, visitor, ".*", wday(index[i]), sep = ".")
        regex = paste0("^",regex,"$")
        
        regex1 = paste("b", home, ".*", sep = ".")
        regex1 = paste0("^",regex1,"$")
        
        
        ind = grep(regex, namesOfVariables)
        ind1 = grep(regex1, namesOfVariables)
        
        if(wday(index[i]) != 7 && is.element(home, farTeams) && is.element(visitor, farTeams)){
          
          objectiveFunction[ind] = bigM+distanceMatrix[home,visitor]
        }
        else{
          objectiveFunction[ind] = distanceMatrix[home,visitor]
          objectiveFunction[ind1] = distanceMatrix[home, "Bye"]
        }
      }
    }
  }
}
# # # # constraint that home teams should have 2 home games each (actually 5)
# # # 
for(home in namesOfTeamsWithoutBye){
  
  
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  
  regex = paste("x",home, ".*", ".*", ".*", sep = ".")
  regex = paste0("^", regex, "$")
  indicesToModify = grep(pattern = regex, namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  #update our constraint matrix, inequalities, and right hand side:
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,"=")
  rightHandSide = rbind(rightHandSide,5)
  
  
}
#
# # constraint that Saturday home dates should be close to equal ,
# # between 1 amd 3 for mini (actaully three to four max),
#
for(home in namesOfTeamsWithoutBye){
  
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  
  #dayOfWeek = wday(playDate)
  
  regex = paste("x",home, ".*", ".*", 7, sep = ".")
  regex = paste0("^", regex, "$")
  indicesToModify = grep(pattern = regex, namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  
  
  #update our constraint matrix, inequalities, and right hand side:
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,"<=")
  rightHandSide = rbind(rightHandSide,4)
  
}

for(home in namesOfTeamsWithoutBye){
  
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  
  #dayOfWeek = wday(playDate)
  
  regex = paste("x",home, ".*", ".*", 7, sep = ".")
  regex = paste0("^", regex, "$")
  indicesToModify = grep(pattern = regex, namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  
  
  #update our constraint matrix, inequalities, and right hand side:
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,">=")
  rightHandSide = rbind(rightHandSide,3)
  
}




# # how do we write this without this many loops? Really slow
# # constraint that limits number of consecutive home or away games to 2, maybe try 1 (actually no more than two in a row, but open to three)
#
for(home in namesOfTeamsWithoutBye){
  for(visitor in namesOfTeamsWithoutBye){
    if(home!= visitor){
      
      newConstraint = matrix(0,1,length(namesOfVariables))
      colnames(newConstraint) = namesOfVariables
      
      i<- 0
      for(i in 1: length(index[indexToLookFor]){
        if(i<=10){
          currentIndex <- i
          # indexToLookFor <- i + 1
          
          newConstraint = matrix(0,1,length(namesOfVariables))
          colnames(newConstraint) = namesOfVariables
          
          # indicesToGrab1 = paste(index[currentIndex:indexToLookFor], collapse = "|")
          # regex1 = paste0("(", indicesToGrab1, ")")
          
          
          regexA = paste("x", home, ".*",regex1, ".*", sep = ".")
          regexA = paste0("^", regexA, "$")
          indicesToModifyA = grep(pattern = regexA, namesOfVariables)
          newConstraint[indicesToModifyA] = 1
          
          regexB = paste("x", ".*",home, regex1, ".*", sep = ".")
          regexB = paste0("^", regexB, "$")
          indicesToModifyB = grep(pattern = regexB, namesOfVariables)
          newConstraint[indicesToModifyB] = 1
          
          regexC = paste("x", ".*",home, regex1, ".*", sep = ".")
          regexC = paste0("^", regexC, "$")
          indicesToModifyC = grep(pattern = regexB, namesOfVariables)
          newConstraint[indicesToModifyC] = 1
          
          
          
          
          #update our constraint matrix, inequalities, and right hand side:
          constraintMatrix = rbind(constraintMatrix,newConstraint)
          regexB = paste("x", ".*",home, regex1, ".*", sep = ".")
          regexB = paste0("^", regexB, "$")
          indicesToModifyB = grep(pattern = regexB, namesOfVariables)
          newConstraint[indicesToModifyB] = 1 
            inequalities = rbind(inequalities,"<=")
          rightHandSide = rbind(rightHandSide, 3)
        }
      }
    }
  }
}

  



# constraint that no team should compete against a team coming out of their bye date more than twice a year

for(home in namesOfTeamsWithoutBye){
  for(visitor in namesOfTeamsWithoutBye){
    if(home != visitor){
      #for(playDate in namesOfPlayDates){
      i<- 1
      for(i in 2: length(namesOfPlayDates)){
        currentIndex <- i
        indexToLookFor <- i -1
  
        newConstraint = matrix(0,1,length(namesOfVariables))
        colnames(newConstraint) = namesOfVariables
        
        regex = paste("x", home, visitor, index[currentIndex],".*", sep = ".")
        regex = paste0("^",regex,"$")
        
        indicesToModify = grep(pattern = regex, namesOfVariables)
        newConstraint[indicesToModify] = 1
        
        regex2 = paste("x", visitor, home, index[currentIndex],".*", sep = ".")
        regex2 = paste0("^",regex2,"$")
        
        indicesToModify2 = grep(pattern = regex2, namesOfVariables)
        newConstraint[indicesToModify2] = 1
        
        regex3 = paste("x", "Bye", visitor, index[indexToLookFor],".*", sep = ".")
        regex3 = paste0("^",regex3,"$")
        
        indicesToModify3 = grep(pattern = regex3, namesOfVariables)
        newConstraint[indicesToModify3] = 1
        
        regex4 = paste("x", visitor, "Bye", index[indexToLookFor],".*", sep = ".")
        regex4 = paste0("^",regex4,"$")
        
        indicesToModify4 = grep(pattern = regex4, namesOfVariables)
        newConstraint[indicesToModify4] = 1
        
        regex5 = paste("b", home, index[currentIndex],sep = ".")
        regex5 = paste0("^",regex5,"$")
        
        indicesToModify5 = grep(pattern = regex5, namesOfVariables)
        newConstraint[indicesToModify5] = -1
        
        
        
        #update our constraint matrix, inequalities, and right hand side:
        constraintMatrix = rbind(constraintMatrix,newConstraint)
        inequalities = rbind(inequalities,"<=")
        rightHandSide = rbind(rightHandSide, 1)
        
      }
    }
  }
}
# 
# 
# 
# 
for(home in namesOfTeamsWithoutBye){
  
  i<- 1
  for(i in 1: length(namesOfPlayDatesMinusFirst)){
    
    currentIndex <- i
    
    
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    
    regex = paste("b", home,  index[currentIndex], sep = ".")
    regex = paste0("^",regex,"$")
    
    indicesToModify = grep(pattern = regex, namesOfVariables)
    newConstraint[indicesToModify] = 1
    
    
    constraintMatrix = rbind(constraintMatrix,newConstraint)
    inequalities = rbind(inequalities,"<=")
    rightHandSide = rbind(rightHandSide, 2)
    
    
    
    
    
  }
}



# 




##################

#update our constraint matrix, inequalities, and right hand side



#now we'll declare an LP object:
myFirstLP = make.lp(NROW(constraintMatrix),NCOL(constraintMatrix))

#the next line tells the lpSolve solver that all variables (from 1 to the number of columns in the constraint matrix) are strictly binary:  0 or 1 only, no 1/2, 1/3, etc.
set.type(myFirstLP,1:NCOL(constraintMatrix),type=c("binary"))

set.objfn(myFirstLP,objectiveFunction)

# Set control for a minimization problem
lp.control(myFirstLP,sense='min') # 'min' or 'max'

#each row of our LP object needs to be set individually.  Let's use a for-loop to accomplish this onerous task!
for(rowCounter in 1:NROW(constraintMatrix)){
  set.row(myFirstLP,rowCounter,constraintMatrix[rowCounter,])#note that constraintMatrix[rowCounter,] pulls out the rowCounterth row of the constraint matrix
  set.constr.type(myFirstLP,inequalities[rowCounter,1],rowCounter)#make sure your inequalities are all either "<=", ">=", or "="
  set.rhs(myFirstLP, rightHandSide[rowCounter,1], rowCounter)
}

# This next line is optional.  You can write your lp object to a .lp data file that you can view in the LPSolveAPI.  Note that this actually creates a file in your working directory that you could open separately in the version of LPSolve we've been dealing with. 
write.lp(myFirstLP,'ProjectMensAllConstraints.mps',type='mps')

# And finally, solve it!  
#When problems get big, this next line might take a few seconds, a few minutes, an hour or two....if it takes longer, ask me about COIN-OR CBC.  It has way more horsepower than LPSolve!
#solve(myFirstLP)

optimalSolutionVector = coinOrCbcSolutionParser("ProjectMen.txt", namesOfVariables)
optimalObjectiveValue =get.objective(myFirstLP)
#optimalSolutionVector = matrix(get.variables(myFirstLP),1,length(namesOfVariables)) #we have to coerce the output to be understandable to R as a matrix

#rename variables in optimalSolutionVector so they make sense
colnames(optimalSolutionVector) = namesOfVariables
whereItsAt = namesOfVariables[which(optimalSolutionVector != 0)]#our optimal solution vector, in all its glory!
optimalObjectiveValue

results = matrix("",nrow(tableMatrix),ncol(tableMatrix))
row.names(results) = namesOfTeams
colnames(results) = namesOfPlayDates

results2 = matrix("",nrow(tableMatrix),nrow(tableMatrix))
row.names(results2) = namesOfTeams
colnames(results2) = namesOfTeams



View(optimalSolutionVector)
for(home in namesOfTeams){
  for(visitor in namesOfTeams){
    for(playDate in namesOfPlayDates){
      newVariable = paste("x", home, visitor, playDate, wday(playDate), sep = ".")
      if(length(grep(newVariable,whereItsAt))>0)
      {
        results[home,playDate] = paste0("vs ", visitor)
        results[visitor,playDate] = paste0("@ ", home)
        
      }
    }
  }
}
for(home in namesOfTeams){
  for(visitor in namesOfTeams){
    for(playDate in namesOfPlayDates){
      newVariable = paste("x", home, visitor, playDate, wday(playDate), sep = ".")
      if(length(grep(newVariable,whereItsAt))>0)
      {
        results2[home,visitor] = paste0("TRUE")
        
        #results2[visitor,home] = paste0("TRUE")
        
        results2[visitor,home] = paste0("FALSE")
      }
    }
  }
}
matchUps2023 <- results2
matchUps2023
# costs = matrix("",nrow(tableMatrix),ncol(tableMatrix))
# row.names(costs) = namesOfTeams
# colnames(costs) = namesOfPlayDates
# for(home in namesOfTeamsWithoutBye){
#   for(visitor in namesOfTeamsWithoutBye){
#     for(playDate in namesOfPlayDates){
#       newVariable1 = paste("x", home, visitor, playDate, wday(playDate), sep = ".")
#      newVariable2 = paste("x", visitor, home, playDate, wday(playDate), sep = ".")
#       if(length(grep(newVariable1,whereItsAt))>0)
#       {
#         costs[home,playDate] = paste0(objectiveFunction[grep(newVariable1, namesOfVariables)])
#       
#         
#       }
#    
#     }
#   }
# }
# for(team in namesOfTeams){
#   for(playDate in namesOfPlayDates){
#     newVariable = paste("x",variable,team,sep = ".") 
#     if(length(grep(newVariable,whereItsAt))>0)
#     {
#       results[variable,team] = paste0("X (",distanceMatrix[variable,team],")")
#     }
#   }
# }

results
#costs
library("xlsx")
write_xlsx(results, "C:/Users/13204/Documents/ProjectResults.xlsx")
write_xlsx(dataFrame,"C:/Users/13204/Documents/ProjectResults2024.xlsx" )
write_xlsx(x = daily, path = "daily.xlsx", col_names = TRUE)
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${C:/Users/13204/Documents}"', con = "~/.Renviron")
Sys.which("make")
View(results)
dataFrame = data.frame(results)