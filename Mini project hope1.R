setwd("C:/Users/13204/Documents")
#Jake Peters

library(readxl)#We import this library so that we can read in things from Excel files.
library(lpSolve)
library(lpSolveAPI)

#################################################################################################################################################################################################################################
#This inital code is from the initial mini project code by Dr. Axvig
#get data in
skillsNeeded = as.matrix(read_excel("skillsNeeded.xlsx"))
namesOfPeople = skillsNeeded[,1]
skillsNeededMatrix = matrix(as.numeric(skillsNeeded[,2:ncol(skillsNeeded)]),nrow = nrow(skillsNeeded),ncol = ncol(skillsNeeded) -1)
colnames(skillsNeededMatrix) = colnames(skillsNeeded)[2:ncol(skillsNeeded)]
rownames(skillsNeededMatrix) = namesOfPeople
View(namesOfPeople)
skillsOffered = as.matrix(read_excel("skillsOffered.xlsx"))
namesOfPrograms = skillsOffered[,1]
skillsOfferedMatrix = matrix(as.numeric(skillsOffered[,2:(ncol(skillsOffered) -1)]),nrow=nrow(skillsOffered),ncol=ncol(skillsOffered)-2)
colnames(skillsOfferedMatrix) = colnames(skillsOffered)[2:(ncol(skillsOffered)-1)]
rownames(skillsOfferedMatrix) = namesOfPrograms
View(programConflictsListRegExp)
programCosts = matrix(as.numeric(skillsOffered[,ncol(skillsOffered)]),nrow(skillsOffered),1)
row.names(programCosts) = namesOfPrograms

programConflicts =  as.matrix(read_excel("programConflicts.xlsx"))
programConflictsList = matrix(programConflicts[,2],nrow(programConflicts),1)
programConflictsListRegExp = paste("(",gsub(",","|",programConflictsList),")")
View(programConflictsListRegExp)
programConflictsListRegExp = gsub(" ","",programConflictsListRegExp)
programConflictsListRegExp = matrix(programConflictsListRegExp,length(programConflictsListRegExp),1)
row.names(programConflictsListRegExp) = namesOfPrograms
#let's create some variables in the spirit of our past ones!
namesOfVariables = c()#initialize an empty list

for(person in namesOfPeople){
  for(program in namesOfPrograms){
    newVariable = paste("x",person,program,sep = ".") 
    namesOfVariables = c(namesOfVariables,newVariable)#tack the new one onto the end of the list
  }
}

namesOfVariables
length(namesOfVariables)

#################################################################################################################################################################################################################################

#CONSTRAINTS

#initialize objects to store stuff in
constraintMatrix = matrix(0,0,length(namesOfVariables))
colnames(constraintMatrix) = namesOfVariables
inequalities = matrix("",0,1)
rightHandSide = matrix(0,0,1)

#now let's add THE CONSTRAINT THAT AT MOST 3 CAN REGISTER FOR ANY GIVEN PROGRAM

for(program in namesOfPrograms){
  #Select programs using a regular expression that modifies the indicies to modify to 1 based on the employee for each program
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  regularExpressionToLookFor = paste0("^x\\..*\\.",program,"$")#the .* indicates any symbol for any length of a run.  basically, it grabs any name between two literal periods.
  
  indicesToModify = grep(pattern = regularExpressionToLookFor,namesOfVariables)
  newConstraint[indicesToModify] = 1
  
  #update our constraint matrix, inequalities, and right hand side:
  constraintMatrix = rbind(constraintMatrix,newConstraint)
  inequalities = rbind(inequalities,"<=")
  rightHandSide = rbind(rightHandSide,3)
  
}


#Now you take it from here!  Write the other families of constraints, create an LP object (day2script1.R will help), solve it, and display the optimal objective value and optimal solution. :)



#THE CONSTRAINT STATES THAT EACH PERSON IS TRAINED TO THE SKILLS THEY NEED

for(employee in namesOfPeople){
  #setting up the skillsNeededMatrix for the employees
  rowForThatPerson = skillsNeededMatrix[employee,]
  skillsToGrabIndices = which(rowForThatPerson == 1)
  skillsToGrab = colnames(skillsNeededMatrix)[skillsToGrabIndices]
  
  for(skill in skillsToGrab){
    #the first 
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    currentSkill = skill
    columnForThatSkill = skillsOfferedMatrix[,currentSkill]
    programsToGrabIndices = which(columnForThatSkill == 1)
    programsToGrab = namesOfPrograms[programsToGrabIndices]
    #select the conlficting skills based on the regex and modifying them to 1 based on whether they conlficted with the original program
    #for each skill, then for each employee
    programsToGrabRegExp = paste0("(",paste(programsToGrab,collapse = "|"),")")
    pattern = paste0("^x\\.",employee,"\\.",programsToGrabRegExp,"$")
    indicesToModify = grep(pattern,namesOfVariables)
    newConstraint[indicesToModify] = 1
    
    #update our constraint matrix, inequalities, and right hand side:
    constraintMatrix = rbind(constraintMatrix,newConstraint)
    inequalities = rbind(inequalities,">=")
    rightHandSide = rbind(rightHandSide,1)
    
  }
}




#THE CONSTRAINT STATES THAT NO ONE CAN ATTEND TWO TRAINING SESSIONS THAT CONFLICT WITH ONE ANOTHER

for(employee in namesOfPeople){
  
  rowForThatPerson = skillsNeededMatrix[employee,]
  skillsToGrabIndices = which(rowForThatPerson == 1)
  skillsToGrab = colnames(skillsNeededMatrix)[skillsToGrabIndices]
  
  for(program in namesOfPrograms){
    #Big M part that modifies the first indice of each program for each employee to 1000
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    regularExpressionToLookFor = paste0("^x\\.",employee,"\\.",program,"$")#the .* indicates any symbol for any length of a run.  basically, it grabs any name between two literal periods.

    indicesToModify = grep(pattern = regularExpressionToLookFor,namesOfVariables)
    newConstraint[indicesToModify] = 1000
    
    #mark indicies to modify if the person needs that program, for each program, for each employee
   #regularExpressionToLookFor = paste0("^x\\.",employee,"\\.Program ",programConflictsListRegExp[program, 1],"$")#the .* indicates any symbol for any length of a run.  basically, it grabs any name between two literal periods.
    regularExpressionToLookFor = paste0("^x\\.",employee,"\\.Program ",programConflictsListRegExp[program, 1],"$")
    indicesToModify = grep(pattern = regularExpressionToLookFor,namesOfVariables)
    newConstraint[indicesToModify] = 1
    
    
    
    
    #update our constraint matrix, inequalities, and right hand side:
    constraintMatrix = rbind(constraintMatrix,newConstraint)
    inequalities = rbind(inequalities,"<=")
    rightHandSide = rbind(rightHandSide,1000)
    
  }
}



#View(namesOfVariables)
#View(cbind(constraintMatrix,inequalities,rightHandSide))
#our objective function for this LP that takes in the cost of each program for each employee.
objectiveFunction = matrix(programCosts,1,length(namesOfVariables))

#Create a one-row matrix that is filled with program costs in our columns. I figured this would return an error as the length of 
#variables is not equal to the number of program costs. 

#Joe Addy and I irst tried using a loop for the objective function, but found that this method works the same way

#View(objectiveFunction)
#for(program in namesOfPrograms)
#{
  #objectiveFunction = matrix(0,1,length(namesOfVariables))
  #Regu

#}
  
#This code is from day2script1.R from Dr. Axvig
#now we'll declare an LP object:
myFirstLP = make.lp(NROW(constraintMatrix),NCOL(constraintMatrix))

#the next line tells the lpSolve solver that all variables (from 1 to the number of columns in the constraint matrix) are strictly binary:  0 or 1 only, no 1/2, 1/3, etc.
set.type(myFirstLP,1:NCOL(constraintMatrix),type=c("binary"))

# Let's use our objective function!
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
write.lp(myFirstLP,'miniProject.mps',type='mps')

# And finally, solve it!  
#When problems get big, this next line might take a few seconds, a few minutes, an hour or two....if it takes longer, ask me about COIN-OR CBC.  It has way more horsepower than LPSolve!
solve(myFirstLP)
optimalObjectiveValue =get.objective(myFirstLP)
optimalObjectiveValue
optimalSolutionVector = matrix(get.variables(myFirstLP),1,length(namesOfVariables)) #we have to coerce the output to be understandable to R as a matrix

#rename variables in optimalSolutionVector so they make sense
colnames(optimalSolutionVector) = namesOfVariables
optimalSolutionVector#our optimal solution vector, in all its glor








