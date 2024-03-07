setwd("Z:\\PPMI_Data\\Excels\\CollaborativeFiltering")
age = 48
n =3
f1 = "Apathy"
f1Val = 2
f2 = "Clock"
f2Val = 7
f3 = "UPSIT"
f3Val = 39


vec = c(f1, f1Val, f2, f2Val, f3, f3Val)
inputVec = paste(vec,  collapse =" ")
command = sprintf("python Z:\\PPMI_Data\\Excels\\CollaborativeFiltering\\Neural_andIndivi.py %s %s", age, n)
command = paste(command, inputVec, " ")
out = shell(command,intern = TRUE)
out[9]

