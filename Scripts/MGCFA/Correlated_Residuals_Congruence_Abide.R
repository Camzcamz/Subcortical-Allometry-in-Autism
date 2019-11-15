list1 = read.csv("list1.csv")
list2 = read.csv("list2.csv")
list3 = read.csv("list3.csv")
list4 = read.csv("list4.csv")
list5 = read.csv("list5.csv")

setdiff(list1,list5) # 34 
setdiff(list2,list5) # 41 
setdiff(list3,list5) # 48 
setdiff(list4,list5) # 69

setdiff(list1,list3) # 34 


