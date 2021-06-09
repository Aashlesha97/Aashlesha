#import excel file with the columns to be correlated
view(mydata)

#the corr function will correlate the columns present in the excel file
cor(mydata, use = "pairwise.complete.obs")

 #the correlation information is stored in the object M
M <- cor(mydata, use = "pairwise.complete.obs")

#corrplot is used to form the correlation matrix. 
#using different fucntions within corrplot, different forms if matrix can be formed

corrplot (M, method = "circle")
corrplot (M, typr = "upper")
corrplot (M, type = "lower")
