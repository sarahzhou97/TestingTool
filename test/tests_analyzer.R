#z-test
z.test.fromdata(5,5,10,10)
z.test.fromdata(12,12,10,10) #should raise error (number of success > number of samples)

#t-test from data
t.test.fromdata(c(1,2,3,4,5,6,7,8,9,0),c(0,2,4,5,7,8,4,3,3,45,6,8,8,5,4,3))
t.test.fromdata(c(),c()) #should raise error (empty vectors)

#t-test from values
t.test.fromvalues(5,9,1,3,39,45)