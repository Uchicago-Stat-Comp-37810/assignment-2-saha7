#!/usr/bin/env python
# coding: utf-8

# In[ ]:


cars = 100 # we put a value into a variable
space_in_a_car = 4.0 # we put a value into a variable
drivers = 30 # we put a value into a variable
passengers = 90 # we put a value into a variable
cars_not_driven = cars - drivers # We define a variable using two other variables
cars_driven = drivers # We define a variable using other variable
carpool_capacity = cars_driven * space_in_a_car # We define a variable using two other variables
average_passengers_per_car = passengers / cars_driven # We define a variable using two other variables


print "There are", cars, "cars available." # The specified value goes into 'cars' (=100)
print "There are only", drivers, "drivers available." # The specified value goes into 'drivers' (=30)
print "There will be", cars_not_driven, "empty cars today." # The specified value goes into 'cars_not_driven' 
print "We can transport", carpool_capacity, "people today." # The specified value goes into 'carpool_capacity' 
print "We have", passengers, "to carpool today." # The specified value goes into 'passengers' (=90)
print "We need to put about", average_passengers_per_car, "in each car." # The specified value goes into 'average_passengers_per_car'

