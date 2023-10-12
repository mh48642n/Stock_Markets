# -*- coding: utf-8 -*-
"""
Created on Mon Aug 21 17:10:05 2023

@author: marvi
"""

import openpyxl as op
import pandas as pd
import os

def menu():
    print("Data wrangling with Python\n")
    
    answer = input("""For Merging: \n\tPress 1 
            \nFor joining individual columns \n\tPress 2  
            \nFor changing xls file to xlsx file[Not working right now] \n\tPress 3
            \nFor Getting Work Directory\n\tPress 4
            \nFor ECO400 data file creation[advised to put all files being merged in the same directory]: \n\tPress 5
            \n\nChoice: """)
    
    return answer
    
#this function assumes that any columns added are all of the same length
#And have each of the datasets on a sheet with the same name
def addingToDict(col_name, col_index):
    print("\n\nWhat is the range of these datasets(all have to be the same)")
    begin = int(input("\tBeginning: "))
    end = int(input("\tEnding on: ")) + 1
    sheetName = input("Sheet name: ")
    
    value_dict = {}
    
    columns = int(input("How many columns are you adding: "))
   
    while columns > 0:
        
        data = findingColumns(col_index, begin, end, sheetName)
        if(col_name != "dates"):
            data = pd.Series(data)
            data = data.apply(pd.to_numeric, errors = 'coerce')  
            
        value_dict[col_name] = data
        
        columns -= 1
        if(columns != 0):
            print("\tColumns left: ", columns)
            
            col_name = input("\nWhat you want new column to be named in file A: ")
            col_index = int(input("Index of the column being read(column A = 1): "))
            continue
        else:
            answer = input("Are you adding another column T/F: ")
            if(answer == "F"):
                columns == -1
            else:
                columns += 1
                col_name = input("\nWhat you want new column to be named in file A: ")
                col_index = int(input("Index of the column being read(column A = 1): "))
                continue
      

        
    dataset = pd.DataFrame(value_dict)
    return dataset
    
def findingColumns(col_index, begin, end, sheetName):
    print(r"""Put all .xls and .xlsx files be intereacted with in the same directory\n
          \te.g. C:\Users\Downloads or if there's a folder C:\Users\Downloads\_folder_new """)
    
    fileName = input("\nFile(path)that you want join columns from: ")
    
    bk = op.load_workbook(fileName) 
    bk.active = bk[sheetName]
    sheet = bk.active
    
    #compiles data into list to be stored in a range 
    data = [sheet.cell(row = i, column = col_index).value for i in range(begin, end)]
    bk.close()
    
    return data

# This function is made to merge multiple excel files together 
# An important aspect of this is that the first merge will be left merge in the following function
# Thus when you do a second merge keep that in mind 
# You have to know what columns to join on 
# For the 400 project selection start will equal true  
def mergingtables(start):

    if(start == False):
        num = int(input("Number of merges you're doing today"))

        file1 = input("\n\tLeft(just filename): ")
        file2 = input("\tRight(just filename): ")
    
        left = pd.read_excel(file1, sheet_name = "Sheet 1")
        right = pd.read_excel(file2, sheet_name = "Sheet 1")
        
        how_to = input("\n\tJoin how?(left, right, inner, outer):")
        on_what = input("\tJoin on what column: ")
        
        merge = pd.merge(left, right, how = how_to, on = on_what, sort = True)
        num = num - 1

        for i in range(num):
            right = input("\n\tRight(just filename): ")
            right = pd.read_excel(right, sheet_name = "Sheet 1")

            how_to = input("\n\tJoin how?(left, right, inner, outer):")
            on_what = input("\tJoin on what column: ")
            
            merge = pd.merge(merge, right, how = how_to, on = on_what, sort = True)
        
    if(start == True):
        left = pd.read_excel("indices_vals.xlsx", sheet_name = "Sheet 1")
        right = pd.read_excel("debt_to_the_penny.xlsx", sheet_name = "Sheet 1")

        merge = pd.merge(left, right, how = "left", on = "dates", sort = True)

        files = {0: "macro_factors.xlsx", 1: "debt_ceiling_dates.xlsx"}

        for values in files.items():
            right = pd.read_excel(values, sheet_name = "Sheet 1")
            merge = pd.merge(merge, right, how = "left", on = "dates", sort = True)
        


# This menu has 5 possible selections 
# And it works until the user says no or in this case N
keep_going = "Y"

# Loop choices of data manipulation and merging
while(keep_going == "Y"):
    answer = int(menu())

    match answer: 
        case 1:
            print("Welcome to the file_merger")
            cond = "Yes" 
            
            while(cond == "Yes"):
                start = False
                merge = mergingtables(start)
                merge.to_excel(input("Name of new file(.xlsx or .xls): "), sheet_name = "Sheet 1", index = False)
                cond = input("Continue merging, Yes or No: ")
                merge = None
        case 2:
            print("Welcome to the column_joiner")
            file = input("New file name(.xlsx or .xls): ")
            col_name = input("\nWhat you want new column to be named in the new excel workbook: ")
            col_index = int(input("Index of the column being read(so column A is 1):"))    
            
            data = addingToDict(col_name, col_index)
            data.to_excel(file, sheet_name = "Sheet 1", index = False)
            data = None
        case 3: 
            print("This is file converter, only for xls to xlsx!!!")
            #convert_files()
        case 4:
            path_nm = os.getcwd()  
            print(path_nm)      
        case 5: 
            print("Do you want to change working directory from:", os.getcwd() , "\n")
            confirm = input("\tConfirm: ")

            if(confirm == "Yes"):    
                while(cont == True):
                    cont = False
                    try:    
                        os.chdir(input("Name of path: "))
                    except:
                        print("""\nType the path with double back-slashes instead
                              \nOr if you messed up with writing the path then retype it""")
                        cont = True
                    
            cond = "Yes"
            while(cond == "Yes"):
                start = True
                merge = mergingtables(start)     
                merge.to_excel(input("Name of new file(.xlsx or .xls): "), sheet_name = "Sheet 1", index = False)
                merge = None
                cond = "No"
    
    keep_going = input("Continue the program? Y or N: ")
    print("\n\n")
            

print("\n\nHave fun analyzing this data!!!!")
    
    



