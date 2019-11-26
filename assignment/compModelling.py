import csv
from datetime import date as da
from collections import Counter

def mostPopular(billList):
    return Counter(elem[0] for elem in billList) #found the fucntion counter that counts how may times something occurs on google

def inOrder(billList):
    billList.sort(key=lambda r: r[2]) #Saw a similar answer on stack overflow so I coppied the format but used a different value of the tuple. Sorts the list by date and returns teh list
    return billList

def highest(billList):
    maxCred = 0 #Works by going through the list and finding teh max credit and debit purchase by comparing to the previous max for each
    maxDeb = 0
    for i in billList:
        if i[4] == " credit" or i[4] == "credit":
            if float(i[3]) > maxCred :
                maxCred = float(i[3])
        else:
            if float(i[3])>maxDeb:
                maxDeb = float(i[3])

    return (maxCred,maxDeb)
def yearTotal(billList,year):
    yearList = 0 #works by going through list and getting years that are the same as the entered year and then from there adding to a total and getting the average
    yearAmount = 0
    for i in billList:
        if i[2].year == year:
            yearList = yearList+1
            yearAmount = yearAmount + float(i[3])
    return (yearAmount/yearList)
def monthtotal(billList,month):
    monthList = 0 #works same as yeartotal but uses month instead of year
    monthAmount = 0
    for i in billList:
        if i[2].month == month:
            monthList = monthList+1
            monthAmount = monthAmount + float(i[3])
    return (monthAmount/monthList)
def main():
    billList = []
    with open("C:/Users/legua/Downloads/results.csv") as csv_file:
        csv_reader = csv.reader(csv_file)
        rowcount =0
        for row in csv_reader:
                companyName = row[0]
                personName = row[1]
                billdate = da(int(row[2]),int(row[3]),int(row[4])) #uses the datetime function I found to create dates that makes them easier to sort for later
                amount = row[5]
                payType = row[6]
                bill = (companyName,personName,billdate,amount,payType)
                billList.append(bill) #reads each row of the file line by line and then adds the values to a tuple that's stored in a list
    inner = input(
        "Please enter if you would like to enter details(E), write all details to a file (F) ,read details from a file(R) , Generate all reports to the console(G) or type exit: ")
    while (  inner != "exit"):
        if inner == "E":
            utility_company, customer_name, amount, credOrDeb = input(
                "Please enter the company name, customer name, amount and credit or debit: ").split()
            year, month, day = input("please enter the year. month and day: ").split()
            date = da(int(year),int(month),int(day))
            bill = (utility_company,customer_name,date,amount,credOrDeb)
            billList.append(bill)
            print("Bill list has been update with your bill")
        elif inner == "F":
            with open('bills.txt', 'w') as f:
                for item in billList:
                    f.write("%s\n" % item)
            print("all bills have been written to bills.txt")
        elif inner == "R":
            file = input("Please enter the path for the file to be read from: ")
            with open(file, mode='r') as csv_file:
                csv_reader = csv.DictReader(csv_file)
                rowcount = 0
                for row in csv_reader:
                    if rowcount == 0:
                        print("")
                    else:
                        companyName = row[0]
                        personName = row[1]
                        billdate = da(row[2], row[3], row[4])
                        amount = row[5]
                        payType = row[6]
                        bill = (companyName, personName, billdate, amount, payType)
                        billList.append(bill)
                print("All bills have been read from ",file)
        elif inner == "G":
            print("THe most popular company is ",mostPopular(billList).most_common(1)[0][0])
            print(inOrder(billList))
            tuple = highest(billList)
            print( "Highest Credit: ",tuple[0], " Highest Debit: " ,tuple[1])
            inp,val = input("Please enter year or month  followed by the year or month you want the average for: ").split()
            if inp == "year":
                print("Average for the year ",val, " is ", yearTotal(billList,int(val)))
            else:
                print("Average for the month ", val, " is ", monthtotal(billList, int(val)))

        inner = input(
        "Please enter if you would like to enter details(E), write all details to a file (F) ,read details from a file(R) , Generate all reports to the console(G) or type exit: ")
if __name__ == "__main__":
    main()