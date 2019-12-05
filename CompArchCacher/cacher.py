import math

class cache():
    def __init__(self,L,N,K):
        self.L = L
        self.N = N
        self.K = K
        self.cachetags = {}
        self.cacheLinked = {00:linkedList(),
                            1:linkedList(),
                            10:linkedList(),
                            11:linkedList(),
                            100:linkedList(),
                            101:linkedList(),
                            110:linkedList(),
                            111:linkedList(),
                            1000:linkedList()
                            }

    def cacheCheck(self,Lister):
        if self.K == 1:
            hitNum = 0
            for tag in Lister:
                ctag = self.tagConvert(tag)
                if ctag[1] in self.cachetags:
                    if ctag[0] == self.cachetags[ctag[1]]:
                        print(ctag,"hit")
                        hitNum = hitNum +1
                    else:
                        self.cachetags[ctag[1]] = ctag[0]
                        print("miss")
                else :
                    self.cachetags[ctag[1]] = ctag[0]
                    print("miss")
            print(hitNum)
        else:
            hitNum = 0
            for tag in Lister:
                ctag = self.tagConvert(tag)
                if int(ctag[1]) in self.cacheLinked:
                    linkedList = self.cacheLinked[int(ctag[1])]
                    if linkedList.swap(ctag[0],self.K) == True:
                        print("hit",tag ,ctag)
                        hitNum = hitNum+1
                    else:
                        print("miss",tag,ctag)
            print(hitNum)

    def tagConvert(self,address):
        binval = bin(int(address, 16))[2:]
        binval = str(binval)
        if len(binval) < 4+math.log(self.N,2):
            while len(binval) <=  4+math.log(self.N,2):
                binval = "0" + binval
        tagTuple = (binval[0:len(binval)-(4+int(math.log(self.N,2)))],
                    binval[len(binval)-(4+int(math.log(self.N,2))):len(binval)-4])
        if tagTuple[0] == "" and tagTuple[1] == "":
            return ('0','0')
        elif tagTuple[0] == "" and tagTuple[1] != "":
            return ('0',tagTuple[1])
        elif tagTuple[0] != "" and tagTuple[1] == "":
            return (tagTuple[0],'0')
        return tagTuple

class linkedList():
    def __init__(self):
        self.head = None
        self.length = 0
    class node():
        def __init__(self,val):
            self.val = val
            self.next = None

    def swap(self,val,k):
        if self.head  != None:
            if self.head.val == val:
                return True
            else:
                newTail = None
                curNode = self.head
                prevNode = None
                while curNode != None and curNode.val != val:
                    newTail = prevNode
                    prevNode = curNode
                    curNode = curNode.next
                if(curNode != None):
                    if (prevNode.val != self.head.val):
                        prevNode.next = curNode.next
                    else:
                        self.head.next = curNode.next
                    curNode.next = self.head
                    self.head = curNode
                    return True
                else:
                    if(self.length < k):
                        noder = self.node(val)
                        noder.next = self.head
                        self.head =noder
                        self.length = self.length +1
                        return False
                    else:
                        newTail.next = None
                        noder = self.node(val)
                        noder.next = self.head
                        self.head = noder
                        return False
        else:
            noder = self.node(val)
            self.head = noder
            self.length += 1
            return False
def main():
    cacher = cache(16,1,8)
    list = ["0000","0004","000c","2200","00d0","00e0","1130","0028","113c","2204","0010","0020","0004"
        ,"0040","2208","0008","00a0","0004","1104","0028","000c","0084","000c"
        ,"3390","00b0","1100","0028","0064","0070","00d0","0008","3394"]
    cacher.cacheCheck(list)

if __name__ == '__main__':
    main()