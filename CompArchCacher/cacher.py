import math

hex2bin_map = {
            "0": "0000",
            "1": "0001",
            "2": "0010",
            "3": "0011",
            "4": "0100",
            "5": "0101",
            "6": "0110",
            "7": "0111",
            "8": "1000",
            "9": "1001",
            "a": "1010",
            "b": "1011",
            "c": "1100",
            "d": "1101",
            "e": "1110",
            "f": "1111",
        }
class cache():
    def __init__(self,L,N,K):
        self.L = L
        self.N = N
        self.K = K
        self.cachetags = []

    def cacheCheck(self,Lister):
        if self.K == 1:
            for tag in Lister:
                ctag = self.tagConvert(tag)
                hitNum = 0
                missnUm = 1
                if ctag in self.cachetags:
                    print(tag + "cache hit")
                else:
                    if (len(self.cachetags) < 8):
                        print(tag + ctag + "cache miss")
                        self.cachetags.append(ctag)
                    else:
                        for tager in self.cachetags:
                            if tager[1] == ctag[1]:
                                tager = ctag

    def tagConvert(self,address):
        fullTag = None
        for i in address:
            fullTag.append(hex2bin_map[i])
        tagTuple = (fullTag[math.log(self.N,2)::-1],fullTag[4:math.log(self.N,2):-1])
        return tagTuple
def main():
    cacher = cache(16,8,1)
    list = ["0000","0004","000c","2200","00d0","00e0","1130","0028","113c","2204","0010","0020","0004"
        ,"0040","2208","0008","00a0","0004","1104","0028","000c","0084","000c"
        ,"3390","00b0","1100","0028","0064","0070","00d0","0008","3394"]
    cacher.tagConvert(cacher,list)
