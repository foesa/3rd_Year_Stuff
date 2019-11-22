class cacher:
    def __init__(self,L,N,K):
        self.L = L
        self.K = K
        self.N = N
        self.tagList = None

    def cacheCheck(self,tags):
        if self.K == 8:
            for tags 