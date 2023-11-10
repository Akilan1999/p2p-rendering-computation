import ctypes

p2prc = ctypes.CDLL("SharedOBjects/p2prc.so")

p2prc.Init("")

def StartServer():
    # Starting P2PRC as a server mode
    p2prc.Server()
    for _ in iter(int, 1):
        pass