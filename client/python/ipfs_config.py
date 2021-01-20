'''
This scrypt creates basic config for IPFS server 
'''
import ipfsApi

def config():
    api = ipfsApi.Client('127.0.0.1', 5001)
    return api