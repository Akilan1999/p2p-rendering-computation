'''
This scrypt uses curl to list servers in the IPFS network
'''
import requests
import json

def list_servers(): 
  peers = requests.post('http://127.0.0.1:5001/api/v0/swarm/peers')
  # TODO: Detect nodes that are servers 
  
  peers_dict = peers.json()
  # Ping for each node
  for i in peers_dict['Peers']:
      ping = requests.post('http://127.0.0.1:5001/api/v0/ping?arg='+i['Peer']+'&count=1')
      # Create a replace function TODO
      print(str(ping.content)[2:-3])

list_servers()