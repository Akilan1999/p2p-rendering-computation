# This repo consists of the P2PRC simulation to ensure 
# that we can test various nodes locally with just processes. 
# Note: This implementation is for local nodes only.

from library import *
import os
import socket
from contextlib import closing

# Stores the base information 
# required to know about a P2PRC node
@dataclass
class P2PRCNode:
    Name: str
    ConfigLocation: str
    ServerPort: str
    RunningState: bool
    RootNodePort: str

@dataclass
class P2PRCNodes:
    P2PRCNodes: List[P2PRCNode]

# Tracker for P2PRC nodes
PublicP2PRCNodes: Union[P2PRCNodes, None] = None

# Tracker of root Node
RootNode: P2PRCNode

# Sets up P2PRC with default configuration 
def SetupP2PRCNode(name=""):
    # Unset environment variables
    os.environ.pop('P2PRC', None)
    # Create Test folder
    Dirname = "P2PRC-Test-" + name
    try:
        os.mkdir(Dirname)
        print(f"Directory '{Dirname}' created successfully.")
    except FileExistsError:
        print(f"Directory '{Dirname}' already exists.")
        KillProcedure()
    
    # Set env variable for session
    envPath = os.path.abspath(os.getcwd()) + "/" + Dirname
    LoadDLL(envPath)

    # Generate random 4
    ServerPortNo = str(find_free_port())

    # Initialise data class
    p2prcNode = P2PRCNode(
        Name = name,
        ConfigLocation = envPath,
        ServerPort = ServerPortNo,
        RunningState = True,
        RootNodePort = "",
    )

    # Adds P2PRC node to memory
    AddP2PRCNodeToMemory(p2prcNode)

    # Initialise P2PRC instance
    Init()

    # Add root node to ip table
    AddRootNode("0.0.0.0",ServerPortNo)

    # Change server port no on Config
    ChangeFileValue(envPath + "/config.json", "ServerPort", ServerPortNo)

    thread = threading.Thread(target = StartServer)
    thread.start()
    

# Find free TCP port avaliable
# Source: https://stackoverflow.com/questions/1365265/on-localhost-how-do-i-pick-a-free-port-number
def find_free_port():
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(('', 0))
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return s.getsockname()[1]

# Standard function called after
# a simulation is terminated or 
# when error exception is called
def KillProcedure():
    RemoveAllP2PRCNodesFromMemoryAndConfigFile()

# -------------------------- helper functions -----------------------
def ChangeFileValue(file_path, key, new_value):
    """
    Reads a JSON file, updates a key with a new value, and writes it back.

    :param file_path: Path to the JSON file
    :param key: Key in the JSON to update
    :param new_value: New value to assign to the key
    """
    # Read from the JSON file
    with open(file_path, 'r') as file:
        data = json.load(file)

    # Update the key with the new value
    data[key] = new_value

    # Write the updated data back to the JSON file
    with open(file_path, 'w') as file:
        json.dump(data, file, indent=4)

# Adds the process dataclass the to
# the global variable PublicProcess.
def AddP2PRCNodeToMemory(p2prcNode: P2PRCNode):
    global PublicP2PRCNodes
    global RootNode
    if PublicP2PRCNodes == None:
        PublicP2PRCNodes = P2PRCNodes(P2PRCNodes=[p2prcNode])
        p2prcNode.RootNodePort = p2prcNode.ServerPort
        RootNode = p2prcNode
    else: 
        p2prcNode.RootNodePort = RootNode.ServerPort
        PublicP2PRCNodes.P2PRCNodes.append(p2prcNode)

# Remove all nodes test nodes generated
def RemoveAllP2PRCNodesFromMemoryAndConfigFile():
    global PublicP2PRCNodes
    if PublicP2PRCNodes is not None:
        for node in PublicP2PRCNodes.P2PRCNodes:
            config_path = node.ConfigLocation
            # Safely remove the folder if it exists
            if os.path.exists(config_path) and os.path.isdir(config_path):
                try:
                    shutil.rmtree(config_path)
                    print(f"Deleted folder: {config_path}")
                except Exception as e:
                    print(f"Failed to delete {config_path}: {e}")
        # Clear the node list
        PublicP2PRCNodes = None
        print("All P2PRC nodes removed from memory.")
    else:
        print("No P2PRC nodes to remove.")


if __name__ == "__main__":
    # Setting up 2 P2PRC nodes
    SetupP2PRCNode(name="Test")
    SetupP2PRCNode(name="Test1")