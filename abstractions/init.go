package abstractions

import (
    "github.com/Akilan1999/p2p-rendering-computation/config/generate"
)

func Init(name string) {
    // set the config file with default paths
    generate.SetDefaults(name)
    
}
