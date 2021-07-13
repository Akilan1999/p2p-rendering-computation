package plugin

import (
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"io/ioutil"
)

// Plugins Array of all plugins detected
type Plugins struct {
	PluginsDetected []*Plugin
}

// Plugin Information about the plugins available
type Plugin struct {
	FolderName         string
	PluginDescription  string
}

// DetectPlugins Detects all the plugins available
func DetectPlugins()(*Plugins,error){
     config, err:= config.ConfigInit()
     if err != nil {
		 return nil,err
	 }
		folders, err := ioutil.ReadDir(config.PluginPath)
		if err != nil {
			return nil, err
		}

		var plugins *Plugins = new(Plugins)

		for _, f := range folders {
			if f.IsDir() {
				//Declare variable plugin of type Plugin
				var plugin Plugin

				// Setting name of folder to plugin
				plugin.FolderName = f.Name()
				// Getting Description from file description.txt
				Description, err := ioutil.ReadFile(config.PluginPath + "/" + plugin.FolderName + "/description.txt")
				// if we os.Open returns an error then handle it
				if err != nil {
					return nil, err
				}

				// Get Description from description.txt
				plugin.PluginDescription = string(Description)

				plugins.PluginsDetected = append(plugins.PluginsDetected, &plugin)
			}
		}

	return plugins,nil
}