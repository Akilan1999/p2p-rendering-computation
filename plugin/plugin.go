package plugin

import (
	"context"
	"errors"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"io/ioutil"

	"github.com/apenella/go-ansible/pkg/execute"
	"github.com/apenella/go-ansible/pkg/options"
	"github.com/apenella/go-ansible/pkg/playbook"
	"github.com/apenella/go-ansible/pkg/stdoutcallback/results"
)

// Plugins Array of all plugins detected
type Plugins struct {
	PluginsDetected []*Plugin
}

// Plugin Information about the plugins available
type Plugin struct {
	FolderName         string
	PluginDescription  string
	Execute            []*ExecuteIP
}

// ExecuteIP IP Address to execute Ansible instruction
type ExecuteIP struct {
	IPAddress   string
	SSHPortNo   string
	Success      bool
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

// RunPlugin Executes plugins based on the plugin name provided
func RunPlugin(pluginName string ,IPAddresses []*ExecuteIP) (*Plugin,error) {
	plugins, err := DetectPlugins()
	if err != nil {
		return nil,err
	}

	// Variable to store struct information about the plugin
	var plugindetected *Plugin
	for _,plugin := range plugins.PluginsDetected {
		if plugin.FolderName == pluginName {
			plugindetected = plugin
			plugindetected.Execute = IPAddresses
			break
		}
	}

	if plugindetected == nil {
		return nil, errors.New("Plugin not detected")
	}

	// Executing the plugin
	err  = plugindetected.ExecutePlugin()
	if err != nil {
		return nil,err
	}

	return plugindetected,nil
}

// ExecutePlugin Function to execute plugins that are called
func (p *Plugin) ExecutePlugin() error {
	// Get Execute plugin path from config file
	config, err:= config.ConfigInit()
	if err != nil {
		return err
	}

	// Run ip address to execute ansible inside
	for _,execute := range p.Execute {
		err := execute.RunAnsible(p.FolderName,config.PluginPath)
		if err != nil {
			return err
		}
		// If ran successfully then change success flag to true
		execute.Success = true
	}
	return nil
}

// RunAnsible Executes based on credentials on the struct
func (e *ExecuteIP)RunAnsible(FolderName string,path string) error {
	ansiblePlaybookConnectionOptions := &options.AnsibleConnectionOptions{
		User: "master",
	}

	ansiblePlaybookOptions := &playbook.AnsiblePlaybookOptions{
		Inventory: e.IPAddress + ",",
		ExtraVars: map[string]interface{}{
			"ansible_port":e.SSHPortNo,
		},
	}

	ansiblePlaybookPrivilegeEscalationOptions := &options.AnsiblePrivilegeEscalationOptions{
		Become:        true,
		AskBecomePass: true,
	}

	playbook := &playbook.AnsiblePlaybookCmd{
		Playbooks:                  []string{path + "/" + FolderName + "site.yml"},
		ConnectionOptions:          ansiblePlaybookConnectionOptions,
		PrivilegeEscalationOptions: ansiblePlaybookPrivilegeEscalationOptions,
		Options:                    ansiblePlaybookOptions,
		Exec: execute.NewDefaultExecute(
			execute.WithTransformers(
				results.Prepend("success"),
			),
		),
	}

	err := playbook.Run(context.TODO())
	if err != nil {
		return err
	}

	return nil
}
