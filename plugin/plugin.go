package plugin

import (
	"context"
	"errors"
	"fmt"
	"git.sr.ht/~akilan1999/p2p-rendering-computation/config"
	"gopkg.in/yaml.v2"
	"io/ioutil"
	"strconv"

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

// Host Struct for ansible host
// Generated from https://zhwt.github.io/yaml-to-go/
type Host struct {
	All struct {
		Vars struct {
			AnsiblePythonInterpreter string `yaml:"ansible_python_interpreter"`
		} `yaml:"vars"`
	} `yaml:"all"`
	Main struct {
		Hosts struct {
			Host1 struct {
				AnsibleHost     string `yaml:"ansible_host"`
				AnsiblePort     int    `yaml:"ansible_port"`
				AnsibleUser     string `yaml:"ansible_user"`
				AnsibleSSHPass  string `yaml:"ansible_ssh_pass"`
				AnsibleSudoPass string `yaml:"ansible_sudo_pass"`
			} `yaml:"host1"`
		} `yaml:"hosts"`
	} `yaml:"main"`
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
		// Modify ansible hosts before executing
		err = execute.ModifyHost(p,config.PluginPath)
		if err != nil {
			return err
		}

		err = execute.RunAnsible(p,config.PluginPath)
		if err != nil {
			return err
		}
		// If ran successfully then change success flag to true
		execute.Success = true
	}
	return nil
}

// RunAnsible Executes based on credentials on the struct
func (e *ExecuteIP)RunAnsible(p *Plugin,path string) error {
	ansiblePlaybookConnectionOptions := &options.AnsibleConnectionOptions{
		User: "master",
	}

	ansiblePlaybookOptions := &playbook.AnsiblePlaybookOptions{
		Inventory: path + "/" + p.FolderName + "/hosts",
		ExtraVars: map[string]interface{}{"host_key_checking":"false"},
	}

	ansiblePlaybookPrivilegeEscalationOptions := &options.AnsiblePrivilegeEscalationOptions{
		Become:        true,
	}

	playbook := &playbook.AnsiblePlaybookCmd{
		Playbooks:                  []string{path + "/" + p.FolderName + "/site.yml"},
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

// ModifyHost adds IP address , port no to the config file
func (e *ExecuteIP)ModifyHost(p *Plugin, path string) error {
    host,err := ReadHost(path + "/" + p.FolderName + "/hosts")
    if err != nil {
    	return err
	}
    // Setting ansible host
	host.Main.Hosts.Host1.AnsibleHost = e.IPAddress
	// Setting SSH port no
	host.Main.Hosts.Host1.AnsiblePort, err = strconv.Atoi(e.SSHPortNo)
	if err != nil {
		return err
	}
	// Setting SSH user name
	host.Main.Hosts.Host1.AnsibleUser = "master"
	// Setting SSH password
	host.Main.Hosts.Host1.AnsibleSSHPass = "password"
	// Setting SSH sudo password
	host.Main.Hosts.Host1.AnsibleSudoPass = "password"

	// write modified information to the hosts yaml file
	data,err := yaml.Marshal(host)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(path + "/" + p.FolderName + "/hosts",data,0777)
	if err != nil {
		return err
	}
	return nil
}

// ReadHost Reads host file and adds
func ReadHost(filename string) (*Host, error) {
	buf, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	c := &Host{}
	err = yaml.Unmarshal(buf, c)
	if err != nil {
		return nil, fmt.Errorf("in file %q: %v", filename, err)
	}

	return c, nil
}
