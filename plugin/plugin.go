package plugin

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"strconv"
	"text/template"

	"github.com/Akilan1999/p2p-rendering-computation/client"
	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/google/uuid"
	"gopkg.in/yaml.v2"

	"github.com/apenella/go-ansible/pkg/execute"
	"github.com/apenella/go-ansible/pkg/options"
	"github.com/apenella/go-ansible/pkg/playbook"
	"github.com/apenella/go-ansible/pkg/stdoutcallback/results"
	"github.com/otiai10/copy"
)

// Plugins Array of all plugins detected
type Plugins struct {
	PluginsDetected []*Plugin
}

// Plugin Information about the plugins available
type Plugin struct {
	FolderName        string
	PluginDescription string
	path              string
	Execute           []*ExecuteIP
	NumOfPorts        int
	PluginArgs        []string
}

// ExecuteIP IP Address to execute Ansible instruction
type ExecuteIP struct {
	ContainerID string
	IPAddress   string
	SSHPortNo   string
	Success     bool
}

// Host Struct for ansible host
// Generated from https://zhwt.github.io/yaml-to-go/
type Host struct {
	All struct {
		Vars struct {
			AnsiblePythonInterpreter string `yaml:"ansible_python_interpreter"`
			AnsiblePrivateKeyFile    string `yaml:"ansible_ssh_private_key_file"`
		} `yaml:"vars"`
	} `yaml:"all"`
	Main struct {
		Hosts struct {
			Host1 struct {
				AnsibleHost string `yaml:"ansible_host"`
				AnsiblePort int    `yaml:"ansible_port"`
				AnsibleUser string `yaml:"ansible_user"`
				// AnsibleSSHPass  string `yaml:"ansible_ssh_pass"`
				// AnsibleSudoPass string `yaml:"ansible_sudo_pass"`
			} `yaml:"host1"`
		} `yaml:"hosts"`
	} `yaml:"main"`
}

// DetectPlugins Detects all the plugins available
func DetectPlugins() (*Plugins, error) {
	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return nil, err
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
			// Set plugin path
			plugin.path = config.PluginPath + "/" + plugin.FolderName

			plugins.PluginsDetected = append(plugins.PluginsDetected, &plugin)
			// Get the number of ports the plugin needs
			err = plugin.NumPorts()
			if err != nil {
				return nil, err
			}
		}
	}

	return plugins, nil
}

// SearchPlugin Detects plugin information based on the
// name provided on the parameter
func SearchPlugin(pluginname string) (*Plugin, error) {
	plugins, err := DetectPlugins()
	if err != nil {
		return nil, err
	}

	// loop ot find the plugin name that matches
	for _, plugin := range plugins.PluginsDetected {
		if pluginname == plugin.FolderName {
			return plugin, nil
		}
	}

	return nil, errors.New("plugin not detected")
}

// RunPlugin Executes plugins based on the plugin name provided
func RunPlugin(pluginName string, IPAddresses []*ExecuteIP, PluginArgs []string) (*Plugin, error) {
	plugins, err := DetectPlugins()
	if err != nil {
		return nil, err
	}

	// Variable to store struct information about the plugin
	var plugindetected *Plugin
	for _, plugin := range plugins.PluginsDetected {
		if plugin.FolderName == pluginName {
			plugindetected = plugin
			plugindetected.Execute = IPAddresses
			// Get Execute plugin path from config file
			config, err := config.ConfigInit(nil, nil)
			if err != nil {
				return nil, err
			}
			plugindetected.path = config.PluginPath
			break
		}
	}

	if plugindetected == nil {
		return nil, errors.New("Plugin not detected")
	}

	plugindetected.PluginArgs = append(plugindetected.PluginArgs, PluginArgs...)

	// Create copy of the plugin the tmp directory
	// To ensure we execute the plugin from there
	err = plugindetected.CopyToTmpPlugin()
	if err != nil {
		return nil, err
	}

	// Executing the plugin
	err = plugindetected.ExecutePlugin()
	if err != nil {
		return nil, err
	}

	return plugindetected, nil
}

// ExecutePlugin Function to execute plugins that are called
func (p *Plugin) ExecutePlugin() error {

	// Run ip address to execute ansible inside
	for _, execute := range p.Execute {
		// Modify ansible hosts before executing
		err := execute.ModifyHost(p)
		if err != nil {
			return err
		}
		// sets the ports to the plugin folder
		// err = p.AutoSetPorts(execute.ContainerID)
		err = p.ApplyPluginArgs()

		if err != nil {
			return err
		}
		err = execute.RunAnsible(p)
		if err != nil {
			return err
		}
		// If ran successfully then change success flag to true
		execute.Success = true
	}
	return nil
}

// RunAnsible Executes based on credentials on the struct
func (e *ExecuteIP) RunAnsible(p *Plugin) error {
	ansiblePlaybookConnectionOptions := &options.AnsibleConnectionOptions{
		User: "root",
	}

	conf, err := config.ConfigInit(nil, nil)
	if err != nil {
		return err
	}

	ansiblePlaybookOptions := &playbook.AnsiblePlaybookOptions{
		Inventory: p.path + "/" + p.FolderName + "/hosts",
		ExtraVars: map[string]interface{}{"ansible_ssh_common_args": "-o StrictHostKeyChecking=no",
			"ansible_ssh_private_key_file:": conf.PrivateKeyFile},
	}

	ansiblePlaybookPrivilegeEscalationOptions := &options.AnsiblePrivilegeEscalationOptions{
		Become: true,
	}

	playbook := &playbook.AnsiblePlaybookCmd{
		Playbooks:                  []string{p.path + "/" + p.FolderName + "/site.yml"},
		ConnectionOptions:          ansiblePlaybookConnectionOptions,
		PrivilegeEscalationOptions: ansiblePlaybookPrivilegeEscalationOptions,
		Options:                    ansiblePlaybookOptions,
		Exec: execute.NewDefaultExecute(
			execute.WithTransformers(
				results.Prepend("success"),
			),
		),
	}

	err = playbook.Run(context.TODO())
	if err != nil {
		return err
	}

	return nil
}

// ModifyHost adds IP address , port no to the config file
func (e *ExecuteIP) ModifyHost(p *Plugin) error {
	host, err := ReadHost(p.path + "/" + p.FolderName + "/hosts")
	if err != nil {
		return err
	}
	conf, err := config.ConfigInit(nil, nil)
	if err != nil {
		return err
	}
	host.All.Vars.AnsiblePrivateKeyFile = conf.PrivateKeyFile
	// Setting ansible host
	host.Main.Hosts.Host1.AnsibleHost = e.IPAddress
	// Setting SSH port no
	host.Main.Hosts.Host1.AnsiblePort, err = strconv.Atoi(e.SSHPortNo)
	if err != nil {
		return err
	}
	// Setting SSH user name
	host.Main.Hosts.Host1.AnsibleUser = "root"
	// Setting SSH password
	// host.Main.Hosts.Host1.AnsibleSSHPass = "password"
	// // Setting SSH sudo password
	// host.Main.Hosts.Host1.AnsibleSudoPass = "password"

	// write modified information to the hosts yaml file
	data, err := yaml.Marshal(host)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(p.path+"/"+p.FolderName+"/hosts", data, 0777)
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

// RunPluginContainer Runs ansible plugin based on plugin name and container name which
// is derived from the tracked containers file
// We pass in the group ID as a parameter because when we modify the ports taken
func RunPluginContainer(PluginName string, ContainerID string, PluginArgs []string) error {
	// Gets container information based on container ID
	ContainerInformation, err := client.GetContainerInformation(ContainerID)
	if err != nil {
		return err
	}

	// Setting Up IP's for which the plugins will be executed
	var ExecuteIPs []*ExecuteIP
	var ExecuteIP ExecuteIP
	// Getting port no of SSH port
	for _, port := range ContainerInformation.Container.Ports.PortSet {
		if port.PortName == "SSH" {
			ExecuteIP.SSHPortNo = fmt.Sprint(port.ExternalPort)
			break
		}
	}
	// Handle error if SSH port is not provided
	if ExecuteIP.SSHPortNo == "" {
		return errors.New("SSH port not found")
	}
	// Split the port no from ip address since current the IP address
	// field is populated as
	// <ip address>:<port no>

	host, _, err := net.SplitHostPort(ContainerInformation.IpAddress)
	if err != nil {
		return err
	}
	// IP address of the container
	ExecuteIP.IPAddress = host
	// Set container ID to ExecutorIP
	ExecuteIP.ContainerID = ContainerInformation.Id
	// Append IP to list of executor IP
	ExecuteIPs = append(ExecuteIPs, &ExecuteIP)
	// Run plugin to execute plugin
	_, err = RunPlugin(PluginName, ExecuteIPs, PluginArgs)
	if err != nil {
		return err
	}

	return nil
}

// CheckRunPlugin Checks if the ID belongs to the group or container
// calls the plugin function the appropriate amount of times
func CheckRunPlugin(PluginName string, ID string, PluginArgs []string) error {
	// Check if the ID belongs to the group or container ID
	id, err := client.CheckID(ID)
	if err != nil {
		return err
	}
	// When the ID belongs to a group
	if id == "group" {
		// gets the group information
		group, err := client.GetGroup(ID)
		if err != nil {
			return err
		}
		// Iterate through each container information in the group
		// and run the plugin in each of them
		for _, container := range group.TrackContainerList {
			// runs plugin for each container
			err := RunPluginContainer(PluginName, container.Id, PluginArgs)
			if err != nil {
				return err
			}
		}
	} else { // This means the following ID is a container ID
		err := RunPluginContainer(PluginName, ID, PluginArgs)
		if err != nil {
			return err
		}
	}

	return nil
}

// CopyToTmpPlugin This function would ensure that we create a copy of the
// plugin in the tmp directory, and it would be executed
// from there. This due to the reason of automating port allocation
// when running plugins
func (p *Plugin) CopyToTmpPlugin() error {
	// generate rand to UUID this is debug the ansible file if needed
	id := uuid.New()
	// copies the plugin to the tmp directory
	err := copy.Copy(p.path+"/"+p.FolderName, "/tmp/"+id.String()+"_"+p.FolderName)
	if err != nil {
		return err
	}

	// Set the plugin execution to the tmp location
	p.path = "/tmp"
	p.FolderName = id.String() + "_" + p.FolderName

	return nil
}

// AutoSetPorts Automatically maps free ports to site.yml file
func (p *Plugin) AutoSetPorts(containerID string) error {
	container, err := client.GetContainerInformation(containerID)
	if err != nil {
		return err
	}
	// variable that would have a list of ports
	// to be allocated to the plugin system
	var ports []int
	// Counted that increments when a port is taken
	PortTaken := 0
	// setting all external ports available in an array
	for i, port := range container.Container.Ports.PortSet {
		if port.IsUsed {
			// Ensuring we break outside the loop once the ports
			// are set.
			if PortTaken >= p.NumOfPorts {
				break
			}
			// Setting the following port flag to true
			container.Container.Ports.PortSet[i].IsUsed = true
			// Incrementing the variable PortTaken
			PortTaken++
			// Maps to internal since
			// Inside the machine
			// internal port -> (maps) same internal port
			// TURN (i.e FRP) based approach (internal port -> maps to different external port)
			ports = append(ports, port.InternalPort)
		}
	}

	// parses the site.yml file in the tmp directory
	t, err := template.ParseFiles(p.path + "/" + p.FolderName + "/site.yml")
	if err != nil {
		return err
	}
	// opens the output file
	f, err := os.Create(p.path + "/" + p.FolderName + "/site.yml")
	if err != nil {
		return err
	}
	// sends the ports to the site.yml file to populate them
	err = t.Execute(f, ports)
	if err != nil {
		return err
	}
	// Once the following is done set port to taken
	// n tracked container list
	err = container.ModifyContainerInformation()
	if err != nil {
		return err
	}
	// Once the following is done set port to taken
	// I(Groups)
	err = container.ModifyContainerGroups()
	if err != nil {
		return err
	}

	return nil
}

// AutoSetPorts Automatically maps free ports to site.yml file
func (p *Plugin) ApplyPluginArgs() error {

	// parses the site.yml file in the tmp directory
	t, err := template.ParseFiles(p.path + "/" + p.FolderName + "/site.yml")
	if err != nil {
		return err
	}
	// opens the output file
	f, err := os.Create(p.path + "/" + p.FolderName + "/site.yml")
	if err != nil {
		return err
	}
	// sends the ports to the site.yml file to populate them
	err = t.Execute(f, p.PluginArgs)
	if err != nil {
		return err
	}
	return nil
}

// NumPorts Gets the Number the ports the
// plugin requires
func (p *Plugin) NumPorts() error {
	jsonFile, err := os.Open(p.path + "/ports.json")
	// if we os.Open returns an error then handle it
	if err != nil {
		return err
	}

	// defer the closing of our jsonFile so that we can parse it later on
	defer jsonFile.Close()

	// read our opened xmlFile as a byte array.
	byteValue, _ := ioutil.ReadAll(jsonFile)

	// we unmarshal our byteArray which contains our
	// jsonFile's content into 'users' which we defined above
	json.Unmarshal(byteValue, &p)

	return nil
}
