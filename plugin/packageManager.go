package plugin

import (
	"net/url"
	"os"
	"strings"

	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/go-git/go-git/v5"
)

// DownloadPlugin This functions downloads package from
// a git repo.
func DownloadPlugin(pluginurl string) (string, error) {
	// paring plugin url
	u, err := url.Parse(pluginurl)
	if err != nil {
		return "", err
	}
	path := u.Path
	// Trim first character of the string
	path = path[1:]
	// trim last element of the string
	path = path[:len(path)-1]
	// Replaces / with _
	folder := strings.Replace(path, "/", "_", -1)
	// Reads plugin path from the config path
	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return "", err
	}
	// clones a repo and stores it at the plugin directory
	_, err = git.PlainClone(config.PluginPath+"/"+folder, false, &git.CloneOptions{
		URL:      pluginurl,
		Progress: os.Stdout,
	})
	// returns error if raised
	if err != nil {
		return "", err
	}

	return folder, nil
}

// DeletePlugin The following function deletes a plugin based on
// the plugin name provided.
func DeletePlugin(pluginname string) error {
	config, err := config.ConfigInit(nil, nil)
	if err != nil {
		return err
	}

	plugin, err := SearchPlugin(pluginname)
	if err != nil {
		return err
	}

	// Delete the directory holding the plugin
	err = os.RemoveAll(config.PluginPath + "/" + plugin.FolderName)
	if err != nil {
		return err
	}

	return nil
}
