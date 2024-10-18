package generate

import (
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"io/ioutil"
	"os"

	"github.com/Akilan1999/p2p-rendering-computation/config"
	"github.com/Akilan1999/p2p-rendering-computation/p2p"
	"github.com/go-git/go-git/v5"
	"golang.org/x/crypto/ssh"
)

// GenerateFiles Generates all the files needed to setup P2PRC
func GenerateFiles(rootNodes ...p2p.IpAddress) (err error) {
	err = GenerateIPTableFile(rootNodes)
	if err != nil {
		return err
	}
	err = GenerateDockerFiles()
	if err != nil {
		return err
	}
	// err = GeneratePluginDirectory()
	// if err != nil {
	// 	return err
	// }
	err = GenerateClientTrackContainers()
	return
}

// GenerateIPTableFile Generates the IPTable file with the appropirate root node
func GenerateIPTableFile(rootNodes []p2p.IpAddress) (err error) {
	var rootnodes p2p.IpAddresses
	var rootnode p2p.IpAddress

	err = CreateIPTableFolderStructure()
	if err != nil {
		return err
	}

	// If root node addresses are not provided as optional parameters
	if len(rootNodes) <= 0 {
		rootnode.Name = "Node1"
		rootnode.ServerPort = "9999"
		rootnode.NAT = "False"
		rootnode.Ipv4 = "5.252.55.163"

		rootnodes.IpAddress = append(rootnodes.IpAddress, rootnode)
	} else {
		// if root nodes are provided then override them as the optional parameter
		for i := range rootNodes {
			rootnodes.IpAddress = append(rootnodes.IpAddress, rootNodes[i])
		}
	}

	err = rootnodes.WriteIpTable()
	return
}

// CreateIPTableFolderStructure Create folder structure for IPTable
func CreateIPTableFolderStructure() (err error) {
	path, err := config.GetCurrentPath()
	if err != nil {
		return err
	}
	if _, err = os.Stat(path + "p2p"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"p2p", os.ModePerm); err != nil {
			return err
		}
	}
	if _, err = os.Stat(path + "p2p/iptable"); os.IsNotExist(err) {
		if err = os.Mkdir(path+"p2p/iptable", os.ModePerm); err != nil {
			return err
		}
	}
	if _, err = os.Stat(path + "p2p/iptable/ip_table.json"); os.IsNotExist(err) {
		_, err = os.Create(path + "p2p/iptable/ip_table.json")
		if err != nil {
			return err
		}
	}

	return
}

// GenerateDockerFiles Generate default docker files
func GenerateDockerFiles() (err error) {
	path, err := config.GetCurrentPath()
	if err != nil {
		return err
	}
	// if _, err = os.Stat(path + "server"); os.IsNotExist(err) {
	// 	if err = os.Mkdir(path+"server", os.ModePerm); err != nil {
	// 		return err
	// 	}
	// 	if _, err = os.Stat(path + "server/docker"); os.IsNotExist(err) {
	// 		if err = os.Mkdir(path+"server/docker", os.ModePerm); err != nil {
	// 			return err
	// 		}
	// 	}
	// 	if _, err = os.Stat(path + "server/docker/containers"); os.IsNotExist(err) {
	// 		if err = os.Mkdir(path+"server/docker/containers", os.ModePerm); err != nil {
	// 			return err
	// 		}
	// 	}
	// 	if _, err = os.Stat(path + "server/docker/containers"); os.IsNotExist(err) {
	// 		if err = os.Mkdir(path+"server/docker/containers", os.ModePerm); err != nil {
	// 			return err
	// 		}
	// 	}
	// 	if _, err = os.Stat(path + "server/docker/containers/mrp2p-docker"); os.IsNotExist(err) {
	// 		if err = os.Mkdir(path + "server/docker/containers/mrp2p-docker", os.ModePerm); err != nil {
	// 			return err
	// 		}
	// 	}

	// }

	// Clone base docker image
	_, err = git.PlainClone(path+"server/docker/containers/docker-ubuntu-sshd", false, &git.CloneOptions{
		URL:      "https://github.com/Akilan1999/docker-ubuntu-sshd",
		Progress: os.Stdout,
	})
	if err != nil {
		return err
	}

	_, err = git.PlainClone(path+"server/docker/containers/mrp2p-docker", false, &git.CloneOptions{
		URL:      "https://github.com/MFMemon/mrp2p-docker",
		Progress: os.Stdout,
	})

	return
}

// GeneratePluginDirectory Generates plugin directory structure
func GeneratePluginDirectory() (err error) {
	path, err := config.GetCurrentPath()
	if err != nil {
		return err
	}
	// if _, err = os.Stat(path + "plugin"); os.IsNotExist(err) {
	// 	if err = os.Mkdir(path+"plugin", os.ModePerm); err != nil {
	// 		return err
	// 	}
	// }

	// if _, err = os.Stat(path + "plugin/deploy"); os.IsNotExist(err) {
	// 	if err = os.Mkdir(path+"plugin/deploy", os.ModePerm); err != nil {
	// 		return err
	// 	}
	// }

	// if _, err = os.Stat(path + "plugin/deploy/mretcd"); os.IsNotExist(err) {
	// 	if err = os.Mkdir(path+"plugin/deploy/mretcd", os.ModePerm); err != nil {
	// 		return err
	// 	}
	// }

	_, err = git.PlainClone(path+"plugin/deploy/mretcd", false, &git.CloneOptions{
		URL:      "https://github.com/MFMemon/mrp2pmaster.git",
		Progress: os.Stdout,
	})
	if err != nil {
		return err
	}

	return
}

func GenerateClientTrackContainers() (err error) {
	path, err := config.GetCurrentPath()
	if err != nil {
		return
	}

	dirPath := path + "client/trackcontainers/"

	err = os.MkdirAll(dirPath, 0777)

	if err != nil {
		return
	}

	fExists, err := FileExists(dirPath + "trackcontainers.json")
	if err != nil {
		return
	}

	if !fExists {
		_, err = os.Create(dirPath + "trackcontainers.json")
		if err != nil {
			return
		}
	}

	fExists, err = FileExists(dirPath + "grouptrackcontainers.json")
	if err != nil {
		return
	}

	if !fExists {
		_, err = os.Create(dirPath + "grouptrackcontainers.json")
		if err != nil {
			return
		}
	}

	// if _, err = os.Stat(path + "client"); os.IsNotExist(err) {
	// 	if err = os.Mkdir(path+"client", os.ModePerm); err != nil {
	// 		return err
	// 	}

	// 	if err = os.Mkdir(path+"client/trackcontainers", os.ModePerm); err != nil {
	// 		return err
	// 	}

	// 	if _, err = os.Stat(path + "client/trackcontainers/trackcontainers.json"); os.IsNotExist(err) {
	// 		_, err = os.Create(path + "client/trackcontainers/trackcontainers.json")
	// 		if err != nil {
	// 			return err
	// 		}
	// 		_, err = os.Create(path + "client/trackcontainers/grouptrackcontainers.json")
	// 		if err != nil {
	// 			return err
	// 		}
	// 	}

	// }
	return
}

// MakeSSHKeyPair make a pair of public and private keys for SSH access.
// Public key is encoded in the format for inclusion in an OpenSSH authorized_keys file.
// Private Key generated is PEM encoded
// source: https://gist.github.com/goliatone/e9c13e5f046e34cef6e150d06f20a34c
func MakeSSHKeyPair(pubKeyPath, privateKeyPath string) error {
	privateKey, err := rsa.GenerateKey(rand.Reader, 1024)
	if err != nil {
		return err
	}
	// generate and write private key as PEM
	privateKeyFile, err := os.Create(privateKeyPath)
	defer privateKeyFile.Close()
	if err != nil {
		return err
	}

	// Set permission to private key to SSH into docker machine
	err = os.Chmod(privateKeyPath, 600)
	if err != nil {
		return err
	}

	privateKeyPEM := &pem.Block{Type: "RSA PRIVATE KEY", Bytes: x509.MarshalPKCS1PrivateKey(privateKey)}
	if err := pem.Encode(privateKeyFile, privateKeyPEM); err != nil {
		return err
	}
	// generate and write public key
	pub, err := ssh.NewPublicKey(&privateKey.PublicKey)
	if err != nil {
		return err
	}
	return ioutil.WriteFile(pubKeyPath, ssh.MarshalAuthorizedKey(pub), 0655)
}

// FileExists exists returns whether the given file or directory exists
// source: https://stackoverflow.com/questions/10510691/how-to-check-whether-a-file-or-directory-exists
func FileExists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) {
		return false, nil
	}
	return false, err
}

// AuthorizedKey struct represents the structure of an authorized key
type AuthorizedKey struct {
	Username string
	Key      string
}

// AddPublicKeyBareMetal Adds the parameter public key to the auth list
// Generated by ChatGPT
// Prompts:
//   - Generate a go program to read authorised key and map it to a struct and write it back to the same location.
//   - Can you follow the original format
//   (TURNED OUT TO BE PRETTY SHITTY GENERATED CODE)
//func AddPublicKeyBareMetal(PublicKey string) error {
//    // Specify the path to the authorized_keys file
//    dirname, err := os.UserHomeDir()
//    if err != nil {
//        return err
//    }
//
//    authorizedKeysPath := dirname + "/.ssh/" + "authorized_keys"
//
//    // Read the contents of the authorized_keys file
//    file, err := os.Open(authorizedKeysPath)
//    if err != nil {
//        return err
//    }
//    defer file.Close()
//
//    // Create a slice to store AuthorizedKey structs
//    var authorizedKeys []AuthorizedKey
//
//    // Read each line of the file and parse the data
//    scanner := bufio.NewScanner(file)
//    for scanner.Scan() {
//        line := scanner.Text()
//        parts := strings.Fields(line)
//        if len(parts) >= 2 {
//            authorizedKey := AuthorizedKey{
//                Username: parts[0],
//                Key:      parts[1],
//            }
//            authorizedKeys = append(authorizedKeys, authorizedKey)
//        }
//    }
//
//    if err := scanner.Err(); err != nil {
//        return err
//    }
//
//    // Modify the slice of structs if needed
//    // authorizedKeys[0].Username = "newUsername"
//    // authorizedKeys[0].Key = "newKey"
//    authorizedKeys = append(authorizedKeys, AuthorizedKey{Username: "", Key: PublicKey})
//
//    // Write the modified data back to the authorized_keys file
//    newFile, err := os.Create(authorizedKeysPath)
//    if err != nil {
//        return err
//    }
//    defer newFile.Close()
//
//    return nil
//}
