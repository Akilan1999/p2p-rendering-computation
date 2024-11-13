// NOTE: Most of the code snippet was generated using ChatGPT
// Prompt used: "generate go program to read and populate ssh authorization file"

package p2p

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// GetAuthorizedKeysPath returns the path to the authorized_keys file
func GetAuthorizedKeysPath() (string, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("could not find home directory: %v", err)
	}
	return filepath.Join(homeDir, ".ssh", "authorized_keys"), nil
}

// ReadAuthorizedKeys reads and returns the current contents of the authorized_keys file as a map
func ReadAuthorizedKeys(path string) (map[string]bool, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, fmt.Errorf("could not open authorized_keys file: %v", err)
	}
	defer file.Close()

	keys := make(map[string]bool)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		// Skip empty lines and comments
		if line != "" && !strings.HasPrefix(line, "#") {
			keys[line] = true
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading authorized_keys file: %v", err)
	}
	return keys, nil
}

// AddKeyToAuthorizedKeys adds a new key to the authorized_keys file if it doesn’t already exist
func AddKeyToAuthorizedKeys(path, newKey string) error {
	keys, err := ReadAuthorizedKeys(path)
	if err != nil {
		return err
	}

	// Check if the key already exists in the map
	if keys[newKey] {
		return errors.New("key already exists in authorized_keys")
	}

	// Append the new key
	file, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		return fmt.Errorf("could not open authorized_keys file for writing: %v", err)
	}
	defer file.Close()

	if _, err := file.WriteString(newKey + "\n"); err != nil {
		return fmt.Errorf("could not write to authorized_keys file: %v", err)
	}
	return nil
}

func RemoveKeyFromAuthorizedKeys(path, keyToRemove string) error {
	keys, err := ReadAuthorizedKeys(path)
	if err != nil {
		return err
	}

	// Check if the key exists in the map
	if !keys[keyToRemove] {
		return errors.New("key not found in authorized_keys")
	}

	// Delete the key from the map
	delete(keys, keyToRemove)

	// Write updated keys back to the authorized_keys file
	file, err := os.OpenFile(path, os.O_TRUNC|os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		return fmt.Errorf("could not open authorized_keys file for writing: %v", err)
	}
	defer file.Close()

	for key := range keys {
		if _, err := file.WriteString(key + "\n"); err != nil {
			return fmt.Errorf("could not write to authorized_keys file: %v", err)
		}
	}

	return nil
}

// AddAuthorisationKey Adds public key provided to the
// authorization file so that nodes can SSH into
// the
func AddAuthorisationKey(PublicKey string) error {
	path, err := GetAuthorizedKeysPath()
	if err != nil {
		return err
	}

	// Display existing keys
	_, err = ReadAuthorizedKeys(path)
	if err != nil {
		return err
	}

	err = AddKeyToAuthorizedKeys(path, PublicKey)
	if err != nil {
		return err
	}

	return nil
}
