// Complete rewritten by hand no LLM BS here 

package p2p

import (
    "bytes"
    "fmt"
    "golang.org/x/crypto/ssh"
    "os"
    "path/filepath"
)

// GetAuthorizedKeysPath returns the path to the authorized_keys file
func GetAuthorizedKeysPath() (string, error) {
    homeDir, err := os.UserHomeDir()
    if err != nil {
        return "", fmt.Errorf("could not find home directory: %v", err)
    }
    return filepath.Join(homeDir, ".ssh", "authorized_keys"), nil
}

func RemoveAllKeysFromAuthorizedList() error {
    table, err := ReadIpTable()
    if err != nil {
        return err
    }

    for i, _ := range table.IpAddress {
        RemoveAuthorisationKey(table.IpAddress[i].PublicKey)
    }

    return nil
}

// RemoveAuthorisationKey Removes authorisation from the list
func RemoveAuthorisationKey(PublicKey string) error {
    path, err := GetAuthorizedKeysPath()
    if err != nil {
        return err
    }

    // Display existing keys
    keys, err := ReadAuthFile(path)

    // Convert Public key string the right type
    parsedPub, _, _, _, err := ssh.ParseAuthorizedKey([]byte(PublicKey))
    if err != nil {
        return err
    }

    for i, _ := range keys {
        if bytes.Equal(keys[i].Marshal(), parsedPub.Marshal()) {
            keys = remove(keys, i)
            break
        }
    }

    // write to file
    err = WriteToAuthFile(keys)
    if err != nil {
        return err
    }

    return nil
}

func remove(slice []ssh.PublicKey, s int) []ssh.PublicKey {
    return append(slice[:s], slice[s+1:]...)
}

// AddAuthorisationKey Adds public key provided to the
// authorization file so that nodes can SSH into
// the
func AddAuthorisationKey(PublicKey string) error {
    path, err := GetAuthorizedKeysPath()
    if err != nil {
        return err
    }

    // Convert Public key string the right type
    parsedPub, _, _, _, err := ssh.ParseAuthorizedKey([]byte(PublicKey))
    if err != nil {
        return err
    }

    // Display existing keys
    keys, err := ReadAuthFile(path)

    exists := false
    for i, _ := range keys {
        if bytes.Equal(keys[i].Marshal(), parsedPub.Marshal()) {
            exists = true
        }
    }

    if exists {
        return nil
    }

    keys = append(keys, parsedPub)

    // write to file
    err = WriteToAuthFile(keys)
    if err != nil {
        return err
    }

    return nil
}

func ReadAuthFile(File string) ([]ssh.PublicKey, error) {
    authorizedKeysBytes, err := os.ReadFile(File)
    if err != nil {
        return nil, fmt.Errorf("failed to read authorized keys: %s", err)
    }

    var authorizedKeys []ssh.PublicKey

    for {
        key, _, _, rest, err := ssh.ParseAuthorizedKey(authorizedKeysBytes)
        if err != nil {
            // there's no good error to check for here
            break
        }
        authorizedKeys = append(authorizedKeys, key)
        authorizedKeysBytes = rest
    }

    return authorizedKeys, nil
}

func WriteToAuthFile(keys []ssh.PublicKey) error {
    path, err := GetAuthorizedKeysPath()
    if err != nil {
        return err
    }

    var output []byte

    for _, key := range keys {
        output = append(output, ssh.MarshalAuthorizedKey(key)...)
    }

    err = os.WriteFile(path, output, 0666)
    if err != nil {
        return err
    }
    return nil
}
