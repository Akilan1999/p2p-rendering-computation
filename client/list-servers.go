package main
/*
List-servers.go

TODO:
This file lists all server nodes avaliable and with the appropriate information 

*/

import (
	"fmt"
	"github.com/christophwitzko/go-curl"
	"net/http"
	"encoding/json"
)

func list_servers() (error, []byte){

	/*
      Does a POST request to the API: http://127.0.0.1:5001/api/v0/swarm/peers
	*/

	cb := func(st curl.IoCopyStat) error {
		fmt.Println(st.Stat)
		if st.Response != nil {
			fmt.Println(st.Response.Status)
		}
		return nil
	}
	err, str, resp := curl.String(
		// Refer: https://github.com/christophwitzko/go-curl/blob/master/example_test.go#L3
		"http://127.0.0.1:5001/api/v0/swarm/peers", cb, "method=", "POST",
		"disablecompression=", true,
		"header=", http.Header{"X-My-Header": {"Gopher"}},
	)
	if err != nil {
		/*To convert println to log*/
		fmt.Println(err)
		return err ,nil
	}
    // Print header 
	fmt.Println(resp.Header)
	data, _ := json.Marshal(str)
	return nil ,data
}

func main() {
	// Where your local node is running on localhost:5001
	_ , data := list_servers()
	
}