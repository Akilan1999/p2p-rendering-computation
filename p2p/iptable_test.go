package p2p
// import (
// 	"fmt"
// 	"testing"
// )
//
// func TestReadIpTable(t *testing.T) {
// 	json, err := ReadIpTable()
// 	if err != nil {
// 		t.Fatal(err)
// 	}
//
// 	err = json.WriteIpTable()
// 	if err != nil {
// 		t.Fatal(err)
// 	}
//
// 	err = PrintIpTable()
// 	if err != nil {
// 		t.Fatal(err)
// 	}
// }
//
// // Testing is a IPV6 address is returned
// func TestGetCurrentIPV6(t *testing.T) {
// 	res, err := GetCurrentIPV6()
//
// 	if err != nil {
// 		t.Error(err)
// 	}
//
// 	fmt.Println(res)
// }
//
// // This test ensures that the duplicate function works as intended
// func TestIpAddresses_RemoveDuplicates(t *testing.T) {
// 	var testduplicates IpAddresses
// 	var duplicateaddress1 IpAddress
// 	var duplicateaddress2 IpAddress
//
// 	duplicateaddress1.Ipv6="2001:8f8:172d:ee93:7588:ad57:c351:3309"
// 	duplicateaddress1.Ipv4="0.0.0.0"
//
// 	duplicateaddress2.Ipv6="2001:8f8:172d:ee93:7588:ad57:c351:3309"
// 	duplicateaddress2.Ipv4="0.0.0.0"
//
// 	testduplicates.IpAddress = append(testduplicates.IpAddress, duplicateaddress1)
// 	testduplicates.IpAddress = append(testduplicates.IpAddress, duplicateaddress2)
//
// 	err := testduplicates.RemoveDuplicates()
// 	if err != nil {
// 		t.Error(err)
// 	}
//
// 	if len(testduplicates.IpAddress) == 2 {
// 		t.Fail()
// 	}
//
// }
//
// func TestViewNetworkInterface(t *testing.T) {
// 	err := ViewNetworkInterface()
// 	if err != nil {
// 		t.Error()
// 	}
// }
//
// func TestIp4or6(t *testing.T) {
// 	// This test ensures that the ipv6 address gets detected
// 	test := "2001:8f8:172d:7e27:4f23:ae4:bce5:e037"
// 	res := Ip4or6(test)
// 	if res != "version 6" {
// 		t.Fail()
// 	}
//
// }
