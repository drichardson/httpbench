package main

import (
	"fmt"
	"net/http"
	"strings"
	"time"
)

var buf string

func handler(w http.ResponseWriter, r *http.Request) {
	//fmt.Println("Handling")
	//time.Sleep(5 * time.Second)
	w.Header().Set("ContentType", "text/plain")
	w.Header().Set("ContentLength", "1024")
	w.Write([]byte(buf))
	//fmt.Println("Done Handling")
}

func main() {
	buf = strings.Repeat("x", 1024)
	http.HandleFunc("/", handler)
	http.ListenAndServe(":8080", nil)
}
