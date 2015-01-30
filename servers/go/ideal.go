package main

import (
	"net/http"
	"strings"
)

var buf string

func handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("ContentType", "text/plain")
	w.Header().Set("ContentLength", "1024")
	w.Write([]byte(buf))
}

func main() {
	buf = strings.Repeat("x", 1024)
	http.HandleFunc("/", handler)
	http.ListenAndServe(":8080", nil)
}
