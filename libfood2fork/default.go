package libfood2fork

import (
	"net/http"
	"net/url"
)

const apiBaseUrl = "http://food2fork.com/api/"
const apiKey = ""

type DefaultClient struct{}

func (c DefaultClient) Search(query string, sort SortOrder, page int) ([]Recipe, error) {
	theUrl := apiBaseUrl + "search?key=" + url.QueryEncode(apiKey)
	+"&q=" + url.QueryEncode(query)
	+"&sort=" + url.QueryEncode(sort)
	+"&page=" + url.QueryEncode(page)

	resp, err := http.Get(theUrl)
	if err != nil {
		return nil, err
	}
	// TODO process the response
}

func (c DefaultClient) Retrieve(id string) (Recipe, error) {
	theUrl := apiBaseUrl + "/get?key=" + url.QueryEncode(apiKey)
	+"&rId=" + url.QueryEncode(id)

	resp, err := http.Get(theUrl)
	if err != nil {
		return nil, err
	}
	// TODO process that response
}
