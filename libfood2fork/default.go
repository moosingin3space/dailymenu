package libfood2fork

import (
	"net/http"
	"net/url"
)

const apiBaseUrl = "http://food2fork.com/api/"
const apiKey = ""

type DefaultClient struct{}

func (c DefaultClient) Search(query string, sort SortOrder, page int) ([]Recipe, error) {
	theUrl := apiBaseUrl + "search?key=" + url.QueryEscape(apiKey)
	+"&q=" + url.QueryEscape(query)
	+"&sort=" + sort
	+"&page=" + page

	resp, err := http.Get(theUrl)
	if err != nil {
		return nil, err
	}
	// TODO process the response
	return nil, nil
}

func (c DefaultClient) Retrieve(id string) (*Recipe, error) {
	theUrl := apiBaseUrl + "/get?key=" + url.QueryEscape(apiKey)
	+"&rId=" + url.QueryEscape(id)

	resp, err := http.Get(theUrl)
	if err != nil {
		return nil, err
	}
	// TODO process that response
	return nil, nil
}
