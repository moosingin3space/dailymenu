package libfood2fork

import (
	"net/url"
)

type Recipe struct {
	Id        string
	Title     string
	Publisher string
	Url       url.URL
	Rank      int
}

type SortOrder string

const (
	Rating       SortOrder = "r"
	Trendingness           = "t"
)

type Client interface {
	Search(query string, sort SortOrder, page int) ([]Recipe, error)
	Retrieve(id string) (Recipe, error)
}
