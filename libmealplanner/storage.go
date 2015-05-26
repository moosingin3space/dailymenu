package libmealplanner

type BackingStore interface {
	SaveRecipe(r Recipe) error
	RetrieveRecipe(name string) (Recipe, error)
}
