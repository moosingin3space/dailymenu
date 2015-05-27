package week

import (
	planner "github.com/moosingin3space/dailymenu/libmealplanner"
)

type planWeek struct{}

func (w planWeek) CompatibleSizes(r1 planner.Recipe, r2 planner.Recipe) bool {
	// TODO
	return false
}

func (w planWeek) CompatibleStyles(r1 planner.Recipe, r2 planner.Recipe) bool {
	// TODO
	return false
}

func New() planner.PlanPeriod {
	return planWeek{}
}
