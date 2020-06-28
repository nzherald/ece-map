import { combineReducers } from "redux";
import data from "./data"
import state from "./state"
import map from "./map"

export default combineReducers({ state, map, data });
