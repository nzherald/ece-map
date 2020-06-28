import { SELECT, CHANGE_LAYER } from "../actions.js";
import { LAYER_TYPE } from "../../constants.js";

const initialState = { selected: null, layer: LAYER_TYPE };

export default (state = initialState, action) => {
    // console.log(state, action)
  const { payload } = action;
  switch (action.type) {
    case SELECT:
      return { ...state, selected: payload };
    case CHANGE_LAYER:
      return { ...state, layer: payload };
    default:
      return state;
  }
};
