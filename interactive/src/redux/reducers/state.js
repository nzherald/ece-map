import { SELECT, CHANGE_LAYER, ZOOMLEVEL } from "../actions.js";
import { LAYER_TYPE, ZOOM_FAR, ZOOM_MIDDLE } from "../../constants.js";

const initialState = { selected: null, layer: LAYER_TYPE, zoomLevel: ZOOM_FAR };

export default (state = initialState, action) => {
  // console.log(state, action)
  const { payload } = action;
  switch (action.type) {
    case ZOOMLEVEL:
      if (payload === ZOOM_FAR && state.selected) {
        return state;
      }
      return { ...state, zoomLevel: payload };
    case SELECT:
      return {
        ...state,
        selected: payload,
        zoomLevel: state.zoomLevel === ZOOM_FAR ? ZOOM_MIDDLE : state.zoomLevel,
      };
    case CHANGE_LAYER:
      return { ...state, layer: payload };
    default:
      return state;
  }
};
