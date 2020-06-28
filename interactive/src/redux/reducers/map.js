import { ZOOMLEVEL } from "../actions";
import { ZOOM_FAR } from "../../constants";

const initialState = { zoomLevel: ZOOM_FAR };

export default (state = initialState, action) => {
  // console.log(state, action)
  const { payload } = action;
  switch (action.type) {
    case ZOOMLEVEL:
      return { ...state, zoomLevel: payload };
    default:
      return state;
  }
};
