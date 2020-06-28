import { SELECT } from "../actions.js"

const initialState = { selected: null }

export default (state = initialState, action) => {
//   console.log(state, action)
  const { payload } = action;
  switch (action.type) {
    case SELECT:
      return { ...state, selected: payload };
    default:
      return state;
  }
};