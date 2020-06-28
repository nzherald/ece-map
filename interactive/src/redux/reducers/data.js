import { DETAILS } from "../actions";

const initialState = { details: {}, schools: [], allSchool: [] };

export default (state = initialState, action) => {
  // console.log(state, action)
  const { payload } = action;
  switch (action.type) {
    case DETAILS:
      return { ...state, details: payload };
    default:
      return state;
  }
};