import React from "react";
import { useDispatch, useSelector } from "react-redux";
import styled from "styled-components";
import { SELECT } from "../redux/actions";


const Details = styled.div`
  position: absolute;
  top: 441px;
  z-index: 2;
  left: 0;
  right: 0;
  bottom: -14px;
  background-color: white;
  box-shadow: 1px 1px 2px 0px rgba(0, 0, 0, 0.75);
  transition: opacity 100ms ease-in-out;
  pointer-events: ${(props) => (props.opacity ? "all" : "none")};
  opacity: ${(props) => props.opacity};
`;

const colors = [
  "#e41a1c",
  "#377eb8",
  "#4daf4a",
  "#984ea3",
  "#ff7f00",
  "#f781bf",
];
const Heading = styled.div`
position: relative;
  background-color: ${(props) => colors[props.type - 1]};
  color: white;
  margin: 0;
  padding: 6px 0 12px;
  text-align: center;
  font-family: "Stag Book";
  h2 {
    margin: 2px 0 8px;
  }
`;

const Type = styled.div`
  font-size: 20px;
  margin-bottom: 6px;
`;

const Authority = styled.div``;

const Body = styled.div`
  display: flex;
  justify-content: space-around;
`;

const Rating = styled.div`
  a {
    text-decoration: underline;
    pointer-events: all;
    cursor: pointer;
  }
`;

const Summary = styled.div`
`;

const Rolls = styled.div`
  flex: 1 0 100px;
  display: flex;
  justify-content: space-around;
  align-items: center;
  flex-direction: column;
  flex-wrap: wrap;
`;

const TotalRoll = styled.div`
background-color: ${props => colors[props.type-1]};
border-radius: 8px;
margin: 10px;
padding: 12px 18px;
min-width: 100px;
display: flex;
justify-content: center;
align-items: center;
flex-direction: column;
text-align: center;
color: white;
box-sizing: border-box;
`

const Under2Roll = styled.div`
border: 3px solid ${props => colors[props.type-1]};
border-radius: 8px;
margin: 10px;
padding: 12px 18px;
min-width: 100px;
display: flex;
justify-content: center;
align-items: center;
flex-direction: column;
text-align: center;
box-sizing: border-box;
color: ${props => colors[props.type-1]};
`

const RollNum = styled.div`
font-family: 'Stag Sans Medium';
font-size: 40px;
`

const Address = styled.div`
font-family: 'Stag Book';
font-size: 18px;
`

const Wrap = styled.div`
flex: 0 1 400px;
display: flex;
flex-direction: column;
justify-content: space-between;
margin-top: 20px;
`

const Close = styled.svg`
position: absolute;
pointer-events: all;
cursor: pointer;
&:hover {
  opacity: 0.3;
}
top: 5px;
right: 5px;
`

export default () => {
  const dispatch = useDispatch()
  const { selected } = useSelector(({ state }) => state);
  const {
    name,
    type,
    authority,
    type_idx,
    erodate,
    erolink,
    rating,
    total_roll,
    under_2s,
    street,
    suburb,
    town_city
  } = useSelector(({ data }) => (data.details && data.details[selected]) || {});
  return (
    <Details opacity={selected ? 1 : 0}>
      <Heading type={type_idx}>
        <h2>{name}</h2>
        <Type>{type}</Type>
        <Authority>{authority}</Authority>
        <Close height="32" viewBox="0 0 24 24" width="32" onClick={() => dispatch({type: SELECT, payload: null})}>
          <path d="M0 0h24v24H0V0z" fill="none" opacity=".87"/>
          <path fill="white" d="M12 2C6.47 2 2 6.47 2 12s4.47 10 10 10 10-4.47 10-10S17.53 2 12 2zm5 13.59L15.59 17 12 13.41 8.41 17 7 15.59 10.59 12 7 8.41 8.41 7 12 10.59 15.59 7 17 8.41 13.41 12 17 15.59z"/>
        </Close>
      </Heading>
      <Body>
        <Summary>
          <Rolls>
            <TotalRoll type={type_idx}>
              <RollNum>{total_roll}</RollNum>
              <div>Total Roll</div>
            </TotalRoll>
            <Under2Roll  type={type_idx}>
              <RollNum>{under_2s}</RollNum>
              <div>Under 2s</div>
            </Under2Roll>
          </Rolls>
        </Summary>
        <Wrap>
        <Address>
          {street && <div>{street}</div>}
          {suburb && <div>{suburb}</div>}
          {town_city && <div>{town_city}</div>}
        </Address>
        <Rating>
            {rating !== "no rating" ? <p>
            Last visited by ERO on {erodate} and rated "{rating}".
          </p> : <p>
            Last visited by ERO on {erodate}.
          </p>}
          <a href={`${erolink}`} target="_blank">
            <p>Click here to read the full report.</p>
          </a>
        </Rating>
</Wrap>
      </Body>
    </Details>
  );
};
