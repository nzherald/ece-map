import React, { Suspense } from "react";
import styled from "styled-components";
import { useDispatch } from "react-redux";
import { FLYTO } from "../redux/actions";

const Nav = styled.div`
  margin: 3px 6px;
  line-height: 1.2;
  font-size: 14px;
  font-family: "Stag Book";
  color: #596465;
`;

const Dest = styled.span`
  cursor: pointer;
  margin-left: 0.25em;
  text-decoration: underline;
  &:hover {
    color: #333c3d;
  }
`;

export default ({go}) => {
  // const dispatch = useDispatch();
  // const go = (longitude, latitude, zoom) => dispatch({type: FLYTO, payload: {latitude, longitude, zoom}})
  return (
    <Nav>
      Go to
      <Dest onClick={() => go(174.263, -35.593, 7.22)}>Northland</Dest>, 
      <Dest onClick={() => go(174.712, -36.807, 9.13)}>Auckland</Dest>, 
      <Dest onClick={() => go(175.228, -37.744, 8.88)}>Waikato</Dest>, 
      <Dest onClick={() => go(176.129, -37.66, 9.19)}>Bay of Plenty</Dest>, 
      <Dest onClick={() => go(176.077, -38.465, 8.31)}>Central</Dest>, 
      <Dest onClick={() => go(174.341, -39.006, 8.85)}>Taranaki</Dest>, 
      <Dest onClick={() => go(177.416, -39.061, 7.5)}>Hawke's Bay</Dest>, 
      <Dest onClick={() => go(175.462, -40.176, 8.91)}>Manawatu-Whanganui </Dest>
      ,<Dest onClick={() => go(174.829, -41.187, 9.8)}>Wellington</Dest>,
      <Dest onClick={() => go(172.699, -43.637, 8.28)}>Christchurch</Dest>,
      <Dest onClick={() => go(170.454, -45.849, 8.35)}>Dunedin</Dest> or
      <Dest onClick={() => go(171.748, -41.1088, 4.08)}>reset</Dest>
    </Nav>
  );
};
