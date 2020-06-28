import React, { useState, useEffect } from "react";
import styled from "styled-components";
import { useSelector } from "react-redux";

import TypeChart from "./type-chart";
import { ZOOM_FAR } from "../constants";

const Footer = styled.div`
min-height: 260px;
`;

const Instructions = styled.div`
  margin: 0px 6px;
`;

const ChartWrap = styled.div`
  position: relative;
`;

const Intro = styled.div`
  max-width: 80%;
  margin: 0 auto;
  @media (min-width: 500px) {
    position: absolute;
    right: 10px;
    top: 0;
    max-width: 40%;
    z-index: 1;
  }
  pointer-events: none;
  padding: 0 10px;
  background-color: white;
  box-shadow: 1px 1px 2px 0px rgba(0, 0, 0, 0.75);
`;

export default () => {
  const { zoomLevel } = useSelector((state) => state.map);

  return (
    <Footer>
      {zoomLevel === ZOOM_FAR ? (
        <Instructions>
          <p>
            Circles with numbers in them indicate how many early childhood
            education centres there are in an area.
          </p>
          <p>
            Zoom in or click on a numbered circle to see the individual ECE
            centres.
          </p>
          <p>Or click on a labelled location to go straight there.</p>
        </Instructions>
      ) : (
        <ChartWrap>
          <TypeChart />
          <Intro>
            <p>
              Zoom in to see individual ECE centres. Centres are represented by
              circles coloured by their type and sized by their roll.
            </p>
            <p>Click on a centre to see more detail.</p>
          </Intro>
        </ChartWrap>
      )}
    </Footer>
  );
};
