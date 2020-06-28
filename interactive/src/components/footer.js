import React, { useState, useEffect } from "react";
import styled from "styled-components";
import { useSelector, useDispatch } from "react-redux";

import TypeChart from "./type-chart";
import RatingChart from "./rating-chart";
import { ZOOM_FAR, LAYER_RATING, LAYER_TYPE } from "../constants";
import { CHANGE_LAYER } from "../redux/actions";

const Footer = styled.div`
  min-height: 200px;
`;

const Instructions = styled.div`
  margin: 20px 6px;
  div {
    margin: 12px 0;
    font-size: 18px;
  }
`;

const ChartWrap = styled.div`
  position: relative;
`;

const IntroRating = styled.div`
  padding: 0 6px;
  line-height: 1.15;
  @media (min-width: 500px) {
    margin: 0 auto;
    position: absolute;
    right: 10px;
    bottom: 16px;
    max-width: 45%;
    z-index: 1;
    pointer-events: none;
    padding: 10px;
    background-color: white;
    box-shadow: 1px 1px 2px 0px rgba(0, 0, 0, 0.75);
  }
`;

const Intro = styled.div`
  padding: 0 6px;
  line-height: 1.15;
  @media (min-width: 500px) {
    margin: 0 auto;
    position: absolute;
    right: 10px;
    top: 0;
    max-width: 40%;
    z-index: 1;
    pointer-events: none;
    padding: 10px;
    background-color: white;
    box-shadow: 1px 1px 2px 0px rgba(0, 0, 0, 0.75);
  }
`;

const Switch = styled.div`
margin-top: 8px;
  text-decoration: underline;
  pointer-events: all;
  cursor: pointer;
`;

export default () => {
  const dispatch = useDispatch();
  const { zoomLevel } = useSelector((state) => state.map);
  const { layer } = useSelector(({ state }) => state);

  return (
    <Footer>
      {zoomLevel === ZOOM_FAR ? (
        <Instructions>
          <div>
            Circles with numbers in them indicate how many early childhood
            education centres there are in an area.
          </div>
          <div>
            Zoom in or click on a numbered circle to see the individual ECE
            centres.
          </div>
          <div>Or click on a labelled location to go straight there.</div>
        </Instructions>
      ) : layer === LAYER_TYPE ? (
        <ChartWrap>
          <Intro>
            <div>
              Zoom in to see individual ECE centres. Centres are represented by
              circles coloured by their type and sized by their roll. Click on
              these to see more detail.
            </div>
            <div>
              <Switch
                onClick={() =>
                  dispatch({ type: CHANGE_LAYER, payload: LAYER_RATING })
                }
              >
                Click here to colour the map by ERO ratings.
              </Switch>
            </div>
          </Intro>
          <TypeChart />
        </ChartWrap>
      ) : (
        <ChartWrap>
          <IntroRating>
            <div>
              Centres are coloured by their ERO rating and sized by their roll. Click on
              these to see more detail.
            </div>
            <div>
              <Switch
                onClick={() =>
                  dispatch({ type: CHANGE_LAYER, payload: LAYER_TYPE })
                }
              >
                Click to colour the map by centre type.
              </Switch>
            </div>
          </IntroRating>
          <RatingChart />
        </ChartWrap>
      )}
    </Footer>
  );
};
