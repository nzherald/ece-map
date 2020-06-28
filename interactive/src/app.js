import React, { Suspense, useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import styled from "styled-components";
import axios from "axios";

import { ZOOM_FAR } from "./constants";
import { DETAILS } from "./redux/actions"
import details from "./assets/details.json";

const Map = React.lazy(() => import("./components/map"));
const IntroText = React.lazy(() => import("./components/intro-text"));
const Footer = React.lazy(() => import("./components/footer"));
const DetailModal = React.lazy(() => import("./components/detail-modal"))

const App = styled.div`
  background-color: #efeeeb;
  padding-bottom: 12px;
`;

const MapWrap = styled.div`
  position: relative;
`;

const Title = styled.h2`
  background-color: #121617;
  color: white;
  margin: 0;
  padding: 16px 0 12px;
  text-align: center;
`;

export default () => {
  const dispatch = useDispatch()
  const { zoomLevel } = useSelector((state) => state.map);

  useEffect(() => {
    const fetchData = async (url, action) => {
      try {
      const { data }  = await axios(url);
      dispatch({type: action, payload: data});
      } catch {
        console.log("Could not fetch " + url)
      }
    };
    fetchData(details, DETAILS);
  },[]);

  return (
    <App>
      <Title>Find the best early childhood education centre for you</Title>
      <Suspense fallback="loading...">
        <MapWrap>
          <IntroText opacity={zoomLevel === ZOOM_FAR ? 1 : 0} />
          <Map />
          <Footer />
          <DetailModal />
        </MapWrap>
      </Suspense>
    </App>
  );
};
