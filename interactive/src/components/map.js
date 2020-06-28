import React, { useState, useEffect } from "react";
import ReactMapGL, { Source, Layer, FlyToInterpolator } from "react-map-gl";
import { useDispatch, useSelector } from "react-redux";
import styled from "styled-components";
import "mapbox-gl/dist/mapbox-gl.css";

import { ZOOMLEVEL, SELECT } from "../redux/actions";
import { ZOOM_FAR, ZOOM_MIDDLE, ZOOM_NEAR } from "../constants";
import Nav from "./nav-menu";

import {
  clusterLayer,
  clusterCountLayer,
  unclusteredPointLayer,
} from "./layers";
import ece from "../assets/ece.geojson";

const MAPBOX_TOKEN = process.env.MAPBOX_TOKEN;

const initialviewport = {
  latitude: -41.1088,
  longitude: 171.748,
  zoom: 4.08,
  bearing: 0,
  pitch: 0,
};

const Automate = styled.div`
  margin-top: 50px;
`;

const Buttons = styled.div`
  display: flex;
  justify-content: space-between;
  flex-direction: column;
  position: absolute;
  left: -120px;
  top: 60px;
  width: 100px;
`;

const Pos = styled.div`
  display: flex;
  justify-content: space-between;
`;

export default () => {
  const { layer, selected } = useSelector(({ state }) => state);
  const dispatch = useDispatch();
  const [viewport, setViewport] = useState(initialviewport);
  const [zoomlevel, setZoomlevel] = useState(ZOOM_FAR);
  const [hover, setHover] = useState(-1);
  const flyToViewport = (longitude, latitude, zoom) =>
    setViewport({
      ...viewport,
      latitude,
      longitude,
      zoom,
      transitionInterpolator: new FlyToInterpolator({ speed: 1.2 }),
      transitionDuration: "auto",
    });

  useEffect(() => {
    // Mirror state here because I don't quite get it
    // Or I'm smart and I don't want to run viewport through store
    if (zoomlevel === ZOOM_FAR && viewport.zoom > 4.2) {
      dispatch({ type: ZOOMLEVEL, payload: ZOOM_MIDDLE });
      setZoomlevel(ZOOM_MIDDLE);
    } else if (zoomlevel === ZOOM_MIDDLE && viewport.zoom <= 4.2) {
      dispatch({ type: ZOOMLEVEL, payload: ZOOM_FAR });
      setZoomlevel(ZOOM_FAR);
    } else if (zoomlevel === ZOOM_MIDDLE && viewport.zoom > 12) {
      dispatch({ type: ZOOMLEVEL, payload: ZOOM_NEAR });
      setZoomlevel(ZOOM_NEAR);
    } else if (zoomlevel === ZOOM_NEAR && viewport.zoom <= 12) {
      dispatch({ type: ZOOMLEVEL, payload: ZOOM_MIDDLE });
      setZoomlevel(ZOOM_MIDDLE);
    }
  }, [viewport.zoom, zoomlevel]);

  const clickHandler = (event) => {
    const { lngLat, features } = event;
    const [longitude, latitude] = lngLat;

    const sel = features.filter(
      ({ layer }) => layer.id === "unclustered-point"
    );
    if (sel.length > 0) {
      dispatch({ type: SELECT, payload: sel[0].properties.number });
    } else if (
      features.filter(({ layer }) => layer.id === "clusters").length > 0
    ) {
      setViewport({
        ...viewport,
        longitude,
        latitude,
        zoom: viewport.zoom + 2,
        transitionInterpolator: new FlyToInterpolator({ speed: 1.2 }),
        transitionDuration: "auto",
      });
    }
  };

  const hoverHandler = (event) => {
    const { features } = event;
    if (typeof features == "undefined") {
      return;
    }
    const sel = features.filter(
      ({ layer }) => layer.id === "unclustered-point"
    );
    if (sel.length > 0) {
      setHover(sel[0].properties.number);
    } else if (hover !== -1) {
      setHover(-1);
    }
  };

  const getCursor = ({ isDragging }) => {
    if (isDragging) {
      return "grabbing";
    }
    return hover !== -1 ? "pointer" : "grab";
  };

  return (
    <>
      <ReactMapGL
        {...viewport}
        width="100%"
        height="380px"
        onViewportChange={setViewport}
        onClick={clickHandler}
        onHover={hoverHandler}
        getCursor={getCursor}
        mapStyle="mapbox://styles/nzherald/ckbvt9no60gvk1ipone676rf6"
        mapboxApiAccessToken={MAPBOX_TOKEN}
      >
        <Source
          type="geojson"
          data={ece}
          cluster={true}
          clusterMaxZoom={11}
          clusterRadius={50}
        >
          <Layer {...clusterLayer} />
          <Layer {...clusterCountLayer} />
          <Layer {...unclusteredPointLayer(layer, +selected || -1, hover)} />
        </Source>
      </ReactMapGL>
      <Nav go={flyToViewport} />
      <Automate>
        
        <Buttons>
          <button
            onClick={() =>
              setViewport({
                ...viewport,
                latitude: -41.291836,
                longitude: 174.776386,
                zoom: 13.631,
                transitionInterpolator: new FlyToInterpolator({ speed: 1.2 }),
                transitionDuration: "auto",
              })
            }
          >
            Fly to Wellington
          </button>
          <button onClick={() => dispatch({ type: SELECT, payload: 60346 })}>
            Capital City Preschool
          </button>
          <button
            onClick={() => {
              dispatch({ type: SELECT, payload: null });
              setViewport({
                ...viewport,
                latitude: -36.872575,
                longitude: 174.63529,
                zoom: 12.52538,
                transitionInterpolator: new FlyToInterpolator({ speed: 1.2 }),
                transitionDuration: "auto",
              });
            }}
          >
            Fly to Auckland
          </button>
          <button onClick={() => dispatch({ type: SELECT, payload: 5054 })}>
            Henderson Kindergarten
          </button>
          <button
            onClick={() => {
              dispatch({ type: SELECT, payload: null });
              setViewport({
                ...viewport,
                latitude: -45.24561,
                longitude: 169.39844,
                zoom: 13.325520,
                transitionInterpolator: new FlyToInterpolator({ speed: 1.2 }),
                transitionDuration: "auto",
              });
            }}
          >
            Fly to Alex
          </button>
          <button onClick={() => dispatch({ type: SELECT, payload: 45256 })}>
            BestStart Alexandra
          </button>
        </Buttons>
      </Automate>
    </>
  );
};

/*
<Pos>
          <div>Latitude: {viewport.latitude}</div>
          <div>Longitude: {viewport.longitude}</div>
          <div>Zoom: {viewport.zoom}</div>
        </Pos>
        */