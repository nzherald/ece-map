import React, { Suspense, useState, useEffect } from "react";
import ReactMapGL, { Source, Layer, FlyToInterpolator } from "react-map-gl";
import { useDispatch } from "react-redux";
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

export default () => {
  const dispatch = useDispatch();
  const [viewport, setViewport] = useState(initialviewport);
  const [zoomlevel, setZoomlevel] = useState(ZOOM_FAR);
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

  return (
    <>
      <ReactMapGL
        {...viewport}
        width="100%"
        height="440px"
        onViewportChange={setViewport}
        onClick={clickHandler}
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
          <Layer {...unclusteredPointLayer} />
        </Source>
      </ReactMapGL>
      <Nav go={flyToViewport} />
    </>
  );
};
