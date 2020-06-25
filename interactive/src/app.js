import React, { useState } from 'react';
import MapGL from 'react-map-gl';

const MAPBOX_TOKEN = process.env.MAPBOX_TOKEN; // Set your mapbox token here

const initialViewport = {
      latitude: -36.845,
      longitude: 174.735,
      zoom: 9.89,
      bearing: 0,
      pitch: 0
    }

const App = props => {
  const [viewport, setViewport] = useState(initialViewport)
  console.log(viewport)
  return (
    <MapGL
        {...viewport}
        width="100%"
        height="320px"
        onViewportChange={setViewport}
        mapboxApiAccessToken={MAPBOX_TOKEN}
      >
      </MapGL>
  )
}

export default App
