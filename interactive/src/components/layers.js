import { LAYER_RATING } from "../constants";

export const clusterLayer = {
  id: 'clusters',
  type: 'circle',
  source: 'earthquakes',
  filter: ['has', 'point_count'],
  paint: {
    // 'circle-color': ['step', ['get', 'point_count'], '#51bbd6', 100, '#f1f075', 750, '#f28cb1'],
    'circle-color': '#ecac0c',
    'circle-radius': ['step', ['get', 'point_count'], 20, 100, 30, 750, 40]
  }
};

export const clusterCountLayer = {
  id: 'cluster-count',
  type: 'symbol',
  source: 'earthquakes',
  filter: ['has', 'point_count'],
  layout: {
    'text-field': '{point_count_abbreviated}',
    'text-font': ['DIN Offc Pro Medium', 'Arial Unicode MS Bold'],
    // 'text-font': ['Stag Sans Medium'],
    'text-size': 14
  },
  paint: {
    'text-color': '#121617'
  }
};

export const unclusteredPointLayer = (layer, selected, hover) => {
  return {
  id: 'unclustered-point',
  type: 'circle',
  source: 'earthquakes',
  filter: ['!', ['has', 'point_count']],
  paint: {
    'circle-color': layer === LAYER_RATING ?
    // ,'#bae4bc','#7bccc4','#43a2ca','#0868ac']
     ['step', ['get', 'ero'], '#eceece', 1, '#0868ac',2,'#43a2ca',3,'#7bccc4',4,'#bae4bc',5,'#8856a7',6,'#fdcc8a'] :
     ['step', ['get', 'type'], '#eceece', 1, '#e41a1c',2,'#377eb8',3,'#4daf4a',4,'#984ea3',5,'#ff7f00',6,'#f781bf'],
    'circle-radius': ['interpolate', ['linear'], ['get', 'total_roll'], 1, 8, 100, 18, 200, Math.sqrt(200) + 8],
    'circle-stroke-width': 2,
    'circle-stroke-color': ['case', 
    ['==', ['get', 'number'], selected], '#121617', 
    ['==', ['get', 'number'], hover], '#808285', 
    '#fff']
  }
}
};
