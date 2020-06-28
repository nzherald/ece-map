import React from "react";
import styled from "styled-components"
import {
  XYPlot,
  XAxis,
  YAxis,
  LabelSeries,
  HorizontalBarSeries,
} from "react-vis";

import typeData from "../types.json"

const Chart = styled.div`
padding: 0 6px;
.rv-xy-plot__series--label {
    text {
        font-family: 'Stag Sans Book';
        fill: #333c3d;
    }
}
`
const Label = styled.tspan`
font-size: ${props => props.idx ? '13px' : '14px'};
font-family: 'Stag Sans Medium';
fill: #596465;
`

const formatTypes = tick => {
    if (tick.includes("\n")) {
        const tickRows = tick.split("\n")
        return tickRows.map((t,i) => <Label idx={i}key={i} x={i * -5} dy={i*20-5}>{t}</Label>)
    }
    return [<Label key={0} x={0}>{tick}</Label>]
}

const xOffsets = {
    0: -10,
    5: -32
}

const yOffsets = {
    5: -8,
    4: -8,
    3: -8
}

export default () => {
  return (
    <Chart>
        <h3>Types of ECE centres in NZ</h3>
      <XYPlot width={Math.min(600, window.innerWidth - 40)} height={220} yType="ordinal"
      margin={{left: 148, right: 0, top: 0, bottom: 3}}
      colorScale={"category"}
      colorDomain={[1,2,3,4,5,6]}
      colorRange={[ '#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#f781bf' ]}
      >
          <YAxis tickFormat={formatTypes} />
        <HorizontalBarSeries
          data={typeData}
        />
        <LabelSeries 
        data={typeData.map((d,i) => {
            d.label = `${d.x}`
            d.yOffset = yOffsets[i] || 10
            d.xOffset = xOffsets[i] || -30
            return d
            })}
             />
      </XYPlot>
    </Chart>
  );
};
