
import React from "react";
import styled from "styled-components"
import {
  XYPlot,
  XAxis,
  YAxis,
  LabelSeries,
  HorizontalBarSeries,
} from "react-vis";

import ratingData from "../rating.json"

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
    0: 5,
    4: -22,
    1: -30
}

const yOffsets = {
    5: -8,
    4: -8,
    3: -8
}

export default () => {
  return (
    <Chart>
        <h3>ERO Ratings of NZ ECE centres</h3>
      <XYPlot width={Math.min(600, window.innerWidth - 40)} height={220} yType="ordinal"
      margin={{left: 198, right: 0, top: 0, bottom: 13}}
      colorScale={"category"}
      colorDomain={[1,2,3,4,5,6]}
      colorRange={[ '#0868ac','#43a2ca','#7bccc4','#bae4bc','#8856a7','#fdcc8a']}
      >
          <YAxis tickFormat={formatTypes} />
        <HorizontalBarSeries
          data={ratingData}
        />
        <LabelSeries 
        data={ratingData.map((d,i) => {
            d.label = `${d.x}`
            d.yOffset = yOffsets[i] || 10
            d.xOffset = xOffsets[i] || 5
            return d
            })}
             />
      </XYPlot>
    </Chart>
  );
};