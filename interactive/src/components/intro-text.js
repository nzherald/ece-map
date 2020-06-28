import React, { Suspense } from "react";
import styled from 'styled-components'

const Intro = styled.div`
position: absolute;
pointer-events: none;
left: 10px;
top: 15px;
max-width: 40%;
z-index: 1;
padding: 0 10px;
background-color: white;
box-shadow: 1px 1px 2px 0px rgba(0,0,0,0.75);
transition: opacity 700ms ease-in-out;
opacity: ${props => props.opacity};
div {
    margin: 12px 0;
}
`

export default ({opacity}) => {
    return (
        <Intro opacity={opacity}>
            <div>This interactive map shows most ECE centres in NZ</div>
            <div>Zoom in and click to find details of centres in your area, including roll size and ERO reports.</div>
        </Intro>
    );
}