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
`

export default ({opacity}) => {
    return (
        <Intro opacity={opacity}>
            <p>This interactive map shows most ECE centres in NZ</p>
            <p>Zoom in and click to find details of centres in your area, including roll size and ERO reports.</p>
        </Intro>
    );
}