import React from "react";
import { render } from "react-dom";

import { Provider } from "react-redux";

import Base from "./lib/base.js";
import { nzhconsole, setupScrolly, appWarn } from "./lib/util.js";
import "./root.less";

import store from "./redux/store";
import App from "./app";

class Main extends Base {
  constructor(selector, params) {
    nzhconsole("Setting up visualisation with parameters:", params);
    super(selector);
    nzhconsole("Loading data...");
    null;
    this.premiumWait(() => {
      nzhconsole("Rendering...");
      render(
          <Provider store={store}>
            <App />
          </Provider>,
        document.querySelector(selector)
      );
      nzhconsole("Done.");
      this.fadeOut();
      // if (params.setupScrolly) {
      //     const scrolly = setupScrolly(params.setupScrolly)
      // }
      if (params.appWarn) {
        appWarn(this.root.selector, params.appWarn, params.category);
      }
    });
  }
}

window.Main = window.UniqClassName = Main;
