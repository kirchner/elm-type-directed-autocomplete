import { Elm } from './src/Main.elm';

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}

const cacheKey = "elm-type-directed-autocomplete"

var app = Elm.Main.init({
  flags: {
    localValues:  localStorage.getItem(cacheKey) || "",
    coreJson: require("./docs/core.json")
  },
  node: document.getElementById("elm-container")
});

app.ports.cache.subscribe(function(data) {
  localStorage.setItem(
    cacheKey,
    JSON.stringify(data)
  );
});
